module Compile where

import qualified Turtle as T
import Analyse
import Parser

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Prelude hiding (Ordering (..))

{-|
Callbacks are used when calling a function. The Compiler places a 'Call (-1)' instruction
reserving space to call the function address once it is known. The Compiler then registers
a callback telling it which function address is needed and where to insert it. These
callbacks are then resolved at the end of compilation.
|-}
type Callback = (Identifier, Int)

-- |The Context used in the Analyse module becomes the symbol table for this module
type SymTable = Context
-- |The ContextType used in the Analyse module is used as a symbol table entry for this module
type SymTableType = ContextType

{-
This polymorphism allows the usage of boxed and unboxed vectors of Marshallable data
as the basis for the compiler. This type generality is a powerful way to not tie the
output type (binary data or text assembly) to the compilation process.
-}
-- |CompileType describing the state of the compiler as it compiles the program
data CompileType s v a = CompType {index :: Int, memory :: v s a, symtable :: SymTable, callbacks :: [Callback]}

{-
This is some spooky type kung-fu allowing haskell to use mutable strict arrays
as the basis for writing memory (which cannot be written sequentially). This
removes the O(n) element random access and makes the backend feel more like C.
-}
-- |The ST monad is needed to make array access strict and mutable
type Compiler s v a = StateT (CompileType s v a) (T.ST s)

-- |Distinguish global and local variables
data Scope = Global
           | Local
  deriving (Eq, Show)

{-|
entry point for compiler backend controlling in what order each part of the program
is to be compiled.
|-}
compileProgram :: (T.PMemory v a) => Program -> Compiler s v a ()
compileProgram (Program _ vars (FuncDecls funs) body) = do
  --place inits
  ctx <- gets symtable
  mem <- lift $ T.initMem     --initialise the program memory
  put $ CompType 0 mem ctx [] --put CompileType with initialised memory into state

  compileVarDecs [] vars      --compile the 'inits' section
  mainJmpIx <- gets index     --get index of jump to mainline for back patching
  emit1 (T.Jump (-1))         --jump to main body (will be patched when address is known)

  compileFunctions funs       --compile the functions (not mainline)

  --patch above jump now functions are compiled and body start location known
  bodyIx <- gets index
  (gets memory)  >>= lift . T.writeAtMem (fromIntegral mainJmpIx + 1) (T.marshall bodyIx)
  compileBody [] body

  emit1 T.Halt                --specify halting of mainline
  --go back over the callbacks and insert function locations
  resolveFnCallbacks

{-
These functions insert a value into the CompileType data structure.
This is used in combination with the strict state modify, modify', to update the index.
-}
-- |Place index into CompileType monad
putIx :: (T.PMemory v a) => Int -> CompileType s v a -> CompileType s v a
putIx ix (CompType _ m s c) = CompType ix m s c

-- |Place Symbol Table into CompileType monad
putSymTbl :: (T.PMemory v a) => SymTable -> CompileType s v a -> CompileType s v a
putSymTbl tbl (CompType i m _ c) = CompType i m tbl c

-- |Place Callbacks into CompileType monad
putCallback :: (T.PMemory v a) => Callback -> CompileType s v a -> CompileType s v a
putCallback cb (CompType i m s cbs) = CompType i m s (cb:cbs)

{-|
callbacks are (function name, jump index) pairs, it is the job of this function
to lookup the (function name) and fill in the jump index with the correct function offset
|-}
resolveFnCallbacks :: (T.PMemory v a) => Compiler s v a ()
resolveFnCallbacks = gets callbacks >>= mapM_ resolveCallback
  where
    resolveCallback :: (T.PMemory v a) => Callback -> Compiler s v a ()
    resolveCallback (fun, ix) = do
      symtbl <- gets symtable
      case M.lookup fun symtbl of
        Just (Function off _ _ _) -> gets memory >>= lift . T.writeAtMem ix (T.marshall off)
        _ -> error "Found malformed symbol table when compiling" -- Should never occur

-- |Compile Variable Declations
compileVarDecs :: (T.PMemory v a) => Addr -> VarDeclarations -> Compiler s v a ()
compileVarDecs addr (VarDecls vars) = foldM_ (compileVarDecl addr) 1 vars

-- |Compile Functions (not mainline)
compileFunctions :: (T.PMemory v a) => [FuncDeclaration] -> Compiler s v a ()
compileFunctions funs = mapM_ compileFunction funs
  where
    compileFunction :: (T.PMemory v a) => FuncDeclaration -> Compiler s v a ()
    compileFunction (FuncDeclaration name (Params params) vars body) = do
      --set the function pointer to be the current index
      gets index >>= updateFunOffset name . fromIntegral
      let n = (-1) - (fromIntegral $ length params)
          addr = [name]
      --update location of parameters on stack
      foldM_ (fillParam addr) n params
      compileVarDecs addr vars  --Compile local variables
      compileBody addr body     --Compile function body
      --each function MUST return once finished
      emit1 T.Rts

{-
place parameter location on stack into function context, return new offset.
Use with foldM_ e.g.
foldM_ (fillParam address) startN parameters
-}
fillParam :: (T.PMemory v a) => Addr -> Offset -> Identifier -> Compiler s v a Offset
fillParam addr off param = do
  updateVarOffset addr param off
  return (succ off)

{-
Compile function body, requires parameters to be filled in and initialiser section compiled
-}
compileBody :: (T.PMemory v a) => Addr -> CompoundStatement -> Compiler s v a ()
compileBody addr (CompoundStatement stmts) = compileStatements addr stmts

compileStatements :: (T.PMemory v a) => Addr -> Statements -> Compiler s v a ()
compileStatements addr (Statements stmts) = mapM_ (compileStatement addr) stmts

compileStatement :: (T.PMemory v a) => Addr -> Statement -> Compiler s v a ()
compileStatement addr stmt = do
  case stmt of
    Up -> emit1 T.Up
    Down -> emit1 T.Down
    Moveto e1 e2 -> compileExpr addr e1 >> compileExpr addr e2 >> emit1 T.Move
    Read var -> do
      (off, scope) <- resolveVar addr var      --get the offset of the variable
      case scope of
        Global -> emit1 (T.ReadGP off)
        Local  -> emit1 (T.ReadFP off)
    Assignment var expr -> do
      compileExpr addr expr
      (off, scope) <- resolveVar addr var  --resolveVar looks up a variable within a function scope
      case scope of
        Global -> emit1 $ T.StoreGP  off
        Local  -> emit1 $ T.StoreFP  off
    IfElse cmp b1 b2 -> do
      op <- compileComparison addr cmp --conditionals (Z & N) set
      cmpIx <- gets index >>= return . fromIntegral
      case op of
        Equals -> emit1 (T.Jeq (cmpIx + 4))       --cmpIx + 0 (Jump past next jump)
        LT ->     emit1 (T.Jlt (cmpIx + 4))       --cmpIx + 0 (Jump past next jump)
      emit1 (T.Jump (-1))                         --cmpIx + 2 (jump to else block)
      compileBody addr b1

      emit1 (T.Jump (-1))                         --elseIx - 2 (Jump to end of else if block)
      elseIx <- gets index
      --correct the above jumps (write elseIx to cmpIx+3)
      gets memory >>= lift . T.writeAtMem (fromIntegral cmpIx + 3) (T.marshall elseIx)

      compileBody addr b2
      finalIx <- gets index
      --correct the jump to point to end of if else (write to elseIx - 1)
      gets memory >>= lift . T.writeAtMem (fromIntegral elseIx - 1) (T.marshall finalIx)
    If cmp body -> do
      op <- compileComparison addr cmp    --conditionals (Z & N) set
      cmpIx <- gets index >>= return . fromIntegral
      case op of
        Equals -> do
          emit1 (T.Jeq (cmpIx + 4))       --cmpIx + 0 (Jump past next jump)
        LT -> do
          emit1 (T.Jlt (cmpIx + 4))       --cmpIx + 0 (Jump past next jump)
      emit1 (T.Jump (-1))                 --cmpIx + 2 (jump to end of block)
      compileBody addr body

      endIx <- gets index
      --correct the above jumps to point to endIx
      gets memory >>= lift . T.writeAtMem (fromIntegral cmpIx + 3) (T.marshall endIx)
    While cmp body -> do
      topIx <- gets index
      op <- compileComparison addr cmp
      cmpIx <- gets index >>= return . fromIntegral
      case op of
        Equals -> do
          emit1 (T.Jeq (cmpIx + 4))       --cmpIx + 0 (Jump past next jump)
        LT -> do
          emit1 (T.Jlt (cmpIx + 4))       --cmpIx + 0 (Jump past next jump)
      emit1 (T.Jump (-1))                 --cmpIx + 2 (jump to end of block)

      compileBody addr body

      emit1 (T.Jump (-1))                 --reserve jump to go to top of block
      endIx <- gets index
      --correct top jumps to point to end of block
      gets memory >>= lift . T.writeAtMem (fromIntegral cmpIx + 3) (T.marshall endIx)
      --correct bottom jump to point to top of block
      gets memory >>= lift . T.writeAtMem (fromIntegral endIx - 1) (T.marshall topIx)
    Return expr -> do
      compileExpr addr expr
      --Store Instruction
      ret <- getFnRetAddr addr
      emit1 (T.StoreFP ret)
      emit1 T.Rts
    StmtFnCall fn (FnArgs args) -> do
      emit1 $ T.Loadi 0                     --reserve 0 on the stack
      mapM_ (compileExpr addr)args    --compile exprs and leave values on stack
      ix1 <- gets index
      emit1 $ T.Jsr (-1)                    --emit jsr to jump to function (-1 is used as a stub address because it will seg-fault)
      modify' $ putCallback (fn, ix1+1) --add a callback (asking compiler to resolve address when function defined)
      --pop all arguments
      emit1 (T.Pop (fromIntegral $ length args))

-- |lookup a functions return address from the symbol table
getFnRetAddr :: (T.PMemory v a) => Addr -> Compiler s v a Offset
getFnRetAddr addr = do
  symtbl <- gets symtable
  return $ getFnRetAddr' addr symtbl
  where
    getFnRetAddr' :: Addr -> SymTable -> Offset
    getFnRetAddr' addr' symtbl = case addr' of
      [x] -> case M.lookup x symtbl of
        Just (Function _ r _ _) -> r
        _ -> error "Compiler encountered malformed symbol table when compiling"
      x:xs -> case M.lookup x symtbl of
        Just (Function _ _ _ s) -> getFnRetAddr' xs s
        _ -> error "Compiler encountered malformed symbol table when compiling"
      []  -> error "Main body does not have a return address"

compileComparison :: (T.PMemory v a) => Addr -> Comparison -> Compiler s v a RelationalOp
compileComparison addr (Comparison e1 op e2) = do
  compileExpr addr e1    --evaluate both expressions, leaving their values ontop of the stack
  compileExpr addr e2
  emit1 T.Sub            --subttact and Jumpe
  emit1 T.Test
  emit1 (T.Pop 1)        --once tested, remove value from stack
  --in future, other comparison ops may be introduced
  return op

compileVarDecl :: (T.PMemory v a) => Addr -> Offset -> VarDeclaration -> Compiler s v a Offset
compileVarDecl addr off var = do
  case var of
    VarDecl ident -> do
      emit1 $ T.Loadi 0         --put 0 onto stack
      updateVarOffset addr ident off  --update the variables defined location (symtable)
      return $ succ off      --return incremented offset
    VarExpr ident expr -> do
      compileExpr addr expr       --compile expression, leave value on top of stack
      updateVarOffset addr ident off  --update the variables defined location (symtable)
      return $ succ off      --return incremented offset

compileExpr :: (T.PMemory v a) => Addr -> Expression -> Compiler s v a ()
compileExpr addr expr = do
  case expr of
    IntLit i -> emit1 $ T.Loadi $ fromIntegral i
    ExprFnCall fn (FnArgs args) -> do
      emit1 $ T.Loadi 0                  --reserve 0 on the stack
      mapM_ (compileExpr addr) args      --compile exprs and leave values on stack
      ix1 <- gets index
      emit1 $ T.Jsr (-1)                 --emit jsr to jump to function (-1 is used as a stub address because it will seg-fault)
      modify' $ putCallback (fn, ix1+1)  --add a callback (asking compiler to resolve address when function defined)
      emit1 (T.Pop (fromIntegral $ length args))
    Variable var -> do
      (off, scope) <- resolveVar addr var   --get the offset of the variable
      case scope of                      --emit appropriate load instruction
        Global -> emit1 $ T.LoadGP off
        Local  -> emit1 $ T.LoadFP off
    Mult e1 e2 -> compileExpr addr e1 >> compileExpr addr e2 >> emit1 T.Mul
    Add e1 e2  -> compileExpr addr e1 >> compileExpr addr e2 >> emit1 T.Add
    Sub e1 e2  -> compileExpr addr e1 >> compileExpr addr e2 >> emit1 T.Sub
    Neg e      -> compileExpr addr e >> emit1 T.Neg

{-
lookup variable from symbol table
-}
resolveVar :: (T.PMemory v a) => Addr -> Identifier -> Compiler s v a (Offset, Scope)
resolveVar addr var = do
  symtbl <- gets symtable
  case resolveVar' symtbl addr var Global of
    Just n -> return n
    _ -> error "Compiler could not resolve variable from symbol table"

resolveVar' :: SymTable -> Addr -> Identifier -> Scope -> Maybe (Offset, Scope)
resolveVar' symtbl addr' var scope = case addr' of
  x:xs -> case M.lookup x symtbl of
    Just (Function _ _ _ s) -> case resolveVar' s xs var Local of
      Just (n, scope') -> Just (n, scope')
      Nothing -> resolveVar' symtbl [] var scope
    _ ->error "Compiler found malformed symbol table when compiling"
  [] ->case M.lookup var symtbl of
    Just (Var n) -> Just (n, scope)
    _ -> Nothing

{-|
Update the offset value of a variable within a scope. This involves searching
for the variable within the nested scopes.
|-}
updateVarOffset :: (T.PMemory v a) => Addr -> Identifier -> Offset -> Compiler s v a ()
updateVarOffset addr var off = do
  symtbl <- gets symtable
  case updateVarOffset' symtbl addr var off of
    Just s -> modify' $ putSymTbl s
    _ -> error "Compiler found malformed symbol table when compiling"
  where
{-
Pure helper function updating the offset of a variable.
This function searches for the variable from the bottom up, once found,
the offset is updated and the table returned. Nothing is returned if the variable
cannot be found. This approach allows for nesting scopes to be added at a later date.
-}
    updateVarOffset' :: SymTable -> Addr -> Identifier -> Offset -> Maybe SymTable
    updateVarOffset' symtbl addr' var' off' = case addr' of
      x:xs -> case M.lookup x symtbl of
        Just (Function o r c s) -> case updateVarOffset' s xs var' off' of
          Just s' -> Just $ M.insert x (Function o r c s') symtbl
          Nothing -> updateVarOffset' symtbl [] var' off'
        _ -> error "Compiler found malformed symbol table when compiling"
      _ -> case M.lookup var' symtbl of
        Just (Var _) -> Just $ M.insert var' (Var off') symtbl
        _ -> Nothing

{-
Does not deal with nested functions. If this were to be added, this function
would have to be extended.
-}
-- |Similar to updateVarOffset. adds a start address to a function once it is known
updateFunOffset :: (T.PMemory v a) => Identifier -> Offset -> Compiler s v a ()
updateFunOffset fun off = do
  symtbl <- gets symtable
  case M.lookup fun symtbl of
    Just (Function _ r c s) -> modify' $ putSymTbl $ M.insert fun (Function off r c s) symtbl
    _ -> error "Compiler found malformed symbol table when compiling"

-- |emit an instruction into memory at the current index
emit1 :: (T.PMemory v a) => T.Instruction -> Compiler s v a ()
emit1 instr = do
  mem <- gets memory
  ix <- gets index
  (lift $ T.insertInstrMem ix instr mem) >>= modify' . putIx
