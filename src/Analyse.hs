{-
Check scoping issues
Syntax Check
-}
module Analyse (
  ContextType(..),
  Context,
  SemanticCheck,
  SemanticError,
  Offset,
  Addr,
  Identifier,
  checkProgram
               ) where

import Parser

import Prelude
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Int (Int16)

import qualified Data.Map.Strict as M

{-|
Addr holds the 'address' of each scope. Because scopes (functions) form a tree
structure, it is nessacary to have the address of each scope as a list of address
that lead from the root.
e.g. The address of the scope within the function 'f' is ["f"]
the root of the tree is the global scope and its address is []
|-}
type Addr = [String]

{-|
An identifier is that name given to a function or variable. It must be unique within
its scope, however, identifiers from outer scopes can be reused and the analyser
and comiler will resolve these from the bottom up.
|-}
type Identifier = String

{-|
The symbol table will be used by the compiler to encode certain information about
the location of variables and functions within memory. The Offset type is used to
hold this information.
|-}
type Offset = Int16

{-|
This type will hold the values of the function scopes and variables that represent
the symbol table. It is recursivly defined and forms a tree like structure in
which nested scopes are defined.
|-}
data ContextType =
    Var Offset
    | Function {fOffset :: Offset, fRetAddr :: Offset, fVarCount :: Int, fContext :: Context}
    deriving (Eq, Show, Ord)

{-|
It is not the job of the analyser to resolve location information and the like
about variables, thus a 'null' type is used since the information will be worked
out later during the compilation phase.
|-}
varNull :: ContextType
varNull = Var 0

{-|
A Context is a map of identifiers to ContextTypes.
|-}
type Context = M.Map Identifier ContextType

{-|
The SemanticCheck monad will throw exceptions when semantic errors are uncovered,
this removes the tedium of passing bool's up the tree back to the root. The State
monad is used to hold the symbol table.
|-}
type SemanticCheck = ExceptT SemanticError (State Context)

data ErrorType = ReturnInMain
               | IncorrectFunctionParameters
               | FunctionCallOnVariable
               | UndefinedFunctionCalled
               | InvalidContextAddress
               | VarOrFuncRedefinition
               | UninitialisedVariable
               | UsedFunctionAsVariable

instance Show ErrorType where
  show err = case err of
    ReturnInMain ->
      "Analyser found return call within main body in the expression: %s\n"
    IncorrectFunctionParameters ->
      "Analyser found call fo function: %s with incorrect parameters\n"
    FunctionCallOnVariable ->
      "Analyser found function call was actually a variable: %s\n"
    UndefinedFunctionCalled ->
      "Analyser found call to undeclared function: %s\n"
    InvalidContextAddress ->
      "Analyser looked for context: %s and found none\n"
    VarOrFuncRedefinition ->
      "Variable or Function already within context: %s\n"
    UninitialisedVariable ->
      "Analyser detected usage of uninitialised variable: %s\n"
    UsedFunctionAsVariable ->
      "Analyser looked for function: %s and found variable\n"

{-|
Predefined error type, this type can be expanded to encode error codes at a
later date.
|-}
type SemanticError = (ErrorType, String)

{-
It's a fairly common pattern to perform a semantic check on a data structure in
the parse tree. To save creating a new function each time, each datastructure
implaments Checkable, optionally taking an address as well.
-}
class Checkable a where
  checkAddr :: Addr -> a -> SemanticCheck ()
  checkAddr _ c =  check c
  check ::  a -> SemanticCheck ()
  check = checkAddr []

{-|
This is the function will perform semantic analysis on the Program tree. This
analysis will produce a symbol table to be used by the compiler, as well as throw
throw errors for:
    variable redeclaration within the same scope
    calling a variable
    assigning to a function
    using an undeclared variable
    calling an undeclared function
    calling a function with incorrect parameters
-}
instance Checkable Program where
  check (Program _ vardecs fundecs body) = do
    {-add function declarations to symbol table
    (out of order parsing allows functions to be called before they're defined)-}
    fillContextFuncDecs fundecs
    fillContextVars [] vardecs   --add global variables to symbol table
    fillContextFuncBodys fundecs --add local variables to their symbol table

    check fundecs                --with the symbol table populated, Analyse the contents of each function
    checkAddr [] body            --likewise, check the body of the mainline

{-| here is the entry point for the semantic analysis -}
checkProgram :: Program -> SemanticCheck()
checkProgram = check

-- |Add variables into global pointed to by addr.
fillContextVars :: Addr -> VarDeclarations -> SemanticCheck ()
fillContextVars addr (VarDecls vars) = mapM_ (insertVarIntoContext addr) vars


-- |semantic check on the 'compound statement' pointed to by addr
instance Checkable CompoundStatement where
  checkAddr addr (CompoundStatement (Statements stmts)) = mapM_ (checkAddr addr) stmts

-- |semantic check on the 'Statement' pointed to by addr
instance Checkable Statement where
  checkAddr addr stmt = case stmt of
    Up      -> return ()
    Down    -> return ()
    Moveto e1 e2 -> checkAddr addr e1 >> checkAddr addr e2
    Read ident   -> varWithinScope addr ident
    Assignment var expr -> varWithinScope addr var >> checkAddr addr expr
    IfElse cond b1 b2 -> checkAddr addr cond >> checkAddr addr b1 >> checkAddr addr b2
    If cond body      -> checkAddr addr cond >> checkAddr addr body
    While cond body   -> checkAddr addr cond >> checkAddr addr body
    Return expr -> case addr of
      [] -> throwError (ReturnInMain, show stmt)
      _  -> checkAddr addr expr
    StmtFnCall fn args -> checkFnCall fn args

{-|
Semantic Check on a function call, that is:
  - Has the function been declared.
      Assumes functions processed and in context before being called
  - Does the call have the correct number of parameters
      Known because function delcarations already processed
|-}
checkFnCall :: Identifier -> FnArguments -> SemanticCheck ()
checkFnCall fun (FnArgs args) = do
  ctx <- get
  case M.lookup fun ctx of
    Just (Function _ _ len _)
      | len == length args -> return ()
      | otherwise          -> throwError (IncorrectFunctionParameters, fun)
    Just _  -> throwError (FunctionCallOnVariable, fun)
    Nothing -> throwError (UndefinedFunctionCalled, fun)

-- |Semantic Check on conditional expression
instance Checkable Comparison where
  checkAddr addr (Comparison e1 _ e2) = checkAddr addr e1 >> checkAddr addr e2

-- |Semantic Check on an 'Expression'
instance Checkable Expression where
  checkAddr addr expr = case expr of
    IntLit _ -> return ()
    ExprFnCall fn args -> checkFnCall fn args
    Variable var -> varWithinScope addr var    -- Enforce variable scoping
    Mult e1 e2   -> checkAddr addr e1 >> checkAddr addr e2
    Add e1 e2   -> checkAddr addr e1 >> checkAddr addr e2
    Sub e1 e2   -> checkAddr addr e1 >> checkAddr addr e2
    Neg e1      -> checkAddr addr e1

-- |insert a 'varNull' into symbol table pointed to by addr
insertVarIntoContext :: Addr -> VarDeclaration -> SemanticCheck ()
insertVarIntoContext addr var = do
    ctx <- get
    case var of
      VarDecl name -> insertIntoContext ctx addr name varNull >>= put
      VarExpr name expr -> checkAddr addr expr >> insertIntoContext ctx addr name varNull >>= put

{-|
Insert a functtion into a context given by addr.
The use of addr in this function is here to allow for sub funcitons or while/if
scoping sould I care to add it at a later date.
|-}
insertFuncIntoContext :: Addr -> FuncDeclaration -> SemanticCheck ()
insertFuncIntoContext addr (FuncDeclaration name (Params args) _ _) = do
  ctx <- get
  let n = length args
      retAddr = fromIntegral (-n) - 2        -- calculate return address
  insertIntoContext ctx addr name (Function 0 retAddr n M.empty) >>= put

{-|
Insert a value into the context pointed to by Addr.
This function is also allows for sub functions to be inserted into functions
should I add this functionality at a later date.
|-}
insertIntoContext :: Context -> Addr -> Identifier -> ContextType -> SemanticCheck Context
insertIntoContext ctx addr name val = case addr of
  x:xs -> case M.lookup x ctx of
    Nothing -> throwError (InvalidContextAddress, x)
    Just (Function _ r n s) -> do
      newScope <- insertIntoContext s xs name val
      return $ M.insert x (Function 0 r n newScope) ctx
    _       -> throwError (UsedFunctionAsVariable, x)
  []   -> case M.lookup name ctx of
    Nothing -> return $ M.insert name val ctx
    _       -> throwError (VarOrFuncRedefinition, name)

-- |Fill a function scope with its parameters and declared local variables
fillFuncBody :: Addr -> FuncDeclaration -> SemanticCheck ()
fillFuncBody addr (FuncDeclaration name (Params args) vars _) = do
    let newAddr = addr ++ [name]
    mapM_ (insertParamIntoContext newAddr) args --insert Parameters into scope
    fillContextVars newAddr vars                --insert local variable declarations into scope
  where
    insertParamIntoContext :: Addr -> Identifier -> SemanticCheck ()
    insertParamIntoContext addr' var = do
      ctx <- get
      insertIntoContext ctx addr' var varNull >>= put

{-|
Check if a variable usage is valid within a given scope
This involves checking the scope and its parent scopes until the variable is found
or the root is hit.
|-}
varWithinScope :: Addr -> Identifier -> SemanticCheck ()
varWithinScope addr name = do
  ctx <- get
  varWithinScope' ctx addr name >>= (\found -> unless found $
                                      throwError (UninitialisedVariable, name))
  where
    -- Actually check if variable is within scope
    varWithinScope' :: Context -> Addr -> Identifier -> SemanticCheck Bool
    varWithinScope' ctx addr' var = case addr' of
      x:xs -> case M.lookup x ctx of

                Nothing -> throwError (UndefinedFunctionCalled, x)
                Just (Function _ _ _ s) -> varWithinScope' s xs var >>= (\found -> unless' found $ varWithinScope' ctx [] var)
                _       -> throwError (UsedFunctionAsVariable, x)
      []   -> case M.lookup var ctx of
                Just (Var _) -> return True
                Nothing  -> return False
                _        -> throwError (UsedFunctionAsVariable, var)

    -- Only check higher scopes if not found in lower ones using unless'
    unless' :: Applicative f => Bool -> f Bool -> f Bool
    unless' cond f = if cond then pure True else f

-- |Put function declarations into symbol table to allow them to be called
fillContextFuncDecs :: FuncDeclarations -> SemanticCheck ()
fillContextFuncDecs (FuncDecls funcs) = mapM_ (insertFuncIntoContext []) funcs

-- |Put local variable declarations into their symbol tables
fillContextFuncBodys :: FuncDeclarations -> SemanticCheck ()
fillContextFuncBodys (FuncDecls funcs) = mapM_ (fillFuncBody []) funcs

-- |Check the function bodies for semantic correctness
instance Checkable FuncDeclarations where
  check (FuncDecls funcs) = mapM_ (checkAddr []) funcs

-- |Check the body of a function for semantic correctness
instance Checkable FuncDeclaration where
  checkAddr addr (FuncDeclaration name _ _ body) = checkAddr (addr ++ [name]) body
