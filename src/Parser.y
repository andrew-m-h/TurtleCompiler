{
module Parser where

import Lexer
import Prelude hiding (Ordering(..))
import Data.Functor

{-
The parse tree is derived from the turtle specification.
Standard alex tricks are used to make parsing faster, and newtypes are used 
to ensure typesafety with zero cost.
-}
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    int_lit     {T_IntLit $$}
    identifier  {T_Identifier $$}
    "while"     {T_While}
    "if"        {T_If}
    "else"      {T_Else}
    "return"    {T_Return}
    "turtle"    {T_Turtle}
    "var"       {T_Var}
    "fun"       {T_Fun}
    "up"        {T_Up}
    "down"      {T_Down}
    "moveto"    {T_Moveto}
    "read"      {T_Read}
    "}"         {T_RBrace}
    "{"         {T_LBrace}
    ")"         {T_RParen}
    "("         {T_LParen}
    "="         {T_Assign}
    "=="        {T_EQ}
    "<"         {T_LT}
    "+"         {T_Add}
    "-"         {T_Sub}
    "*"         {T_Times}
    ","         {T_Comma}

%nonassoc "<" "=="
%left "+" "-"
%left "*" "/"

%%

Program :
        "turtle" identifier VarDeclarations
         FuncDeclarations CompoundStatement              {Program $2 $3 $4 $5}

VarDeclarations :
        VarDeclarations1                                 {VarDecls $1}

VarDeclarations1 :
                VarDeclaration VarDeclarations1          {$1 : $2}
                |                                        {[]}

VarDeclaration :
        "var" identifier "=" Expression                  {VarExpr $2 $4}
        | "var" identifier                               {VarDecl $2}

FuncDeclarations : FuncDeclarations1                     {FuncDecls $1}

FuncDeclarations1 :
        FuncDeclaration  FuncDeclarations1               {$1 : $2}
        |                                                {[]}

FuncDeclaration :
        "fun" identifier "(" Parameters ")"
        VarDeclarations CompoundStatement                {FuncDeclaration $2 $4 $6 $7}

Parameters : Parameters1                                 {Params $1}

Parameters1 :
        identifier "," Parameters1                       {$1 : $3}
        | identifier                                     {[$1]}
        |                                                {[]}

CompoundStatement :
       "{"  Statements  "}"                              {CompoundStatement $2}

Statements : Statements1                                 {Statements $1}

Statements1 :
          Statement  Statements1                         {$1 : $2}
          |                                              {[]}

Statement :
        "up"                                             {Up}
        | "down"                                         {Down}
        | "moveto" "(" Expression "," Expression ")"     {Moveto $3 $5}
        | "read" "(" identifier ")"                      {Read $3}
        | identifier "=" Expression                      {Assignment $1 $3}
        | "if" "(" Comparison ")"
        CompoundStatement "else" CompoundStatement       {IfElse $3 $5 $7}
        | "if" "(" Comparison ")" CompoundStatement      {If $3 $5}
        | "while" "(" Comparison ")" CompoundStatement   {While $3 $5}
        | "return" Expression                            {Return $2}
        | identifier "(" FnArguments ")"                 {StmtFnCall $1 $3}

Comparison :
        Expression RelationalOp Expression               {Comparison $1 $2 $3}

RelationalOp :
        "=="                                             {Equals}
        | "<"                                            {LT}

FnArguments : FnArguments1                               {FnArgs $1}

FnArguments1 :
        Expression "," FnArguments1                      {$1 : $3}
        | Expression                                     {[$1]}
        |                                                {[]}

Expression :
        Expression "+" Expression                        {Add $1 $3}
        | Expression "-" Expression                      {Sub $1 $3}
        | Expression "*" Expression                      {Mult $1 $3}
        | "-" Expression                                 {Neg $2}
        | "(" Expression ")"                             {$2}
        | int_lit                                        {IntLit $1}
        | identifier "(" FnArguments ")"                 {ExprFnCall $1 $3}
        | identifier                                     {Variable $1}


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Program =
    Program String VarDeclarations FuncDeclarations CompoundStatement
    deriving (Eq, Show)

newtype VarDeclarations = VarDecls [VarDeclaration]
    deriving (Eq, Show)

data VarDeclaration =
    VarExpr String Expression
    | VarDecl String
    deriving (Eq, Show)

newtype FuncDeclarations = FuncDecls [FuncDeclaration]
    deriving (Eq, Show)

data FuncDeclaration =
    FuncDeclaration String Parameters VarDeclarations CompoundStatement
    deriving (Eq, Show)

newtype Parameters = Params [String]
    deriving (Eq, Show)

newtype CompoundStatement = CompoundStatement Statements
    deriving (Eq, Show)

newtype Statements = Statements [Statement]
    deriving (Eq, Show)

data Statement =
    Up
    | Down
    | Moveto Expression Expression
    | Read String
    | Assignment String Expression
    | IfElse Comparison CompoundStatement CompoundStatement
    | If Comparison CompoundStatement
    | While Comparison CompoundStatement
    | Return Expression
    | StmtFnCall String FnArguments
    deriving (Eq, Show)

data Comparison =
    Comparison Expression RelationalOp Expression
    deriving (Eq, Show)

data RelationalOp =
    Equals
    | LT
    deriving (Eq, Show)

newtype FnArguments = FnArgs [Expression]
    deriving (Eq, Show)

data Expression =
    IntLit Int
    | ExprFnCall String FnArguments
    | Variable String
    | Mult Expression Expression
    | Add Expression Expression
    | Sub Expression Expression
    | Neg Expression
    deriving (Eq, Show)

{-
main :: IO()
main = do
  inStr <- getContents
  let parseTree = parse (alexScanTokens inStr)
  putStrLn ("parseTree: " ++ show(parseTree))
  print "done"
-}
}
