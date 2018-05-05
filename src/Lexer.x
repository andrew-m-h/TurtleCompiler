{
module Lexer where

import Prelude
}

%wrapper "basic"

$decdig     = [0-9]
$hexdig     = [0-9A-Fa-f]
$identstart = [a-zA-Z]
$identbody  = [a-zA-Z_'0-9]
$graphic = $printable # $white

tokens :-
    $white+                     ;
    "//".*                      ;

    $decdig+ {\s -> T_IntLit (read s)}

    "turtle"        {const T_Turtle}
    "var"           {const T_Var}
    "fun"           {const T_Fun}

    "up"            {const T_Up}
    "down"          {const T_Down}
    "moveto"        {const T_Moveto}
    "read"          {const T_Read}

    "if"            {const T_If}
     "while"        {const T_While}
     "else"         {const T_Else}
     "return"       {const T_Return}

    "{"             {const T_LBrace}
    "}"             {const T_RBrace}
    "("             {const T_LParen}
    ")"             {const T_RParen}

    "=="            {const T_EQ}
    "="             {const T_Assign}
    "<"             {const T_LT}
    "+"             {const T_Add}
    "-"             {const T_Sub}
    "*"             {const T_Times}
    ","             {const T_Comma}
    $identstart $identbody*  {T_Identifier}

{

data Token =
        T_IntLit  Int
        | T_Identifier String
        | T_While
        | T_If
        | T_Else
        | T_RBrace
        | T_LBrace
        | T_RParen
        | T_LParen
        | T_Assign
        | T_EQ
        | T_LT
        | T_Add
        | T_Sub
        | T_Times
        | T_Comma
        | T_Return
        | T_Turtle
        | T_Var
        | T_Fun
        | T_Up
        | T_Down
        | T_Moveto
        | T_Read
        deriving (Eq, Show)

}
