{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  $digit+                       { \s -> TokenInt (read s) }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \^                            { \s -> TokenPow }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \!                            { \s -> TokenFact }
  \%                            { \s -> TokenMod }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }



{
-- The token type:
data Token = TokenInt Int
           | TokenSym String
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenPow
           | TokenLParen
           | TokenRParen
           | TokenFact
           | TokenMod
           deriving (Eq,Show)

scanTokens = alexScanTokens
}
