{
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Declare (Exp(..))
import Tokens
}


%name parser
%tokentype { Token }
%error { parseError }

%token
    digits  { TokenInt $$ }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '^'     { TokenPow }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '%'     { TokenMod }
    '!'     { TokenFact }


%%

AExpr : AExpr '+' NExpr        { Add $1 $3 }
      | AExpr '-' NExpr        { Sub $1 $3 }
      | NExpr                  { $1 }


NExpr : '-' NExpr             { Neg $2 }
      | MExpr               { $1 }

MExpr : MExpr '*' Factor        { Mult $1 $3 }
     | MExpr '/' Factor        { Div $1 $3 }
     | MExpr '%' Factor        { Mod $1 $3 }
     | Factor                 { $1 }

Factor : Factor '^' Primary     { Power $1 $3 }
       | Primary '!'            { Fact $1 }
       | Primary                { $1 }

Primary : digits              { Num $1 }
        | '(' AExpr ')'       { $2 }



{

parseError _ = error "Parse error"

parseExpr = parser . scanTokens

}
