module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import AccTree

expression :: GenParser Char st AST
expression
    =   literal
    <|> binaryExpression
    <|> variable

binaryExpression :: GenParser Char st AST
binaryExpression = do
    operand1 <- literal
    op <- operator
    operand2 <- expression

    return $
        Tree (ASTNode "Expr" "")
            [
                Tree (ASTNode "Op" op) 
                [
                    operand1,
                    operand2
                ]
            ]

literal :: GenParser Char st AST
literal = number <|> variable

number :: GenParser Char st AST
number =
    astLeafNode "Num" <$>
        (string "0" <|> fmap (: []) (oneOf "123456789") <> many digit)

variable :: GenParser Char st AST
variable = do
    firstChar <- choice [letter, char '_']
    otherChars <- many (choice [alphaNum , char '_'])

    return $ astLeafNode "Var" (firstChar : otherChars)

operator :: GenParser Char st String
operator = show <$> oneOf ['+', '-', '*', '/', '%']

parseExpression :: String -> Either ParseError AST
parseExpression = parse expression ""