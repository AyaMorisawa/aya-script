module AyaScript where

import AyaScript.GenES
import AyaScript.Program
import AyaScript.Stmt
import AyaScript.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

many2 :: Parser a -> Parser [a]
many2 x = (:) <$> x <*> many1 x

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { reservedOpNames = [
  ".",
  "**",
  "*", "/",
  "+", "-",
  "&&", "||",
  ">", "<", ">=", "<=",
  "==", "/="
]})

natural     = P.natural lexer
parens      = P.parens lexer
reservedOp  = P.reservedOp lexer
identifier  = P.identifier lexer
lexeme      = P.lexeme lexer

compile :: String -> Either ParseError String
compile x = genES <$> ayaScriptParser x

ayaScriptParser :: String -> Either ParseError Program
ayaScriptParser = parse program ""

program :: Parser Program
program = stmts <* eof

stmts :: Parser [Stmt]
stmts = sepBy stmt (char '\n')

stmt :: Parser Stmt
stmt = try (Decl <$> expr <*> (lexeme (char '=') *> expr))
   <|> try (Assign <$> expr <*> (lexeme (string "#=") *> expr))
   <|> Expr <$> expr

expr :: Parser Expr
expr = buildExpressionParser table term
   <|> fun
   <|> ifExpr
   <?> "expression"
  where
    table = [[binop "." AssocLeft],
             [unary "-"],
             [binop "**" AssocRight],
             [binop "*" AssocLeft, binop "/" AssocLeft],
             [binop "+" AssocLeft, binop "-" AssocLeft],
             [binop "&&" AssocLeft, binop "||" AssocLeft],
             [binop ">" AssocLeft, binop "<" AssocLeft, binop ">=" AssocLeft, binop "<=" AssocLeft],
             [binop "==" AssocLeft, binop "/=" AssocLeft]]
    binop op assoc = Infix (do
        reservedOp op
        return $ BinOp op
      <?> "operator") assoc
    unary op = Prefix (do
        reservedOp op
        return $ UnaryOp op
      )

exprs :: Parser [Expr]
exprs = sepBy expr (lexeme (char ','))

term :: Parser Expr
term = try mapOp
   <|> try pipeApp
   <|> try app
   <|> factor

mapOp :: Parser Expr
mapOp = foldl1 mapApp <$> ((:) <$> factor <*> many1 (lexeme (string "<$>") *> factor))
  where
    mapApp f xs = App (BinOp "." xs (Var "map")) f

pipeApp :: Parser Expr
pipeApp = foldl1 (flip App) <$> ((:) <$> factor <*> many1 (lexeme (string "|>") *> factor))

app :: Parser Expr
app = foldl1 App <$> many2 factor

factor :: Parser Expr
factor = Natural <$> natural
     <|> try (Str <$> (lexeme (char '"') *> many (noneOf "\"") <* lexeme (char '"')))
     <|> Var <$> identifier
     <|> try (parens expr)
     <|> tuple
     <|> list

tuple :: Parser Expr
tuple = Tuple <$> parens exprs

list :: Parser Expr
list = List <$> (lexeme (char '[') *> exprs <* lexeme (char ']'))

fun :: Parser Expr
fun = Fun <$> (lexeme (char '\\') *> identifier)
          <*> (lexeme (string "->") *> expr)

ifExpr :: Parser Expr
ifExpr = If <$> (lexeme (string "if") *> expr)
            <*> (lexeme (string "then") *> expr)
            <*> (lexeme (string "else") *> expr)
