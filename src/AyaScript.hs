module AyaScript where

import AyaScript.GenES
import AyaScript.Types
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef{ reservedOpNames = [] })

natural     = P.natural lexer
parens      = P.parens lexer
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
expr = pipeOp

exprs :: Parser [Expr]
exprs = sepBy expr (lexeme (char ','))

pipeOp, mapOp, addOp, mulOp, expOp, logicOp, eqOp, compareOp, appOp, memberOp :: Parser Expr
pipeOp    = binOp AssocLeft  ["|>"] mapOp
mapOp     = binOp AssocLeft  ["<$>"] addOp
addOp     = binOp AssocLeft  ["+", "-"] mulOp
mulOp     = binOp AssocLeft  ["*", "/"] expOp
expOp     = binOp AssocRight ["**"] logicOp
logicOp   = binOp AssocLeft  ["&&", "||"] eqOp
eqOp      = binOp AssocLeft  ["==", "/="] compareOp
compareOp = binOp AssocLeft  ["<=", ">=", "<", ">"] appOp
appOp     = binOp AssocLeft  [""] memberOp
memberOp  = binOp AssocLeft  ["."] value

value :: Parser Expr
value = try ifE
    <|> try negateOp
    <|> funE
    <|> listE
    <|> try (parens expr)
    <|> tupleE
    <|> var
    <|> numL
    <|> strL

negateOp :: Parser Expr
negateOp  = unaryOp ["-"] memberOp

ifE :: Parser Expr
ifE = If <$> (lexeme (string "if") *> expr)
         <*> (lexeme (string "then") *> expr)
         <*> (lexeme (string "else") *> expr)

funE :: Parser Expr
funE = Fun <$> (lexeme (char '\\') *> identifier)
          <*> (lexeme (string "->") *> expr)

listE :: Parser Expr
listE = List <$> (lexeme (char '[') *> exprs <* lexeme (char ']'))

tupleE :: Parser Expr
tupleE = Tuple <$> parens exprs

var :: Parser Expr
var = Var <$> identifier

numL :: Parser Expr
numL = Natural <$> natural

strL :: Parser Expr
strL = Str <$> (lexeme (char '"') *> many (noneOf "\"") <* lexeme (char '"'))

binOp :: Assoc -> [String] -> Parser Expr -> Parser Expr
binOp assoc ops prev = do
  e1 <- prev
  es <- many $ try $ do
    op <- lexeme $ foldl1 (<|>) (try . string <$> ops)
    e2 <- prev
    return $ (op, e2)
  return $ case assoc of
    AssocLeft -> foldl (\acc (op, e2) -> BinOp op acc e2) e1 es
    AssocRight -> foldr (\(op, e2) acc -> BinOp op acc e2) e1 es

unaryOp :: [String] -> Parser Expr -> Parser Expr
unaryOp ops prev = do
  op <- foldl1 (<|>) (try . string <$> ops)
  e <- try prev
  return $ UnaryOp op e
