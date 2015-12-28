module AyaScript where

import Data.List
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

data Stmt = Expr Expr
          | Decl Expr Expr
          | Assign Expr Expr
  deriving (Show, Eq)

data Expr = Natural Integer
          | UnaryOp String Expr
          | BinOp String Expr Expr
          | Var String
          | Fun String Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Tuple [Expr]
          | List [Expr]
  deriving (Show, Eq)

type Program = [Stmt]

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

genES :: Program -> String
genES stmts =
  "{\"type\":\"Program\"," ++
   "\"body\":[" ++ (intercalate "," $ genESStmt <$> stmts) ++ "]," ++
   "\"sourceType\":\"script\"}"

genESStmt :: Stmt -> String
genESStmt (Expr e) =
  "{\"type\":\"ExpressionStatement\"," ++
   "\"expression\":" ++ genESExpr e ++ "}"

genESStmt (Decl e1 e2) =
  "{\"type\":\"VariableDeclaration\"," ++
   "\"declarations\":[{" ++
       "\"type\":\"VariableDeclarator\"," ++
       "\"id\":" ++ genESExpr e1 ++ "," ++
       "\"init\":" ++ genESExpr e2 ++ "}" ++
     "]," ++
   "\"kind\":\"var\"}"

genESStmt (Assign e1 e2) =
  "{\"type\":\"ExpressionStatement\"," ++
   "\"expression\":{" ++
     "\"type\":\"AssignmentExpression\"," ++
     "\"operator\":\"=\"," ++
     "\"left\":" ++ genESExpr e1 ++ "," ++
     "\"right\":" ++ genESExpr e2 ++ "}}"

genESExpr :: Expr -> String
genESExpr (Natural x) =
  "{\"type\":\"Literal\"," ++
   "\"value\":" ++ show x ++ "," ++
   "\"raw\":\"" ++ show x ++ "\"}"

genESExpr (UnaryOp op e) =
  "{\"type\":\"UnaryExpression\"," ++
   "\"operator\":\"" ++ op ++ "\"," ++
   "\"argument\":" ++ genESExpr e ++ "," ++
   "\"prefix\":true}"

genESExpr (BinOp "." e1 e2@(Var _)) =
  "{\"type\":\"MemberExpression\"," ++
   "\"computed\":false," ++
   "\"object\":" ++ genESExpr e1 ++ "," ++
   "\"property\":" ++ genESExpr e2 ++ "}"

genESExpr (BinOp "." e1 e2) =
  "{\"type\":\"MemberExpression\"," ++
   "\"computed\":true," ++
   "\"object\":" ++ genESExpr e1 ++ "," ++
   "\"property\":" ++ genESExpr e2 ++ "}"

genESExpr (BinOp "**" e1 e2) = genESExpr (App (BinOp "." (Var "Math") (Var "pow")) (Tuple [e1, e2]))

genESExpr (BinOp "==" e1 e2) = genESExpr (BinOp "===" e1 e2)

genESExpr (BinOp "/=" e1 e2) = genESExpr (BinOp "!==" e1 e2)

genESExpr (BinOp op e1 e2) =
  "{\"type\":\"BinaryExpression\"," ++
   "\"operator\":\"" ++ op ++ "\"," ++
   "\"left\":" ++ genESExpr e1 ++ "," ++
   "\"right\":" ++ genESExpr e2 ++ "}"

genESExpr (Var var) =
  "{\"type\":\"Identifier\"," ++
   "\"name\":\"" ++ var ++ "\"}"

genESExpr (Fun param e) =
  "{\"type\":\"ArrowFunctionExpression\"," ++
   "\"id\":null," ++
   "\"params\":[" ++ genESExpr (Var param) ++ "]," ++
   "\"defaults\":[]," ++
   "\"body\":" ++ genESExpr e ++ "," ++
   "\"generator\":false," ++
   "\"expression\":true}"

genESExpr (App e1 (Tuple es)) =
  "{\"type\":\"CallExpression\"," ++
   "\"callee\":" ++ genESExpr e1 ++ "," ++
   "\"arguments\":[" ++ (intercalate "," $ genESExpr <$> es) ++ "]}"

genESExpr (App e1 e2) =
  "{\"type\":\"CallExpression\"," ++
   "\"callee\":" ++ genESExpr e1 ++ "," ++
   "\"arguments\":[" ++ genESExpr e2 ++ "]}"

genESExpr (If e1 e2 e3) =
  "{\"type\":\"ConditionalExpression\"," ++
   "\"test\":" ++ genESExpr e1 ++ "," ++
   "\"consequent\":" ++ genESExpr e2 ++ "," ++
   "\"alternate\":" ++ genESExpr e3 ++ "}"

genESExpr (Tuple es) = genESExpr (List es)

genESExpr (List es) =
 "{\"type\":\"ArrayExpression\"," ++
  "\"elements\":[" ++ (intercalate "," $ genESExpr <$> es) ++ "]}"
