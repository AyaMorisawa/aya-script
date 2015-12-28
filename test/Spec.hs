import Test.Hspec
import AyaScript
import AyaScript.Types

main :: IO ()
main = hspec spec

test :: String -> Program -> (String, String) -> SpecWith ()
test code asAst (es, esAst) =
  context code $ do
    it ("parses to " ++ showAsAst asAst) $ parse code `shouldBe` Right asAst
    it ("compiles to " ++ es) $
      compile code `shouldBe` Right esAst
  where
    showAsAst asAst
      | length asAst == 1 = show $ head asAst
      | otherwise         = show asAst

spec :: Spec
spec = do
  describe "expression" $ do
    describe "binary-operator" $ do
      test "a + b" [Expr (BinOp "+" (Var "a") (Var "b"))] (
          "a + b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"+\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a - b" [Expr (BinOp "-" (Var "a") (Var "b"))] (
          "a - b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"-\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a * b" [Expr (BinOp "*" (Var "a") (Var "b"))] (
          "a * b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"*\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a / b" [Expr (BinOp "/" (Var "a") (Var "b"))] (
          "a / b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"/\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a ** b" [Expr (BinOp "**" (Var "a") (Var "b"))] (
          "Math.pow(a, b)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"a\",\"type\":\"Identifier\"},{\"name\":\"b\",\"type\":\"Identifier\"}],\"callee\":{\"property\":{\"name\":\"pow\",\"type\":\"Identifier\"},\"computed\":false,\"object\":{\"name\":\"Math\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a ** b ** c" [Expr (BinOp "**" (Var "a") (BinOp "**" (Var "b") (Var "c")))] (
          "Math.pow(a, Math.pow(b, c))", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"a\",\"type\":\"Identifier\"},{\"arguments\":[{\"name\":\"b\",\"type\":\"Identifier\"},{\"name\":\"c\",\"type\":\"Identifier\"}],\"callee\":{\"property\":{\"name\":\"pow\",\"type\":\"Identifier\"},\"computed\":false,\"object\":{\"name\":\"Math\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"type\":\"CallExpression\"}],\"callee\":{\"property\":{\"name\":\"pow\",\"type\":\"Identifier\"},\"computed\":false,\"object\":{\"name\":\"Math\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a && b" [Expr (BinOp "&&" (Var "a") (Var "b"))] (
          "a && b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"&&\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a || b" [Expr (BinOp "||" (Var "a") (Var "b"))] (
          "a || b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"||\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a == b" [Expr (BinOp "==" (Var "a") (Var "b"))] (
          "a === b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"===\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a /= b" [Expr (BinOp "/=" (Var "a") (Var "b"))] (
          "a !== b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"!==\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a < b" [Expr (BinOp "<" (Var "a") (Var "b"))] (
          "a < b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"<\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a > b" [Expr (BinOp ">" (Var "a") (Var "b"))] (
          "a > b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\">\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a <= b" [Expr (BinOp "<=" (Var "a") (Var "b"))] (
          "a <= b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"<=\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a >= b" [Expr (BinOp ">=" (Var "a") (Var "b"))] (
          "a >= b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\">=\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"name\":\"b\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a + (b * c)" [Expr (BinOp "+" (Var "a") (BinOp "*" (Var "b") (Var "c")))] (
          "a + b * c", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"+\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"operator\":\"*\",\"left\":{\"name\":\"b\",\"type\":\"Identifier\"},\"right\":{\"name\":\"c\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a * (b + c)" [Expr (BinOp "*" (Var "a") (BinOp "+" (Var "b") (Var "c")))] (
          "a * (b + c)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"*\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"operator\":\"+\",\"left\":{\"name\":\"b\",\"type\":\"Identifier\"},\"right\":{\"name\":\"c\",\"type\":\"Identifier\"},\"type\":\"BinaryExpression\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "negate-operator" $ do
      test "-a" [Expr (UnaryOp "-" (Var "a"))] (
          "-a", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"-\",\"prefix\":true,\"argument\":{\"name\":\"a\",\"type\":\"Identifier\"},\"type\":\"UnaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "mapping-operator" $ do
      test "f <$> xs" [Expr (BinOp "<$>" (Var "f") (Var "xs"))] (
          "xs.map(f)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"f\",\"type\":\"Identifier\"}],\"callee\":{\"property\":{\"name\":\"map\",\"type\":\"Identifier\"},\"computed\":false,\"object\":{\"name\":\"xs\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "application" $ do
      test "f x" [Expr (BinOp "" (Var "f") (Var "x"))] (
          "f(x)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"x\",\"type\":\"Identifier\"}],\"callee\":{\"name\":\"f\",\"type\":\"Identifier\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "f a b" [Expr (BinOp "" (BinOp "" (Var "f") (Var "a")) (Var "b"))] (
          "f(a)(b)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"b\",\"type\":\"Identifier\"}],\"callee\":{\"arguments\":[{\"name\":\"a\",\"type\":\"Identifier\"}],\"callee\":{\"name\":\"f\",\"type\":\"Identifier\"},\"type\":\"CallExpression\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "piping-operator" $ do
      test "x |> f" [Expr (BinOp "|>" (Var "x") (Var "f"))] (
          "f(x)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"x\",\"type\":\"Identifier\"}],\"callee\":{\"name\":\"f\",\"type\":\"Identifier\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "x |> f |> g" [Expr (BinOp "|>" (BinOp "|>" (Var "x") (Var "f")) (Var "g"))] (
          "g(f(x))", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"arguments\":[{\"name\":\"x\",\"type\":\"Identifier\"}],\"callee\":{\"name\":\"f\",\"type\":\"Identifier\"},\"type\":\"CallExpression\"}],\"callee\":{\"name\":\"g\",\"type\":\"Identifier\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "tuple" $ do
      test "()" [Expr (Tuple [])] (
          "[]", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"elements\":[],\"type\":\"ArrayExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "(a, b)" [Expr (Tuple [Var "a",Var "b"])] (
          "[a, b]", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"elements\":[{\"name\":\"a\",\"type\":\"Identifier\"},{\"name\":\"b\",\"type\":\"Identifier\"}],\"type\":\"ArrayExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "f (a, b)" [Expr (BinOp "" (Var "f") (Tuple [Var "a",Var "b"]))] (
          "f(a, b)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"a\",\"type\":\"Identifier\"},{\"name\":\"b\",\"type\":\"Identifier\"}],\"callee\":{\"name\":\"f\",\"type\":\"Identifier\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "(a, b) |> f" [Expr (BinOp "|>" (Tuple [Var "a",Var "b"]) (Var "f"))] (
          "f(a, b)", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"arguments\":[{\"name\":\"a\",\"type\":\"Identifier\"},{\"name\":\"b\",\"type\":\"Identifier\"}],\"callee\":{\"name\":\"f\",\"type\":\"Identifier\"},\"type\":\"CallExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "list" $ do
      test "[]" [Expr (List [])] (
          "[]", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"elements\":[],\"type\":\"ArrayExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "[a]" [Expr (List [Var "a"])] (
          "[a]", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"elements\":[{\"name\":\"a\",\"type\":\"Identifier\"}],\"type\":\"ArrayExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "[a, b]" [Expr (List [Var "a",Var "b"])] (
          "[a, b]", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"elements\":[{\"name\":\"a\",\"type\":\"Identifier\"},{\"name\":\"b\",\"type\":\"Identifier\"}],\"type\":\"ArrayExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "string" $ do
      test "\"abc\"" [Expr (Str "abc")] (
          "'abc'", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"raw\":\"'abc'\",\"value\":\"abc\",\"type\":\"Literal\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "member" $ do
      test "a.b" [Expr (BinOp "." (Var "a") (Var "b"))] (
          "a.b", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"property\":{\"name\":\"b\",\"type\":\"Identifier\"},\"computed\":false,\"object\":{\"name\":\"a\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a.3" [Expr (BinOp "." (Var "a") (Natural 3))] (
          "a[3]", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"property\":{\"raw\":\"3\",\"value\":3,\"type\":\"Literal\"},\"computed\":true,\"object\":{\"name\":\"a\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a.(3 + 2)" [Expr (BinOp "." (Var "a") (BinOp "+" (Natural 3) (Natural 2)))] (
          "a[3 + 2]", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"property\":{\"operator\":\"+\",\"left\":{\"raw\":\"3\",\"value\":3,\"type\":\"Literal\"},\"right\":{\"raw\":\"2\",\"value\":2,\"type\":\"Literal\"},\"type\":\"BinaryExpression\"},\"computed\":true,\"object\":{\"name\":\"a\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "a.3 + 2" [Expr (BinOp "+" (BinOp "." (Var "a") (Natural 3)) (Natural 2))] (
          "a[3] + 2", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"+\",\"left\":{\"property\":{\"raw\":\"3\",\"value\":3,\"type\":\"Literal\"},\"computed\":true,\"object\":{\"name\":\"a\",\"type\":\"Identifier\"},\"type\":\"MemberExpression\"},\"right\":{\"raw\":\"2\",\"value\":2,\"type\":\"Literal\"},\"type\":\"BinaryExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "function" $ do
      test "\\x -> x" [Expr (Fun "x" (Var "x"))] (
          "x => x", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"defaults\":[],\"body\":{\"name\":\"x\",\"type\":\"Identifier\"},\"params\":[{\"name\":\"x\",\"type\":\"Identifier\"}],\"expression\":true,\"id\":null,\"type\":\"ArrowFunctionExpression\",\"generator\":false},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
      test "\\x y -> x" [Expr (Fun "x" (Fun "y" (Var "x")))] (
          "x => y => x", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"defaults\":[],\"body\":{\"defaults\":[],\"body\":{\"name\":\"x\",\"type\":\"Identifier\"},\"params\":[{\"name\":\"y\",\"type\":\"Identifier\"}],\"expression\":true,\"id\":null,\"type\":\"ArrowFunctionExpression\",\"generator\":false},\"params\":[{\"name\":\"x\",\"type\":\"Identifier\"}],\"expression\":true,\"id\":null,\"type\":\"ArrowFunctionExpression\",\"generator\":false},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
    describe "if-then-else" $ do
      test "if a then b else c" [Expr (If (Var "a") (Var "b") (Var "c"))] (
          "a ? b : c", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"consequent\":{\"name\":\"b\",\"type\":\"Identifier\"},\"test\":{\"name\":\"a\",\"type\":\"Identifier\"},\"alternate\":{\"name\":\"c\",\"type\":\"Identifier\"},\"type\":\"ConditionalExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
  describe "statement" $ do
    describe "declaration" $ do
      test "a = 3" [Decl (Var "a") (Natural 3)] (
          "var a = 3", "{\"sourceType\":\"script\",\"body\":[{\"kind\":\"var\",\"type\":\"VariableDeclaration\",\"declarations\":[{\"init\":{\"raw\":\"3\",\"value\":3,\"type\":\"Literal\"},\"id\":{\"name\":\"a\",\"type\":\"Identifier\"},\"type\":\"VariableDeclarator\"}]}],\"type\":\"Program\"}"
        )
    describe "assignment" $ do
      test "a #= 3" [Assign (Var "a") (Natural 3)] (
          "a = 3", "{\"sourceType\":\"script\",\"body\":[{\"expression\":{\"operator\":\"=\",\"left\":{\"name\":\"a\",\"type\":\"Identifier\"},\"right\":{\"raw\":\"3\",\"value\":3,\"type\":\"Literal\"},\"type\":\"AssignmentExpression\"},\"type\":\"ExpressionStatement\"}],\"type\":\"Program\"}"
        )
