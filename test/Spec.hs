import Test.Hspec
import AyaScript

main :: IO ()
main = hspec spec

test :: String -> Program -> (String, String) -> SpecWith ()
test code asAst (es, esAst) =
  context code $ do
    it ("parses to " ++ showAsAst asAst) $ ayaScriptParser code `shouldBe` Right asAst
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
          "a + b", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Identifier\",\"name\":\"a\"},\"right\":{\"type\":\"Identifier\",\"name\":\"b\"}}}],\"sourceType\":\"script\"}"
        )
      test "a - b" [Expr (BinOp "-" (Var "a") (Var "b"))] (
          "a - b", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"-\",\"left\":{\"type\":\"Identifier\",\"name\":\"a\"},\"right\":{\"type\":\"Identifier\",\"name\":\"b\"}}}],\"sourceType\":\"script\"}"
        )
      test "a * b" [Expr (BinOp "*" (Var "a") (Var "b"))] (
          "a * b", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"*\",\"left\":{\"type\":\"Identifier\",\"name\":\"a\"},\"right\":{\"type\":\"Identifier\",\"name\":\"b\"}}}],\"sourceType\":\"script\"}"
        )
      test "a / b" [Expr (BinOp "/" (Var "a") (Var "b"))] (
          "a / b", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"/\",\"left\":{\"type\":\"Identifier\",\"name\":\"a\"},\"right\":{\"type\":\"Identifier\",\"name\":\"b\"}}}],\"sourceType\":\"script\"}"
        )
      test "a + (b * c)" [Expr (BinOp "+" (Var "a") (BinOp "*" (Var "b") (Var "c")))] (
          "a + b * c", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Identifier\",\"name\":\"a\"},\"right\":{\"type\":\"BinaryExpression\",\"operator\":\"*\",\"left\":{\"type\":\"Identifier\",\"name\":\"b\"},\"right\":{\"type\":\"Identifier\",\"name\":\"c\"}}}}],\"sourceType\":\"script\"}"
        )
      test "a * (b + c)" [Expr (BinOp "*" (Var "a") (BinOp "+" (Var "b") (Var "c")))] (
          "a * (b + c)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"*\",\"left\":{\"type\":\"Identifier\",\"name\":\"a\"},\"right\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Identifier\",\"name\":\"b\"},\"right\":{\"type\":\"Identifier\",\"name\":\"c\"}}}}],\"sourceType\":\"script\"}"
        )
    describe "mapping-operator" $ do
      test "f <$> x" [Expr (App (BinOp "." (Var "x") (Var "map")) (Var "f"))] (
          "x.map(f)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"MemberExpression\",\"computed\":false,\"object\":{\"type\":\"Identifier\",\"name\":\"x\"},\"property\":{\"type\":\"Identifier\",\"name\":\"map\"}},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"f\"}]}}],\"sourceType\":\"script\"}"
        )
    describe "application" $ do
      test "f x" [Expr (App (Var "f") (Var "x"))] (
          "f(x)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}}],\"sourceType\":\"script\"}"
        )
      test "f a b" [Expr (App (App (Var "f") (Var "a")) (Var "b"))] (
          "f(a)(b)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"a\"}]},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
        )
    describe "piping-operator" $ do
      test "x |> f" [Expr (App (Var "f") (Var "x"))] (
          "f(x)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}}],\"sourceType\":\"script\"}"
        )
      test "x |> f |> g" [Expr (App (Var "g") (App (Var "f") (Var "x")))] (
          "g(f(x))", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"g\"},\"arguments\":[{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}]}}],\"sourceType\":\"script\"}"
        )
    describe "tuple" $ do
      test "()" [Expr (Tuple [])] (
          "[]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrayExpression\",\"elements\":[]}}],\"sourceType\":\"script\"}"
        )
      test "(a, b)" [Expr (Tuple [Var "a",Var "b"])] (
          "[a, b]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrayExpression\",\"elements\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
        )
      test "f (a, b)" [Expr (App (Var "f") (Tuple [Var "a",Var "b"]))] (
          "f(a, b)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
        )
      test "(a, b) |> f" [Expr (App (Var "f") (Tuple [Var "a",Var "b"]))] (
          "f(a, b)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
        )
    describe "list" $ do
      test "[]" [Expr (List [])] (
          "[]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrayExpression\",\"elements\":[]}}],\"sourceType\":\"script\"}"
        )
      test "[a]" [Expr (List [Var "a"])] (
          "[a]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrayExpression\",\"elements\":[{\"type\":\"Identifier\",\"name\":\"a\"}]}}],\"sourceType\":\"script\"}"
        )
      test "[a, b]" [Expr (List [Var "a",Var "b"])] (
          "[a, b]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrayExpression\",\"elements\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
        )
    describe "member" $ do
      test "a.b" [Expr (BinOp "." (Var "a") (Var "b"))] (
          "a.b", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"MemberExpression\",\"computed\":false,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"Identifier\",\"name\":\"b\"}}}],\"sourceType\":\"script\"}"
        )
      test "a.3" [Expr (BinOp "." (Var "a") (Natural 3))] (
          "a[3]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"MemberExpression\",\"computed\":true,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"}}}],\"sourceType\":\"script\"}"
        )
      test "a.(3 + 2)" [Expr (BinOp "." (Var "a") (BinOp "+" (Natural 3) (Natural 2)))] (
          "a[3 + 2]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"MemberExpression\",\"computed\":true,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"},\"right\":{\"type\":\"Literal\",\"value\":2,\"raw\":\"2\"}}}}],\"sourceType\":\"script\"}"
        )
      test "a.3 + 2" [Expr (BinOp "+" (BinOp "." (Var "a") (Natural 3)) (Natural 2))] (
          "a[3] + 2", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"MemberExpression\",\"computed\":true,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"}},\"right\":{\"type\":\"Literal\",\"value\":2,\"raw\":\"2\"}}}],\"sourceType\":\"script\"}"
        )
    describe "function" $ do
      test "\\x -> x" [Expr (Fun "x" (Var "x"))] (
          "x => x", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrowFunctionExpression\",\"id\":null,\"params\":[{\"type\":\"Identifier\",\"name\":\"x\"}],\"defaults\":[],\"body\":{\"type\":\"Identifier\",\"name\":\"x\"},\"generator\":false,\"expression\":true}}],\"sourceType\":\"script\"}"
        )
    describe "if-then-else" $ do
      test "if a then b else c" [Expr (If (Var "a") (Var "b") (Var "c"))] (
          "a ? b : c", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ConditionalExpression\",\"test\":{\"type\":\"Identifier\",\"name\":\"a\"},\"consequent\":{\"type\":\"Identifier\",\"name\":\"b\"},\"alternate\":{\"type\":\"Identifier\",\"name\":\"c\"}}}],\"sourceType\":\"script\"}"
        )
  describe "statement" $ do
    describe "declaration" $ do
      test "a = 3" [Decl (Var "a") (Natural 3)] (
          "var a = 3", "{\"type\":\"Program\",\"body\":[{\"type\":\"VariableDeclaration\",\"declarations\":[{\"type\":\"VariableDeclarator\",\"id\":{\"type\":\"Identifier\",\"name\":\"a\"},\"init\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"}}],\"kind\":\"var\"}],\"sourceType\":\"script\"}"
        )
    describe "assignment" $ do
      test "a #= 3" [Assign (Var "a") (Natural 3)] (
          "a = 3", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"AssignmentExpression\",\"operator\":\"=\",\"left\":{\"type\":\"Identifier\",\"name\":\"a\"},\"right\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"}}}],\"sourceType\":\"script\"}"
        )
