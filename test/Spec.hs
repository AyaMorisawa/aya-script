import Test.Hspec
import Lib

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
  describe "application" $ do
    test "f x" [Expr (App (Var "f") (Var "x"))] (
        "f(x)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}}],\"sourceType\":\"script\"}"
      )
    test "f a b" [Expr (App (App (Var "f") (Var "a")) (Var "b"))] (
        "f(a)(b)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"a\"}]},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
      )
  describe "piping application" $ do
    test "x |> f" [Expr (BinOp "|>" (Var "x") (Var "f"))] (
        "f(x)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}}],\"sourceType\":\"script\"}"
      )
    test "x |> f |> g" [Expr (BinOp "|>" (BinOp "|>" (Var "x") (Var "f")) (Var "g"))] (
        "g(f(x))", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"g\"},\"arguments\":[{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}]}}],\"sourceType\":\"script\"}"
      )
  describe "tuple" $ do
    test "(a, b)" [Expr (Tuple [Var "a",Var "b"])] (
        "[a, b]", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrayExpression\",\"elements\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
      )
    test "f (a, b)" [Expr (App (Var "f") (Tuple [Var "a",Var "b"]))] (
        "f(a, b)", "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
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
