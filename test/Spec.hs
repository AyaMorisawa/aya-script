import Test.Hspec
import Lib (compile)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "application" $ do
    context "f x" $ do
      it "compiles to f(x)" $
        compile "f x" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}}],\"sourceType\":\"script\"}"
    context "f a b" $ do
      it "compiles to f(a)(b)" $
        compile "f a b" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"a\"}]},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
  describe "piping application" $ do
    context "x |> f" $ do
      it "compiles to f(x)" $
        compile "f x" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}}],\"sourceType\":\"script\"}"
    context "x |> f |> g" $ do
      it "compiles to g(f(x))" $
        compile "x |> f |> g" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"g\"},\"arguments\":[{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"x\"}]}]}}],\"sourceType\":\"script\"}"
  describe "tuple" $ do
    context "(a, b)" $ do
      it "compiles to [a, b]" $
        compile "[a, b]" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"ArrayExpression\",\"elements\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
    context "f (a, b)" $ do
      it "compiles to f(a, b)" $
        compile "f (a, b)" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"CallExpression\",\"callee\":{\"type\":\"Identifier\",\"name\":\"f\"},\"arguments\":[{\"type\":\"Identifier\",\"name\":\"a\"},{\"type\":\"Identifier\",\"name\":\"b\"}]}}],\"sourceType\":\"script\"}"
  describe "member" $ do
    context "a.b" $ do
      it "compiles to a.b" $
        compile "a.b" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"MemberExpression\",\"computed\":false,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"Identifier\",\"name\":\"b\"}}}],\"sourceType\":\"script\"}"
    context "a.3" $ do
      it "compiles to a[3]" $
        compile "a.3" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"MemberExpression\",\"computed\":true,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"}}}],\"sourceType\":\"script\"}"
    context "a.(3 + 2)" $ do
      it "compiles to a[3 + 2]" $
        compile "a.(3 + 2)" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"MemberExpression\",\"computed\":true,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"},\"right\":{\"type\":\"Literal\",\"value\":2,\"raw\":\"2\"}}}}],\"sourceType\":\"script\"}"
    context "a.3 + 2" $ do
      it "compiles to a[3] + 2" $
        compile "a.3 + 2" `shouldBe` Right "{\"type\":\"Program\",\"body\":[{\"type\":\"ExpressionStatement\",\"expression\":{\"type\":\"BinaryExpression\",\"operator\":\"+\",\"left\":{\"type\":\"MemberExpression\",\"computed\":true,\"object\":{\"type\":\"Identifier\",\"name\":\"a\"},\"property\":{\"type\":\"Literal\",\"value\":3,\"raw\":\"3\"}},\"right\":{\"type\":\"Literal\",\"value\":2,\"raw\":\"2\"}}}],\"sourceType\":\"script\"}"
