import Control.Monad.IO.Class

import Data.Either
import Data.String.Utils

import Test.Hspec
import Text.Parsec

import Parser

testCompile fname = it fname $ do
    let outputFile = fname ++ "ast"

    liftIO $ parseFile fname $ Just outputFile

    output <- strip <$> readFile outputFile
    expectedOutput <- strip <$> readFile (outputFile ++ ".out")

    output `shouldBe` expectedOutput

main :: IO ()
main = hspec $ do
    describe "prettyPrint" $ do
        it "prints booleans (true)" $ prettyPrint (B True) `shouldBe` "b(true)"
        it "prints booleans (false)" $ prettyPrint (B False) `shouldBe` "b(false)"
        it "prints integers" $ prettyPrint (I 109) `shouldBe` "i(109)"
        it "prints strings" $ prettyPrint (S "+") `shouldBe` "s(\"+\")"
        it "prints variables" $ prettyPrint (V "X") `shouldBe` "v(\"X\")"
        it "prints expressions" $ do
            let e = Expr $ Comp [I 1, S "+",I 2]
            prettyPrint e `shouldBe` "e(comp(i(1) s(\"+\") i(2)))"
        it "prints functions" $ do
            let f = Func (Comp [S "test",V "X",S "and",V "Y"]) (Constraint []) (Expr (Comp [I 1,S "+",I 2]))
            prettyPrint f `shouldBe` "def(f(comp(s(\"test\") v(\"X\") s(\"and\") v(\"Y\")),cs(nil),e(comp(i(1) s(\"+\") i(2)))))"

    describe "func" $ do
        it "parses function declarations" $ do
            let (Right v) = parse func "" "test X and Y is: 1 + 2."
            v `shouldBe` Func (Comp [S "test",V "X",S "and",V "Y"]) (Constraint []) (Expr (Comp [I 1,S "+",I 2]))
        it "parses functions with constraints" $ do
            let (Right v) = parse func "" "test X and Y is: X = Y, X + 2."
            v `shouldBe` Func (Comp [S "test",V "X",S "and",V "Y"]) (Constraint [Comp [V "X",S "=",V "Y"]]) (Expr (Comp [V "X",S "+",I 2]))

    describe "expr" $ do
        it "parses arithmetic" $ do
            let (Right v) = parse expr "" "1 + 2"
            v `shouldBe` Expr (Comp [I 1, S "+", I 2])
        it "parses nested arithmetic" $ do
            let (Right v) = parse expr "" "(3 * 4) + (9 / 3)"
            v `shouldBe` Expr (Comp [Comp[I 3, S "*", I 4], S "+", Comp[I 9, S "/", I 3]])

    describe "bool" $ do
        it "parses booleans (true)" $ do
            let (Right v) = parse bool "" "true"
            v `shouldBe` B True
        it "parses booleans (false)" $ do
            let (Right v) = parse bool "" "false"
            v `shouldBe` B False

    describe "str" $ do
        it "parses a single string" $ do
            let (Right v) = parse str "" "hello"
            v `shouldBe` S "hello"
        it "does not parse strings ending in a ':'" $
            isLeft (parse str "" "is:") `shouldBe` True

    describe "int" $ do
        it "parses integers" $ do
            let (Right v) = parse int "" "301"
            v `shouldBe` I 301
        it "parses negative integers" $ do
            let (Right v) = parse int "" "-31945812"
            v `shouldBe` I (-31945812)

    describe "enkiId" $ do
        it "parses a single expression" $ do
            let (Right v) = parse enkiId "" "test"
            v `shouldBe` Comp [S "test"]
        it "parses multiple expressions" $ do
            let (Right v) = parse enkiId "" "t y false B"
            v `shouldBe` Comp [S "t", S "y", B False, V "B"]
        it "parses nested expressions" $ do
            let (Right v) = parse enkiId "" "(1 + 2) + 4"
            v `shouldBe` Comp [Comp [I 1, S "+",I 2], S "+", I 4]
        it "does not parse strings ending in a ':'" $ do
            let (Right v) = parse enkiId "" "is:"
            v `shouldBe` Comp []
        it "parses up to an identifier ending in a color" $ do
            let (Right v) = parse enkiId "" "factorial X is:"
            v `shouldBe` Comp [S "factorial", V "X"]

    describe "runParser" $ do
        testCompile "examples/basic.enki"
        testCompile "examples/recursive.enki"
        testCompile "examples/func_call.enki"
        testCompile "examples/many_func.enki"
        testCompile "examples/strings.enki"

        testCompile "examples/basic_rule.enki"

