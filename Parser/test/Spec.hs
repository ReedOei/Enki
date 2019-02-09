import Control.Monad.IO.Class

import Data.String.Utils

import Test.Hspec
import Text.Parsec

import Parser

testCompile fname = do
    let outputFile = fname ++ "ast"
    expectedOutput <- strip <$> readFile (outputFile ++ ".out")

    liftIO $ parseFile fname outputFile

    output <- strip <$> readFile outputFile

    output `shouldBe` expectedOutput

main :: IO ()
main = hspec $ do
    describe "prettyPrint" $ do
        it "prints booleans (true)" $ prettyPrint (B True) `shouldBe` "b(true)"
        it "prints booleans (false)" $ prettyPrint (B False) `shouldBe` "b(false)"
        it "prints integers" $ prettyPrint (I 109) `shouldBe` "i(109)"
        it "prints strings" $ prettyPrint (S "+") `shouldBe` "s(\"+\")"
        it "prints variables" $ prettyPrint (V "X") `shouldBe` "v(\"X\")"
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
    describe "runParser" $ do
        it "runs the parser and writes a file" $ testCompile "examples/basic.enki"

