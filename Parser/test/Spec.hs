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
            let f = Func (Comp [S "test",V "X",S "and",V "Y"]) (Constraints []) (Expr (Comp [I 1,S "+",I 2]))
            prettyPrint f `shouldBe` "def(f(comp(s(\"test\") v(\"X\") s(\"and\") v(\"Y\")),cs(nil),e(comp(i(1) s(\"+\") i(2)))))"

    describe "func" $ do
        it "parses function declarations" $ do
            let (Right v) = parse func "" "test X and Y is 1 + 2."
            v `shouldBe` Func (Comp [S "test",V "X",S "and",V "Y"]) (Constraints []) (Expr (Comp [I 1,S "+",I 2]))
        it "parses functions with constraints" $ do
            let (Right v) = parse func "" "test X and Y is X = Y, X + 2."
            v `shouldBe` Func (Comp [S "test",V "X",S "and",V "Y"]) (Constraints [Constraint (Comp [V "X",S "=",V "Y"])]) (Expr (Comp [V "X",S "+",I 2]))

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
            let (Right v) = parse (str []) "" "hello"
            v `shouldBe` S "hello"
        it "does not parse strings ending in a ':'" $
            isLeft (parse (str ["is"]) "" "is") `shouldBe` True

    describe "int" $ do
        it "parses integers" $ do
            let (Right v) = parse int "" "301"
            v `shouldBe` I 301
        it "parses negative integers" $ do
            let (Right v) = parse int "" "-31945812"
            v `shouldBe` I (-31945812)

    describe "enkiId" $ do
        it "parses a single expression" $ do
            let (Right v) = parse (enkiId []) "" "test"
            v `shouldBe` Comp [S "test"]
        it "parses multiple expressions" $ do
            let (Right v) = parse (enkiId []) "" "t y false B"
            v `shouldBe` Comp [S "t", S "y", B False, V "B"]
        it "parses nested expressions" $ do
            let (Right v) = parse (enkiId []) "" "(1 + 2) + 4"
            v `shouldBe` Comp [Comp [I 1, S "+",I 2], S "+", I 4]
        it "parses up to an excluded identifier" $ do
            let (Right v) = parse (enkiId ["is"]) "" "factorial X is"
            v `shouldBe` Comp [S "factorial", V "X"]

    describe "enkiType" $ do
        it "parses int" $ do
            let (Right v) = parse enkiType "" "int"
            v `shouldBe` EnkiInt
        it "parses string" $ do
            let (Right v) = parse enkiType "" "string"
            v `shouldBe` EnkiString
        it "parses bool" $ do
            let (Right v) = parse enkiType "" "bool"
            v `shouldBe` EnkiBool
        it "parses any" $ do
            let (Right v) = parse enkiType "" "Test"
            v `shouldBe` Any "Test"
        it "parses function types" $ do
            let (Right v) = parse enkiType "" "T1 -> T2"
            v `shouldBe` FuncType (Any "T1") (Any "T2")
        it "parses function types with multiple parameters" $ do
            let (Right v) = parse enkiType "" "T1 -> T2 -> bool"
            v `shouldBe` FuncType (Any "T1") (FuncType (Any "T2") EnkiBool)
        it "parses types in parentheses" $ do
            let (Right v) = parse enkiType "" "(bool -> bool)"
            v `shouldBe` FuncType EnkiBool EnkiBool
        it "parses rule types" $ do
            let (Right v) = parse enkiType "" "string ~ T2"
            v `shouldBe` RuleType EnkiString (Any "T2")
        it "parses data types" $ do
            let (Right v) = parse enkiType "" "int * list"
            v `shouldBe` DataType EnkiInt (TypeName (Comp [S "list"]))
        it "parses parenthesized types at the front of a data type" $ do
            let (Right v) = parse enkiType "" "(int * int) * bool"
            v `shouldBe` DataType (DataType EnkiInt EnkiInt) EnkiBool

    describe "field" $ do
        it "parses a field" $ do
            let (Right v) = parse field "" "X : int"
            v `shouldBe` Field (Comp [V "X"]) EnkiInt
        it "parses a field with a more complex type" $ do
            let (Right v) = parse field "" "Y : int ~ int"
            v `shouldBe` Field (Comp [V "Y"]) (RuleType EnkiInt EnkiInt)

    describe "constructor" $ do
        it "parses a single constructor" $ do
            let (Right v) = parse constructor "" "pair X and Y has X : int, Y : int."
            v `shouldBe` Constructor (Comp [S "pair", V "X", S "and", V "Y"])
                            [Field (Comp [V "X"]) EnkiInt, Field (Comp [V "Y"]) EnkiInt]
        it "parses a constructor with fields of complicated types" $ do
            let (Right v) = parse constructor "" "stored value X for a function F has X : T, F : T -> T."
            v `shouldBe` Constructor (Comp [S "stored",S "value",V "X",S "for",S "a",S "function",V "F"])
                            [Field (Comp [V "X"]) (Any "T"),Field (Comp [V "F"]) (FuncType (Any "T") (Any "T"))]

    describe "dataDef" $ do
        it "parses the declaration of a new datatype" $ do
            let (Right v) = parse dataDef "" "pair may be pair of X and Y has X : int, Y : int."
            v `shouldBe` Data (Comp [S "pair"])
                            [Constructor (Comp [S "pair",S "of",V "X",S "and",V "Y"])
                                [Field (Comp [V "X"]) EnkiInt,Field (Comp [V "Y"]) EnkiInt]]
        it "parses types with multiple constructors" $ do
            let (Right v) = parse dataDef "" "list may be empty. cons Head Tail has Head : int, Tail : list."
            v `shouldBe` Data (Comp [S "list"])
                            [Constructor (Comp [S "empty"]) [],
                             Constructor (Comp [S "cons",V "Head",V "Tail"])
                                [Field (Comp [V "Head"]) EnkiInt,Field (Comp [V "Tail"]) (TypeName (Comp [S "list"]))]]

    describe "runParser" $ do
        testCompile "examples/basic.enki"
        testCompile "examples/recursive.enki"
        testCompile "examples/func_call.enki"
        testCompile "examples/many_func.enki"
        testCompile "examples/strings.enki"

        testCompile "examples/basic_rule.enki"
        testCompile "examples/complicated_rule.enki"
        testCompile "examples/collatz.enki"
        testCompile "examples/list.enki"

