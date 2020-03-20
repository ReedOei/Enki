{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils

import Test.Hspec
import Text.Parsec

import System.Process
import System.TimeIt

import Enki.Types
import Enki.Parser.AST
import Enki.Parser.Parser

import Enki.Compiler
import Enki.Compiler.Types
import Enki.Compiler.TypeChecker

checkFile fname expected = it fname $ do
    executableName <- generateExecutable fname (fname ++ ".out")
    actual <- readProcess executableName [] ""
    actual `shouldBe` expected

inferDef :: String -> IO (TypedDef, Environment)
inferDef str = do
    res <- parseDef str
    let (Right [parsed]) = res
    runStateT (infer parsed) newEnv

inferDefs :: String -> IO (TypedDef, Environment)
inferDefs str = do
    res <- parseDef str
    let (Right parsed) = res
    (res,env) <- runStateT (infer parsed) newEnv
    pure (last res, env)

main :: IO ()
main = hspec $ do
    describe "unifyIds" $ do
        it "grabs arguments from function calls" $ do
            let res = unifyIds (Comp [S "add", I 1, S "to", I 2]) (Comp [S "add", V "X", S "to", V "Y"])
            res `shouldBe` Just (Map.fromList [("X",I 1),("Y",I 2)])
        it "does not match if any part fails" $ do
            let res = unifyIds (Comp [S "add", I 1, S "to", I 2]) (Comp [S "add", V "X", S "nope", V "Y"])
            res `shouldBe` Nothing

    describe "infer (TypedId)" $ do
        it "infers the type of basic ids (int)" $ do
            let res = evalState (infer (I 1)) newEnv
            res `shouldBe` IntVal 1
        it "infers the type of basic ids (string)" $ do
            let res = evalState (infer (S "testing")) newEnv
            res `shouldBe` StringVal "testing"
        it "infers the type of basic ids (var)" $ do
            let (res, env) = runState (infer (V "X")) newEnv
            res `shouldBe` VarVal "X"
            Map.lookup "X" (env^.typeEnv) `shouldBe` Just (Any "T0")

    describe "freshDefType" $ do
        it "replaces all type variables with fresh types in functions" $ do
            let cons = TypedConstructor (Comp [S "pair",S "of",V "X",S "and",V "Y"]) (DataType (Any "A") (DataType (Any "B") (TypeName [Named "pair",Any "A",Any "B"])))
            let res = evalState (freshDefType cons) newEnv
            res `shouldBe` TypedConstructor (Comp [S "pair",S "of",V "X",S "and",V "Y"]) (DataType (Any "T0") (DataType (Any "T1") (TypeName [Named "pair",Any "T0",Any "T1"])))

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

    describe "str" $ do
        it "parses a single string" $ do
            let (Right v) = parse (str "" []) "" "hello"
            v `shouldBe` S "hello"
        it "does not parse strings ending in a ':'" $
            isLeft (parse (str "" ["is"]) "" "is") `shouldBe` True

    describe "int" $ do
        it "parses integers" $ do
            let (Right v) = parse int "" "301"
            v `shouldBe` I 301

    describe "listLiteral" $ do
        it "parses empty lists" $ do
            let v = parse (listLiteral [] []) "" "[]"
            v `shouldBe` Right (S "empty")

        it "parses lists" $ do
            let v = parse (listLiteral [] []) "" "[1,2,3]"
            v `shouldBe` Right (Comp [S "cons", I 1, Comp [S "cons", I 2,Comp [S "cons",I 3,S "empty"]]])

        it "parses nested lists" $ do
            let v = parse (listLiteral [] []) "" "[[f 1 and 3,2,3],2,3]"
            v `shouldBe` Right (Comp [S "cons",Comp [S "cons",Comp [S "f",I 1,S "and",I 3],Comp [S "cons",I 2,Comp [S "cons",I 3,S "empty"]]],Comp [S "cons",I 2,Comp [S "cons",I 3,S "empty"]]])

    describe "enkiId" $ do
        it "parses a single expression" $ do
            let (Right v) = parse (enkiId "" []) "" "test"
            v `shouldBe` S "test"
        it "parses multiple expressions" $ do
            let (Right v) = parse (enkiId "" []) "" "t y false B"
            -- Note that "false" gets automatically sanitized to "enki_false" for Prolog compatibility
            v `shouldBe` Comp [S "t", S "y", S "enki_false", V "B"]
        it "parses nested expressions" $ do
            let (Right v) = parse (enkiId "" []) "" "(1 + 2) + 4"
            v `shouldBe` Comp [Comp [I 1, S "+",I 2], S "+", I 4]
        it "parses up to an excluded identifier" $ do
            let (Right v) = parse (enkiId "" ["is"]) "" "factorial X is"
            v `shouldBe` Comp [S "factorial", V "X"]

    describe "enkiType" $ do
        it "parses int" $ do
            let (Right v) = parse enkiType "" "int"
            v `shouldBe` EnkiInt
        it "parses string" $ do
            let (Right v) = parse enkiType "" "string"
            v `shouldBe` EnkiString
        it "parses any" $ do
            let (Right v) = parse enkiType "" "Test"
            v `shouldBe` Any "Test"
        it "parses function types" $ do
            let (Right v) = parse enkiType "" "T1 -> T2"
            v `shouldBe` FuncType (Any "T1") (Any "T2")
        it "parses function types with multiple parameters" $ do
            let (Right v) = parse enkiType "" "T1 -> T2 -> int"
            v `shouldBe` FuncType (Any "T1") (FuncType (Any "T2") EnkiInt)
        it "parses types in parentheses" $ do
            let (Right v) = parse enkiType "" "(int -> string)"
            v `shouldBe` FuncType EnkiInt EnkiString
        it "parses rule types" $ do
            let (Right v) = parse enkiType "" "string ~ T2"
            v `shouldBe` RuleType EnkiString (Any "T2")
        it "parses data types" $ do
            let (Right v) = parse enkiType "" "int * list"
            v `shouldBe` DataType EnkiInt (Named "list")
        it "parses parenthesized types at the front of a data type" $ do
            let (Right v) = parse enkiType "" "(int * int) * string"
            v `shouldBe` DataType (DataType EnkiInt EnkiInt) EnkiString

    describe "field" $ do
        it "parses a field" $ do
            let (Right v) = parse field "" "X : int"
            v `shouldBe` Field (V "X") EnkiInt
        it "parses a field with a more complex type" $ do
            let (Right v) = parse field "" "Y : int ~ int"
            v `shouldBe` Field (V "Y") (RuleType EnkiInt EnkiInt)

    describe "constructor" $ do
        it "parses a single constructor" $ do
            let (Right v) = parse constructor "" "pair X and Y has X : int, Y : int."
            v `shouldBe` Constructor (Comp [S "pair", V "X", S "and", V "Y"])
                            [Field (V "X") EnkiInt, Field (V "Y") EnkiInt]
        it "parses a constructor with fields of complicated types" $ do
            let (Right v) = parse constructor "" "stored value X for a function F has X : T, F : T -> T."
            v `shouldBe` Constructor (Comp [S "stored",S "value",V "X",S "for",S "a",S "function",V "F"])
                            [Field (V "X") (Any "T"),Field (V "F") (FuncType (Any "T") (Any "T"))]

    describe "dataDef" $ do
        it "parses the declaration of a new datatype" $ do
            let (Right v) = parse dataDef "" "pair may be pair of X and Y has X : int, Y : int."
            v `shouldBe` Data (S "pair")
                            [Constructor (Comp [S "pair",S "of",V "X",S "and",V "Y"])
                                [Field (V "X") EnkiInt,Field (V "Y") EnkiInt]]
        it "parses types with multiple constructors" $ do
            let (Right v) = parse dataDef "" "list may be empty | cons Head Tail has Head : int, Tail : list."
            v `shouldBe` Data (S "list")
                            [Constructor (S "empty") [],
                             Constructor (Comp [S "cons",V "Head",V "Tail"])
                                [Field (V "Head") EnkiInt,Field (V "Tail") (Named "list")]]

    describe "aliases" $ do
        it "performs arbitrary syntax transformations" $ do
            v <- parseDef "define alias square X as X * X. test X is square X."
            v `shouldBe` Right [Func (Comp [S "test",V "X"]) (Constraints []) (Expr (Comp [V "X",S "*",V "X"]))]

        it "performs type substitutions" $ do
            v <- parseDef "define alias self pair X as pair X X. temp X may be con A B has A : self pair X, B : self pair X."
            v `shouldBe` Right [Data (Comp [S "temp",V "X"]) [Constructor (Comp [S "con",V "A",V "B"]) [Field (V "A") (TypeName [Named "pair",Any "X",Any "X"]),Field (V "B") (TypeName [Named "pair",Any "X",Any "X"])]]]

    describe "execute" $ do
        checkFile "examples/pe1.enki" "233168\n"
        checkFile "examples/pe2.enki" "4613732\n"
        checkFile "examples/pe3.enki" "6857\n"
        checkFile "examples/pe5.enki" "232792560\n"
        checkFile "examples/pe6.enki" "25164150\n"
        checkFile "examples/nested-when.enki" "negative\nblah\nat least 5\nmore than 7\nmore than 10\n"
        checkFile "examples/map-test.enki" "65\n"
        checkFile "examples/call-test.enki" "11\n"
        checkFile "examples/filter-test.enki" "30\n"
        checkFile "examples/defref.enki" "24\n2\n[5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]\n"
        checkFile "examples/string-test.enki" "[t,e,s,t,i,n,g,' ',t,h,i,n,g,' ',t,h,i,n,g,' ',o,u,t,' ',o,n,' ',a,' ',l,o,n,g,e,r,' ',s,t,r,i,n,g]\narghesarghing arghhing arghhing ouargh on a longer sarghring\n"
        checkFile "examples/test_backquote.enki" "0\n"
        checkFile "examples/test_single_calls.enki" "1\n2\n"
        checkFile "examples/test_not.enki" "2\n1\n1\n2\n"

