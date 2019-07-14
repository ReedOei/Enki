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

import Enki.Types
import Enki.Parser.AST
import Enki.Parser.Parser

import Enki.Compiler
import Enki.Compiler.Types
import Enki.Compiler.TypeChecker

runFile fname expected = it fname $ do
    source <- compile fname
    let outputName = fname ++ "_out.pl"
    writeFile outputName source
    actual <- readProcess "swipl" [outputName] ""
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
            res `shouldBe` Just (Map.fromList [("X",I 1),("Y",Comp [I 2])])
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
        it "infers the type of basic ids (bool)" $ do
            let res = evalState (infer (B False)) newEnv
            res `shouldBe` BoolVal False
        it "infers the type of basic ids (var)" $ do
            let (res, env) = runState (infer (V "X")) newEnv
            res `shouldBe` VarVal "X"
            Map.lookup "X" (env^.typeEnv) `shouldBe` Just (Any "T0")

    describe "freshDefType" $ do
        it "replaces all type variables with fresh types in functions" $ do
            let cons = TypedConstructor (Comp [S "pair",S "of",V "X",S "and",V "Y"]) (DataType (Any "A") (DataType (Any "B") (TypeName [Named "pair",Any "A",Any "B"])))
            let res = evalState (freshDefType cons) newEnv
            res `shouldBe` TypedConstructor (Comp [S "pair",S "of",V "X",S "and",V "Y"]) (DataType (Any "T0") (DataType (Any "T1") (TypeName [Named "pair",Any "T0",Any "T1"])))

    describe "infer (TypedDef)" $ do
        it "infers the type of functions (simple)" $ do
            (res,_) <- inferDef "add X to Y is Y."
            res `shouldBe` TypedFunc (Comp [S "add",V "X",S "to",V "Y"]) (FuncType (Any "T0") (FuncType (Any "T1") (Any "T1"))) (TypedConstraints []) (TypedExpr {exprId = VarVal "Y"})
        it "infers the type of functions (with constraints)" $ do
            (res,_) <- inferDef "add X to Y is X = Y, Y."
            res `shouldBe` TypedFunc (Comp [S "add",V "X",S "to",V "Y"]) (FuncType (Any "T3") (FuncType (Any "T3") (Any "T3"))) (TypedConstraints [TypedConstraint (BinOp "=" Void (VarVal "X") (VarVal "Y"))]) (TypedExpr {exprId = VarVal "Y"})
        it "infers the type of functions (int operators)" $ do
            (res,_) <- inferDef "add X to Y is X = Y, Y."
            res `shouldBe` TypedFunc (Comp [S "add",V "X",S "to",V "Y"]) (FuncType (Any "T3") (FuncType (Any "T3") (Any "T3"))) (TypedConstraints [TypedConstraint (BinOp "=" Void (VarVal "X") (VarVal "Y"))]) (TypedExpr {exprId = VarVal "Y"})
        it "infers the type of functions (string operators)" $ do
            (res,_) <- inferDef "concat X with Y is X .. Y."
            res `shouldBe` TypedFunc (Comp [S "concat",V "X",S "with",V "Y"]) (FuncType EnkiString (FuncType EnkiString EnkiString)) (TypedConstraints []) (TypedExpr {exprId = FuncCall (TypedFunc (Comp [S "atom_concat",V "X",V "Y"]) (FuncType EnkiString (FuncType EnkiString EnkiString)) (TypedConstraints []) (TypedExpr {exprId = StringVal "dummy value"})) (Map.fromList [("X",VarVal "X"),("Y",VarVal "Y")])})

        it "infers the type of functions (simple function callS)" $ do
            (res, _) <- inferDefs "inc X is X + 1.\ninc X twice is inc (inc X)."
            res `shouldBe` TypedFunc (Comp [S "inc",V "X",S "twice"]) (FuncType EnkiInt EnkiInt) (TypedConstraints []) (TypedExpr {exprId = FuncCall (TypedFunc (Comp [S "inc",V "X"]) (FuncType EnkiInt EnkiInt) (TypedConstraints []) (TypedExpr {exprId = BinOp "+" EnkiInt (VarVal "X") (IntVal 1)})) (Map.fromList [("X",FuncCall (TypedFunc (Comp [S "inc",V "X"]) (FuncType EnkiInt EnkiInt) (TypedConstraints []) (TypedExpr {exprId = BinOp "+" EnkiInt (VarVal "X") (IntVal 1)})) (Map.fromList [("X",VarVal "X")]))])})

        it "infers the type of functions (using data constructors)" $ do
            res <- runInfer "box may be containing X has X : int. put X in a box is containing X."
            res `shouldBe` [TypedData (Comp [S "box"]) [TypedConstructor (Comp [S "containing",V "X"]) (DataType EnkiInt (TypeName [Named "box"]))],TypedFunc (Comp [S "put",V "X",S "in",S "a",S "box"]) (FuncType EnkiInt (TypeName [Named "box"])) (TypedConstraints []) (TypedExpr {exprId = FuncCall (TypedConstructor (Comp [S "containing",V "X"]) (DataType EnkiInt (TypeName [Named "box"]))) (Map.fromList [("X",VarVal "X")])})]

        it "infers the type of functions (using nested data constructors and type variables)" $ do
            res <- runInfer "pair A B may be pair of X and Y has X : A, Y : B. f X Y is pair of (pair of X and Y) and 2."
            res `shouldBe` [TypedData (Comp [S "pair",V "A",V "B"]) [TypedConstructor (Comp [S "pair",S "of",V "X",S "and",V "Y"]) (DataType (Any "A") (DataType (Any "B") (TypeName [Named "pair",Any "A",Any "B"])))],TypedFunc (Comp [S "f",V "X",V "Y"]) (FuncType (Any "T12") (FuncType (Any "T13") (TypeName [Named "pair",TypeName [Named "pair",Any "T12",Any "T13"],EnkiInt]))) (TypedConstraints []) (TypedExpr {exprId = FuncCall (TypedConstructor (Comp [S "pair",S "of",V "X",S "and",V "Y"]) (DataType (TypeName [Named "pair",Any "T12",Any "T13"]) (DataType EnkiInt (TypeName [Named "pair",TypeName [Named "pair",Any "T12",Any "T13"],EnkiInt])))) (Map.fromList [("X",FuncCall (TypedConstructor (Comp [S "pair",S "of",V "X",S "and",V "Y"]) (DataType (Any "T12") (DataType (Any "T13") (TypeName [Named "pair",Any "T12",Any "T13"])))) (Map.fromList [("X",VarVal "X"),("Y",VarVal "Y")])),("Y",IntVal 2)])})]

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
            let (Right v) = parse (str "" []) "" "hello"
            v `shouldBe` S "hello"
        it "does not parse strings ending in a ':'" $
            isLeft (parse (str "" ["is"]) "" "is") `shouldBe` True

    describe "int" $ do
        it "parses integers" $ do
            let (Right v) = parse int "" "301"
            v `shouldBe` I 301
        it "parses negative integers" $ do
            let (Right v) = parse int "" "-31945812"
            v `shouldBe` I (-31945812)

    describe "enkiId" $ do
        it "parses a single expression" $ do
            let (Right v) = parse (enkiId "" []) "" "test"
            v `shouldBe` Comp [S "test"]
        it "parses multiple expressions" $ do
            let (Right v) = parse (enkiId "" []) "" "t y false B"
            v `shouldBe` Comp [S "t", S "y", B False, V "B"]
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
            v `shouldBe` DataType EnkiInt (TypeName [Named "list"])
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
                                [Field (Comp [V "Head"]) EnkiInt,Field (Comp [V "Tail"]) (TypeName [Named "list"])]]

    describe "execute" $ do
        runFile "examples/pe1.enki" "233168\n"
        runFile "examples/pe2.enki" "4613732\n"
        runFile "examples/pe3.enki" "6857\n"
        runFile "examples/pe5.enki" "232792560\n"
        runFile "examples/pe6.enki" "25164150\n"
        runFile "examples/nested-when.enki" "negative\nblah\nat least 5\nmore than 7\nmore than 10\n"
        runFile "examples/map-test.enki" "65\n"
        runFile "examples/call-test.enki" "11\n"
        runFile "examples/filter-test.enki" "30\n"
        runFile "examples/defref.enki" "24\n2\n[5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]\n"

