{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Data.List
import Data.Maybe

import Text.Parsec

import Util

type Parser a = forall s st m. Stream s m Char => ParsecT s st m a

data Id = S String
              | I Integer
              | B Bool
              | V String
              | Comp [Id]
    deriving (Eq, Show)

data Expr = Expr Id
    deriving (Eq, Show)

data Constraint = Constraint Id
                | Constraints [Constraint]
                | When Constraint Constraint
    deriving (Eq, Show)

data Def = Func Id Constraint Expr
         | Rule Id Constraint
    deriving (Eq, Show)

class PrettyPrint a where
    prettyPrint :: a -> String

maudeList :: [String] -> String
maudeList [] = "nil"
maudeList xs@(_:_) = unwords xs

instance PrettyPrint Id where
    prettyPrint (S str)      = "s(" ++ show str ++ ")"
    prettyPrint (I i)        = "i(" ++ show i ++ ")"
    prettyPrint (B b)        = "b(" ++ toLowerCase (show b) ++ ")"
    prettyPrint (V name)     = "v(" ++ show name ++ ")"
    prettyPrint (Comp parts) = "comp(" ++ maudeList (map prettyPrint parts) ++ ")"

instance PrettyPrint Expr where
    prettyPrint (Expr id) = "e(" ++ prettyPrint id ++ ")"

instance PrettyPrint Constraint where
    prettyPrint (Constraint id) = "c(" ++ prettyPrint id ++ ")"
    prettyPrint (Constraints cs) = "cs(" ++ maudeList (map prettyPrint cs) ++ ")"
    prettyPrint (When condition body) = "when(" ++ prettyPrint condition ++ "," ++ prettyPrint body ++ ")"

instance PrettyPrint Def where
    prettyPrint (Func id c e) = "def(f(" ++ prettyPrint id ++ "," ++ prettyPrint c ++ "," ++ prettyPrint e ++ "))"
    prettyPrint (Rule id c) = "def(r(" ++ prettyPrint id ++ "," ++ prettyPrint c ++ "))"

parseFile :: String -> Maybe String -> IO ()
parseFile fname output = do
    contents <- readFile fname
    case parseDef contents of
        Left err -> error $ show err
        Right parsed ->
            let outputStr = intercalate "\n" $ map prettyPrint parsed
            in case output of
                Nothing -> putStrLn outputStr
                Just file -> writeFile file outputStr

parseDef :: String -> Either ParseError [Def]
parseDef = parse enkiDef ""

enkiDef :: Parser [Def]
enkiDef = many (try func <|> try rule)

lineSep :: Parser ()
lineSep = do
    wsSkip
    optional newlines
    wsSkip

rule :: Parser Def
rule = do
    lineSep
    id <- enkiId
    wsSkip
    choice $ map string ["if:", "where:"]
    lineSep
    c <- constraint
    wsSkip
    char '.'

    pure $ Rule id c

func :: Parser Def
func = do
    lineSep
    id <- enkiId
    wsSkip
    string "is:"
    lineSep
    constr <- constraint
    wsSkip
    char '.'

    pure $ case constr of
        Constraint cid -> Func id (Constraints []) $ Expr cid
        Constraints cs ->
            let (Constraint cid) = last cs
            in Func id (Constraints (init cs)) $ Expr cid
        When _ _ -> error "Cannot have a function with only a when branch for it's body"

expr :: Parser Expr
expr = Expr <$> enkiId

constraint :: Parser Constraint
constraint = Constraints <$> ((sepBy1 (when <|> otherwiseBranch) lineSep) <|>
                              (map Constraint <$> (sepBy1 enkiId sep)))
    where
        sep = wsSkip >> string "," >> wsSkip >> optional newlines >> wsSkip

when :: Parser Constraint
when = do
    lineSep
    string "when"
    lineSep
    condition <- constraint
    lineSep

    body <- optionMaybe $ do
        string "then:"
        lineSep
        body <- constraint
        lineSep
        pure body
    char '.'
    lineSep

    pure $ When condition $ fromMaybe (Constraints []) body

otherwiseBranch :: Parser Constraint
otherwiseBranch = do
    lineSep
    string "otherwise"
    lineSep
    string "then:"
    lineSep
    body <- constraint
    lineSep

    pure $ When body $ Constraints []

withWs :: Stream s m Char => ParsecT s st m a -> ParsecT s st m a
withWs parser = do
    a <- parser
    wsSkip
    pure a

enkiId :: Parser Id
enkiId = Comp <$> untilFail (choice (map (try . withWs) [var, bool, int, str, paren]))

untilFail :: Parser a -> Parser [a]
untilFail parser = do
    r <- optionMaybe parser
    case r of
        Nothing -> pure []
        Just v -> do
            rs <- untilFail parser
            pure $ v:rs

paren :: Parser Id
paren = between (string "(" >> wsSkip) (wsSkip >> string ")") enkiId

str :: Parser Id
str = S <$> (symbols <|> concatOp)
    where
        symbols = many1 (oneOf cs) >>= \m -> notFollowedBy (oneOf ":") >> pure m
        cs = ['a'..'z'] ++ ['+', '-', '=', '*', '/', '^']
        concatOp = string ".."

var :: Parser Id
var = V <$> do
    s <- oneOf ['A'..'Z']
    ss <- many $ oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

    pure $ s:ss

nonzeroDigit :: Parser Char
nonzeroDigit = oneOf "123456789"

int :: Parser Id
int = I . read <$> string "0" <|> do
    neg <- optionMaybe $ try $ string "-"
    digits <- (:) <$> nonzeroDigit <*> many digit

    pure $ I $ read $ fromMaybe "" neg ++ digits

bool :: Parser Id
bool = B . read . titleCase <$> choice [string "true", string "false"]

wsSkip :: Parser ()
wsSkip = skipMany $ oneOf " \r\t"

whitespace :: Parser ()
whitespace = skipMany1 $ oneOf " \t\n\r"

newlines :: Parser ()
newlines = skipMany1 (wsSkip >> char '\n' >> wsSkip)

