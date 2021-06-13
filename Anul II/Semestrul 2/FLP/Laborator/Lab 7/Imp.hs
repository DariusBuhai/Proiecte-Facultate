module Imp where

-- Import stuff.
import qualified Text.Parsec.Token as Token
import Text.Parsec.String ( Parser, parseFromFile )
import Text.Parsec.Expr
    ( buildExpressionParser, Assoc(..), Operator(..) )
import Text.ParserCombinators.Parsec.Language
    ( emptyDef, GenLanguageDef( .. ), LanguageDef)
import Text.Parsec ( alphaNum, letter, (<|>), eof,State (State) )
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe


--Defining AST.
type Name = String

data BinAop = Add | Mul | Sub | Div | Mod
  deriving (Show)

data BinCop = Lt | Lte | Gt | Gte
  deriving (Show)

data BinEop = Eq | Neq
  deriving (Show)

data BinLop = And | Or
  deriving (Show)

data Exp
    = Id Name
    | I Integer
    | B Bool
    | UMin Exp
    | BinA BinAop Exp Exp
    | BinC BinCop Exp Exp
    | BinE BinEop Exp Exp
    | BinL BinLop Exp Exp
    | Not Exp
  deriving (Show)

data Type = TInt | TBool
  deriving (Eq, Show)

data Stmt
    = Asgn Name Exp
    | If Exp Stmt Stmt
    | Read String Name
    | Print String Exp
    | While Exp Stmt
    | Block [Stmt]
    | Decl Type Name
  deriving (Show)

-- Creating parser.
impLanguageDef :: LanguageDef ()
impLanguageDef =
    emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = False
    , caseSensitive = True
    , identStart = letter
    , identLetter = alphaNum
    , reservedNames =
        [ "while", "if", "else", "int", "bool"
        , "true", "false", "read", "print"
        ]
    , reservedOpNames =
        [ "+", "-", "*", "/", "%"
        , "==", "!=", "<", "<=", ">=", ">"
        , "&&", "||", "!", "="
        ]
    }

-- Root parser utilities.
impLexer :: Token.TokenParser ()
impLexer = Token.makeTokenParser impLanguageDef
identifier :: Parser String
identifier = Token.identifier impLexer
reserved :: String -> Parser ()
reserved = Token.reserved impLexer
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp impLexer
parens :: Parser a -> Parser a
parens = Token.parens impLexer
braces :: Parser a -> Parser a
braces = Token.braces impLexer
semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep impLexer
integer :: Parser Integer
integer = Token.integer impLexer
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace impLexer
stringParser :: Parser String
stringParser = Token.stringLiteral impLexer
commaParser :: Parser String
commaParser = Token.comma impLexer
bracesParser :: Parser a -> Parser a
bracesParser = Token.braces impLexer
semiParser :: Parser String
semiParser = Token.semi impLexer
semiSepParser :: Parser a -> Parser [a]
semiSepParser = Token.semiSep impLexer

-- Expression parser able to parse expressions.
expression :: Parser Exp
expression = buildExpressionParser operators term
    where
        operators =
            [ [ prefix "!" Not
              , prefix "-" UMin
              ]
            , [ binary "*" (BinA Mul) AssocLeft
              , binary "/" (BinA Div) AssocLeft
              , binary "%" (BinA Mod) AssocLeft
              ]
            , [ binary "+" (BinA Add) AssocLeft
              , binary "-" (BinA Sub) AssocLeft
              ]
            , [ binary "==" (BinE Eq) AssocNone
              , binary "<=" (BinC Lte) AssocNone
              , binary "<" (BinC Lt) AssocNone
              , binary ">=" (BinC Gte) AssocNone
              , binary ">" (BinC Gt) AssocNone
              ]
            , [ binary "&&" (BinL And) AssocLeft
              , binary "||" (BinL Or) AssocLeft
              ]
            ]
        binary name fun = Infix ( reservedOp name >> return fun)
        prefix name fun = Prefix ( reservedOp name >> return fun)

-- Expression term. Used for nested parans.
term :: Parser Exp
term =
    parens expression
    <|> I <$> integer
    <|> Id <$> identifier
    <|> boolExp

-- Boolean expression parser ("true" / "false").
-- Parsec doesn't have a predefined boolean parser.
boolExp :: Parser Exp
boolExp =
    (do
        reserved "true"
        return $ B True)
    <|>
    (do
        reserved "false"
        return $ B False)

-- Reads lines without brackets, and closes them in a Block.
readLines :: Parser Stmt
readLines =
    Block <$> semiSepParser statement

-- Reads lines, and closes them in a Block.
readBlock :: Parser Stmt
readBlock =
    bracesParser (Block <$> semiSepParser statement) <|> statement

-- Parses a statement.
statement :: Parser Stmt
statement = ifStmt
        <|> asgnStmt
        <|> readStmt
        <|> printStmt
        <|> whileStmt
        <|> declStmt

-- Parses assignments.
asgnStmt :: Parser Stmt
asgnStmt = do
    varname <- identifier
    reservedOp "="
    Asgn varname <$> expression

-- Parses "Read(...)" commands.
readStmt :: Parser Stmt
readStmt = do
    reserved "read"

    let readParameters :: Parser (String, Name)
        readParameters = do
            str <- stringParser
            _ <- commaParser
            name <- identifier
            return (str, name)

    (str, name) <- parens readParameters

    return $ Read str name

-- Parses "Print(...)" commands.
printStmt :: Parser Stmt
printStmt = do
    reserved "print"

    let readParameters :: Parser (String, Exp)
        readParameters = do
            str <- stringParser
            _ <- commaParser
            expr <- expression
            return (str, expr)

    (str, expr) <- parens readParameters
    return $ Print str expr

-- Parses a type ("int" / "bool").
typeParser :: Parser Type
typeParser =
    (reserved "int" >> return TInt) <|> (reserved "bool" >> return TBool)

-- Parses a declaration ("int a;").
declStmt :: Parser Stmt
declStmt = do
    t <- typeParser
    Decl t <$> identifier

-- Parses an if/else statement ("if (...) { ... } else { ... }").
ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- parens expression
    thenS <- readBlock
    reserved "else"
    If cond thenS <$> readBlock

-- Parses a While statement ("While (...) { ... }").
whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    condition <- parens expression
    While condition <$> readBlock



type Environment = Map.Map String Int
type Store = Map.Map Int Integer
type Storage = (Environment, Store)
type M = StateT Storage IO

evaluate :: Exp -> M Integer
evaluate (Id name) = do
    (env, store) <- get
    let poz_in_store = fromJust $ Map.lookup name env
    return $ fromJust $ Map.lookup poz_in_store store
evaluate (I x) = return x
evaluate (B True) = return 1
evaluate (B False) = return 0
evaluate (UMin a) = negate <$> evaluate a
evaluate (BinA op a b) = do
    ansa <- evaluate a
    ansb <- evaluate b
    case op of
        Add -> return (ansa + ansb)
        Mul -> return (ansa * ansb)
        Sub -> return (ansa - ansb)
        Div -> return (ansa `div` ansb)
        Mod -> return (ansa `mod` ansb)
evaluate (BinC op a b) = do
    ansa <- evaluate a
    ansb <- evaluate b
    case op of
        Lt -> return $ if ansa < ansb then 1 else 0 
        Lte -> return $ if ansa <= ansb then 1 else 0 
        Gt -> return $ if ansa > ansb then 1 else 0 
        Gte -> return $ if ansa >= ansb then 1 else 0 
evaluate (BinE op a b) = do
    ansa <- evaluate a
    ansb <- evaluate b
    case op of
        Eq -> return $ if ansa == ansb then 1 else 0 
        Neq -> return $ if ansa /= ansb then 1 else 0
evaluate (BinL op a b) = do
    ansa <- evaluate a
    ansb <- evaluate b
    case op of
        And -> return $ if ansa * ansb /= 0 then 1 else 0 
        Or -> return $ if ansa + ansb /= 0 then 1 else 0
evaluate (Not a) = (\x -> if x /= 0 then 0 else 1) <$> evaluate a


interpret :: Stmt -> StateT (Environment, Store) IO ()
interpret (Asgn var exp) = do
    val <- evaluate exp
    (env, store) <- get

    let id_in_store = fromJust $  Map.lookup var env
    let new_map = Map.insert id_in_store val store

    put (env, new_map)
interpret (If cond if_true if_false) = do
    value_cond <- evaluate cond
    if value_cond == 1 then
        interpret if_true
    else
        interpret if_false
interpret (Read display_text var) = do
    lift $ putStr display_text
    read_value <- lift (readLn :: IO Integer)
    interpret (Asgn var (I read_value))
interpret (Print display_text var) = do
    value <- evaluate var
    lift $ putStrLn (display_text ++ " " ++ show value)
interpret (While cond stmt) = do
    value_cond <- evaluate cond
    if value_cond == 0 then
        return ()
    else do
        interpret stmt
        interpret (While cond stmt)
interpret (Decl _ var_name) = do
    (env, store) <- get
    let poz_in_memory = Map.size store
    let new_store = Map.insert poz_in_memory 0 store
    let new_env = Map.insert var_name poz_in_memory env
    put (new_env, new_store)

interpret (Block operations) = do
    (old_env, _) <- get

    mapM_ interpret operations

    -- Maybe trim act_store to the size of old_store?
    (_, act_store) <- get
    put (old_env, act_store)

startInterpretation :: Stmt -> IO ()
startInterpretation stmt = do
    (_, (env, store)) <- runStateT (interpret stmt) (Map.empty, Map.empty)
    putStrLn $ "The final env is: " ++ show env
    putStrLn $ "The final store is: " ++ show store


-- REAL CODE STARTS BELOW
--  ||
--  ||
--  \/

type CheckerState = Map.Map Name Type

emptyCheckerState :: CheckerState
emptyCheckerState = Map.empty


newtype EReader a = EReader { runEReader :: CheckerState -> (Either String a) }

instance Monad EReader where
    return a = EReader (\env -> Right a)
    act >>= k = EReader f
        where
            f env = case (runEReader act env) of
                        Left s -> Left s
                        Right va -> runEReader (k va) env

instance Applicative EReader where
    pure = return
    a <*> b = do
        f <- a
        f <$> b

instance Functor EReader where
    fmap f a = do
        x <- a
        return $ f x

getCheckerState :: EReader CheckerState
getCheckerState =
    EReader f
    where
        f env = Right  env

throwError :: String -> EReader a
throwError err =
    EReader f
    where f _ = Left err

guardType :: Type -> Type -> EReader ()
guardType TInt TInt = return ()
guardType TBool TBool = return ()
guardType _ _ = throwError "Types int and bool do not match!"

lookupM :: String -> EReader Type
lookupM name = do
    state <- getCheckerState
    case Map.lookup name state of
        Nothing ->  throwError $ "Variable " ++ name ++ " is not defined!"
        Just x -> return x

localM :: CheckerState -> EReader a -> EReader a
localM state reader_init =
    EReader f
    where 
        f _ = runEReader reader_init state
-- Monad Checking
type MC = EReader

checkExp :: Exp -> MC Type
checkExp (I _) = return TInt
checkExp (B _) = return TBool
checkExp (Id name) = lookupM name
checkExp (UMin a) = do
    type_a <- checkExp a
    guardType type_a TInt
    return TInt
checkExp (BinA _ a b) = do
    type_a <- checkExp a
    type_b <- checkExp b
    guardType type_a TInt
    guardType type_b TInt
    return TInt
checkExp (BinC _ a b) = do
    type_a <- checkExp a
    type_b <- checkExp b
    guardType type_a type_b
    return TBool
checkExp (BinE _ a b) = checkExp $ BinC Lt a b
checkExp (BinL _ a b) = do
    type_a <- checkExp a
    type_b <- checkExp b
    guardType type_a TBool
    guardType type_b TBool
    return TBool
checkExp (Not e) = do
    type_e <- checkExp e
    guardType type_e TBool
    return TBool

checkStmt :: Stmt -> MC ()
checkStmt (Read _ name) = do
    type_a <- lookupM name
    guardType type_a TInt
checkStmt (Print _ exp) = do
    type_a <- checkExp exp
    guardType type_a TInt
checkStmt (Asgn name exp) = do
    type_name <- lookupM name
    type_exp <- checkExp exp
    guardType type_name type_exp
checkStmt (If exp stm1 stm2) = do
    type_exp <- checkExp exp
    guardType type_exp TBool
    checkStmt stm1
    checkStmt stm2
checkStmt (Decl _ _) = error "Should not get here!"
checkStmt (Block lst) = checkBlock lst
checkStmt (While exp stm) = do
    type_exp <- checkExp exp
    guardType type_exp TBool
    checkStmt stm
    -- (guardType TBool <$> checkExp exp) >> checkStmt stm


checkBlock :: [ Stmt ] -> MC ()
checkBlock [] = return ()
checkBlock (Decl t name : x) = do
    state <- getCheckerState
    let new_state = Map.insert name t state
    localM new_state $ checkBlock x
checkBlock (h:t) = do
    checkStmt h
    checkBlock t


checkPgm :: Stmt -> IO Bool
checkPgm stm = do
    case runEReader (checkStmt stm) Map.empty of
        Left err -> putStrLn err >> return False
        Right _ -> putStrLn "Type check pass!" >> return True



-- Parses the "1.imp" file.
-- Or at least, it's supposed to :(
main :: IO ()
main = do
    result <- parseFromFile (readLines <* eof) "1.imp"
    case result of
        Left err  -> print err
        Right xs  -> do
            type_is_ok <- checkPgm xs
            guard type_is_ok
            startInterpretation xs

