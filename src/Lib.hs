{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import           Control.Applicative        (Applicative, liftA2, many, some,
                                             (<|>), empty)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (Reader, reader, runReader,
                                             withReader)
import           Control.Monad.State.Lazy   (State, join, runState, state)
import           Data.Functor               (void)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Text.Megaparsec            (ParseError, Parsec, Token, between,
                                             notFollowedBy, runParser, try)
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)
--import           Text.Megaparsec.String     (Parser)

type Parser = Parsec MyError String

data MyError
    = SomeInitianError
    | VarNotFound
    | VarHasBeenSet
    | DivByZero
    | ParseError
    deriving (Show, Eq, Ord)

--newtype Var = Var String deriving (Eq, Show)

data Expr = Lambda String Expr
          | Application Expr Expr
          | Var String
          deriving (Eq, Show)

sc :: Parser ()
sc = L.space Text.Megaparsec.Char.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '\''))

myRun :: Parser Expr -> String -> Expr
myRun p s = either (\_ -> Var "Error") id (runParser p "" s)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

varParser :: Parser Expr
varParser = Var <$> identifier

atomParser :: Parser Expr
atomParser  =   parens exprParser
            <|> varParser

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

lambdaParser :: Parser Expr
lambdaParser = do
    _ <- symbol "\\"
    i <- identifier
    _ <- symbol "."
    a <- exprParser
    return (Lambda i a)

exprParser :: Parser Expr
exprParser = foldl1 Application <$> some (atomParser <|> lambdaParser)

newtype Env = Env Int

startSimplify :: Expr -> Expr
startSimplify e = makeGoodVars $ snd $ runSimplify (Env 0) e

makeGoodVars :: Expr -> Expr
makeGoodVars (Var e)           = Var ('v':e)
makeGoodVars (Application a b) = Application (makeGoodVars a) (makeGoodVars b)
makeGoodVars (Lambda str a)    = Lambda ('v':str) $ makeGoodVars a

runSimplify :: Env -> Expr -> (Env, Expr)
runSimplify env expr
    | wasBR     = runSimplify env' expr'
    | otherwise = (env', expr')
  where
    (wasBR, env', expr') = simplify env expr

simplify :: Env -> Expr -> (Bool, Env, Expr)
simplify env (Lambda v expr) = (\(env', expr') -> (False, env', Lambda v expr')) $ runSimplify env expr
simplify env appl@(Application (Lambda v expr) exprToVar) = (\(env', expr') -> (True, env', expr')) $ betaReduction env appl
simplify env (Application a b)
    | wasReduction  = simplify env' (Application newA b)
    | otherwise     = (False, env'', Application newA newB)
  where
    (wasReduction, env', newA) = simplify env a
    (env'', newB) = runSimplify env' b
simplify env a = (False, env, a)

betaReduction :: Env -> Expr -> (Env, Expr)
betaReduction (Env counter) e@(Application (Lambda v expr) b) = (Env counter, change v b newExpr)
  where
    free = findFree b
    locked = findLocked v expr
    (newCounter, m) = makePairs counter $ Set.intersection free locked
    newExpr = renameLocked m expr
betaReduction env e = (env, e)

change :: String -> Expr -> Expr -> Expr
change v e (Var str)         = if str == v then e else Var str
change v e (Application a b) = Application (change v e a) (change v e b)
change v e (Lambda str expr) = Lambda str $ if str == v then expr else change v e expr

type SSet = Set.Set String
type SMap = Map.Map String String

findFree :: Expr -> SSet
findFree (Var s)           = Set.singleton s
findFree (Application a b) = Set.union (findFree a) (findFree b)
findFree (Lambda var a)    = Set.delete var (findFree a)

findLocked :: String -> Expr -> SSet
findLocked = findLocked' Set.empty 
  where
    findLocked' :: SSet -> String -> Expr -> SSet
    findLocked' acc var (Var str)         = if var == str then acc       else Set.empty
    findLocked' acc var (Lambda str a)    = if var == str then Set.empty else findLocked' (Set.insert str acc) var a
    findLocked' acc var (Application a b) = Set.union (findLocked' acc var a) (findLocked' acc var b)

renameLocked :: SMap -> Expr -> Expr
renameLocked = rename' Set.empty
  where
    rename' :: SSet -> SMap -> Expr -> Expr
    rename' locked vars (Var str)
        | Set.member str locked           = Var $ fromMaybe str $ vars Map.!? str
        | otherwise                       = Var str
    rename' locked vars (Lambda str a)    = Lambda (fromMaybe str (vars Map.!? str)) $ rename' (Set.insert str locked) vars a
    rename' locked vars (Application a b) = Application (rename' locked vars a) (rename' locked vars b)

makePairs :: Int -> SSet -> (Int, SMap)
makePairs counter vars = (newCounter, Map.fromList $ zip (Set.toList vars) $ map show [counter .. newCounter - 1])
  where
    newCounter = counter + Set.size vars

toString :: Expr -> String
toString (Var t)           = t
toString (Application a b) = "(" ++ toString a ++ " " ++ toString b ++  ")"
toString (Lambda v b)      = "\\" ++  v ++ "." ++ toString b

simplifyIOString :: IO String -> IO String
simplifyIOString str = (toString . startSimplify . myRun exprParser) <$> str

writeToFile :: String -> IO String -> IO ()
writeToFile fileName res = join $ writeFile fileName <$> res

runFirstTask :: String -> String -> IO ()
runFirstTask input output = writeToFile output $ simplifyIOString $ readFile input

data ExprType 
    = Implication ExprType ExprType
    | TypeVar String
    
instance Show ExprType where
    show (TypeVar t)       = t
    show (Implication a b) = "(" ++ show a ++ "->" ++ show b ++  ")"
    
-- just system, context, counter
data TypeSystem = TS [(ExprType, ExprType)] (Map.Map String ExprType) Int

makeSystem :: Expr -> TypeSystem -> (ExprType, TypeSystem)
makeSystem (Var str) ts@(TS system context counter)
    | Map.member str context = (context Map.! str, ts)
    | otherwise              = (newVar, TS system (Map.insert str newVar context) newCounter)
  where
    (newVar, newCounter) = newType counter
makeSystem (Application a b) ts = (newVar, TS (newSys:system) context newCounter)
  where
    (t1, ts') = makeSystem a ts 
    (t2, (TS system context counter)) = makeSystem b ts' 
    (newVar, newCounter) = newType counter
    newSys = (t1, Implication t2 newVar)
makeSystem (Lambda str a) ts = (Implication t2 t1, TS system context' counter')
  where
    (t1, (TS system context counter)) = makeSystem a ts
    (t', newCounter) = newType counter 
    (t2, context', counter') = 
        if Map.member str context
        then (context Map.! str, Map.delete str context, counter)
        else (t', context, newCounter)
    
type ExprEq     = (ExprType, ExprType)
type STEMap = Map.Map String ExprType
data ExprSys    = ES [ExprEq] STEMap

solveSystem :: [ExprEq] -> (Maybe STEMap)
solveSystem s = fmap (\(ES a b) -> b) $ solveSystem' (ES s Map.empty)
    
solveSystem' :: ExprSys -> Maybe ExprSys
solveSystem' (ES [] vars) = Just (ES [] vars)
solveSystem' (ES (f:fs) vars) = solveOne f fs vars
  where
    solveOne (TypeVar str, q) fs vars
        | hasVar str q  = Nothing
        | otherwise     = Just $ changeSystem (changeType str q) $ ES fs $ Map.insert str q vars 
    solveOne (q, TypeVar str) fs vars
        | hasVar str q  = Nothing
        | otherwise     = Just $ changeSystem (changeType str q) $ ES fs $ Map.insert str q vars 
    solveOne (Implication a b, Implication c d) fs vars = Just $ ES ((a, c):(b, d):fs) vars
       
changeSystem :: (ExprType -> ExprType) -> ExprSys -> ExprSys
changeSystem f (ES fs vars) = (ES (fmap (\(a,b) -> (f a, f b)) fs) (fmap f vars))
   
hasVar :: String -> ExprType -> Bool
hasVar s (TypeVar str) = s == str   
hasVar s (Implication a b) = (hasVar s a) || (hasVar s b)

changeType :: String -> ExprType -> ExprType -> ExprType
changeType s sExpr (TypeVar str) = if s == str then sExpr else TypeVar str
changeType s sExpr (Implication a b) = Implication (changeType s sExpr a) (changeType s sExpr b)
    
newType :: Int -> (ExprType, Int)
newType i = (TypeVar ('v':(show $ i + 1)), i + 1) 
    
--getVars :: Expr -> SSet
--getVars 