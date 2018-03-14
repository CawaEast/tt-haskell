{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Control.Applicative        (Applicative, liftA2, many, some,
                                             (<|>), empty)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (Reader, reader, runReader,
                                             withReader)
import           Control.Monad.State.Lazy   (State, join, runState, state)
import           Data.Functor               (void)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe, isJust, fromJust)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Text.Megaparsec            (ParseError, Parsec, Token, between,
                                             notFollowedBy, runParser, try)
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr       (Operator (..), makeExprParser)
import           Control.Arrow         ((***))
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
    

type ExprEq     = (ExprType, ExprType)
type STEMap = Map.Map String ExprType

data ExprSys    = ES [ExprEq] STEMap deriving Show
-- just system, context, counter

emptyTS :: ExprSys
emptyTS = ES [] Map.empty

makeSystem :: Expr -> (ExprSys, Int) -> (ExprType, ExprSys, Int)
makeSystem (Var str) (es@(ES system context), counter)
    | Map.member str context = (context Map.! str, es, counter)
    | otherwise              = (newVar, ES system (Map.insert str newVar context), newCounter)
  where
    (newVar, newCounter) = newType counter
makeSystem (Application a b) ts = (newVar, ES (newSys:system) context, newCounter)
  where
    (t1, es', c') = makeSystem a ts 
    (t2, ES system context, counter) = makeSystem b (es', c')
    (newVar, newCounter) = newType counter
    newSys = (t1, Implication t2 newVar)
makeSystem (Lambda str a) ts = (Implication t2 t1, ES system context', counter')
  where
    (t1, ES system context, counter) = makeSystem a ts
    (t', newCounter) = newType counter 
    (t2, context', counter') = 
        if Map.member str context
        then (context Map.! str, Map.delete str context, counter)
        else (t', context, newCounter)

solveSystem :: [ExprEq] -> (Maybe STEMap)
solveSystem s = fmap (\(ES a b) -> b) $ solveSystem' (ES s Map.empty)
    
solveSystem' :: ExprSys -> Maybe ExprSys
solveSystem' (ES [] vars) = Just (ES [] vars)
solveSystem' (ES (f:fs) vars) = solveOne f fs vars >>= solveSystem'
  where
    solveOne (TypeVar str, q@(TypeVar str2)) fs vars
        | str == str2  = Just $ ES fs vars
        | hasVar str q  = Nothing
        | otherwise     = Just $ changeSystem (changeType str q) $ ES fs $ Map.insert str q vars
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
    
getType :: Expr -> Maybe (ExprType, STEMap)
getType e = (solveSystem equ) >>= (\m -> Just $ (changeByMap m et, Map.map (changeByMap m) cont))
  where
    (et, (ES equ cont), c) = makeSystem e (emptyTS, 0)
    
changeByMap :: STEMap -> ExprType -> ExprType
changeByMap ma v@(TypeVar str) = fromMaybe v (ma Map.!? str)
changeByMap ma (Implication a b) = (Implication (changeByMap ma a) (changeByMap ma b))
    

runSecondTask :: String -> IO String
runSecondTask input = (\q -> maybe "Лямбда-выражение не имеет типа." printTypeAns q) <$> getType <$> e 
  where
    e = myRun exprParser <$> readFile input
    
printTypeAns :: (ExprType, STEMap) -> String
printTypeAns (et, m) = show et ++ "\n" ++  foldMap (\(a, b) -> a ++ ":" ++ show b ++ "\n") (Map.toList m)
    
          
data HMExpr 
    = HMLambda String HMExpr
    | HMLet String HMExpr HMExpr
    | HMVar String 
    | HMApplication HMExpr HMExpr
    
instance Show HMExpr where
    show (HMLambda s e) = "\\" ++ s ++ "." ++ show e ++ ""
    show (HMLet s e b) = "(let " ++ s ++ " = " ++ show e ++ " in " ++ show b ++ ")"
    show (HMApplication e b) = "(" ++ show e ++ " " ++ show b ++ ")"
    show (HMVar s) = s
    
data HMType
    = ForAll String HMType
    | Mon ExprType
    
instance Show HMType where
    show (Mon e) = show e
    show (ForAll str a) = "(forAll " ++ str ++ "." ++ show a ++ ")" 
    
varHMParser :: Parser HMExpr
varHMParser = do
    _ <- notFollowedBy $ string "in"
    _ <- notFollowedBy $ string "let"
    i <- identifier
    return (HMVar i)

atomHMParser :: Parser HMExpr
atomHMParser  =   parens exprHMParser
            <|> varHMParser

lambdaHMParser :: Parser HMExpr
lambdaHMParser = do
    _ <- symbol "\\"
    i <- identifier
    _ <- symbol "."
    a <- exprHMParser
    return (HMLambda i a)
    
letHMParser :: Parser HMExpr
letHMParser = do
    _ <- symbol "let"
    i <- identifier
    _ <- symbol "="
    a <- exprHMParser
    _ <- symbol "in"
    b <- exprHMParser
    return (HMLet i a b)

exprHMParser :: Parser HMExpr
exprHMParser = foldl1 HMApplication <$> some (lambdaHMParser <|> letHMParser <|> atomHMParser)

-- type ExprEq     = (ExprType, ExprType)
-- type STEMap = Map.Map String ExprType

type Context = Map.Map String HMType
type Uni = HMType -> HMType

newHMType :: Int -> HMType -> (Int, Context, ExprType)
newHMType c (Mon e) = (c, Map.empty, e)
newHMType c (ForAll str e) = (\(c, cont, et) -> (c, Map.insert str (Mon newVar) cont, changeType str newVar et)) $ newHMType newCounter e
  where 
    (newVar, newCounter) = newType c
    
newHMType1 :: Int -> HMType -> (Int, Context, ExprType)
newHMType1 c e = newHMTypeLoc c Set.empty e
  where
    newHMTypeLoc:: Int -> SSet-> HMType -> (Int, Context, ExprType)
    newHMTypeLoc c s (Mon e) = newTypeLoc c s e
    newHMTypeLoc c s (ForAll str e) = newHMTypeLoc c (Set.insert str s) e
    newTypeLoc:: Int -> SSet-> ExprType -> (Int, Context, ExprType)
    newTypeLoc c s (Implication a b) = (c'', Map.union cont' cont'', Implication a' b')
      where
      (c', cont', a') = newTypeLoc c s a 
      (c'', cont'', b') = newTypeLoc c' s b 
    newTypeLoc c s v@(TypeVar str)
        | Set.member str s = (newCounter, Map.empty, newVar)
        | otherwise =         (c, Map.empty, v)
      where
        (newVar, newCounter) = newType c

algoritmW :: HMExpr -> (Context, Int) -> Maybe (ExprType, Context, Int, Uni, STEMap)
algoritmW (HMVar str) (context, counter)
    | Map.member str context = Just (varFromContext, context, newCounter1, id, Map.empty)
    | otherwise              = Just (newVar,  Map.insert str (Mon newVar) context, newCounter, id, Map.empty)
  where
    (newVar, newCounter) = newType counter
    (newCounter1, cont', varFromContext) = newHMType counter $ context Map.! str
algoritmW (HMApplication a b) ts = do 
    (t1, es', c', f1, m1) <- algoritmW a ts 
    (t2, context, counter, f2, m2) <- algoritmW b (es', c')
    let (newVar, newCounter) = newType counter
    let (Mon t1') = f2 (Mon t1) 
    m <- solveSystem [(t1', Implication t2 newVar)]
    let f = (changeByMapHM m)
    return (changeByMap m newVar, fmap f context, newCounter, (f . f2 . f1), m)
algoritmW (HMLambda str a) (context, counter) = do
    let (strT, newCounter) = newType counter 
    let tmpX = context Map.!? str
    (t1, context', counter', f, m1) <- algoritmW a (Map.insert str (Mon strT) context, newCounter)
    let (Mon t1') = f (Mon (Implication strT t1)) 
    --pure (t1', fmap f $ context', counter', f, m1)
    let ma1 = fmap f $ Map.delete str context'
    let ma2 = if isJust tmpX then Map.insert str (fromJust tmpX) ma1 else ma1  
    return (t1', ma2, counter', f, m1)
algoritmW (HMLet str a b) (context, counter) = do
    (t1, context', counter', f, m1) <- algoritmW a (context, counter)
    let (newT1, ss) = closureHM t1 context'
    (t2, context2, counter2, f2, m2) <- algoritmW b (Map.insert str newT1 context', counter')
    return (t2, context2, counter2, f2 . f, m1)


changeByMapHM :: STEMap -> HMType -> HMType
changeByMapHM ma v@(ForAll str a) = ForAll str $ changeByMapHM (Map.delete str ma) $ a
changeByMapHM ma (Mon a) = Mon (changeByMap ma a)

closureHM :: ExprType -> Context -> (HMType, SSet)
closureHM e cont = (foldr ForAll (Mon e) vars, vars)
  where
    vars = (getVarsType e) 
    --vars = Set.difference (getVarsType e) $ Map.foldr (\hm l -> Set.union l $ getHMVarsType hm) Set.empty cont

getHMVarsType :: HMType -> SSet
getHMVarsType (Mon s) = getVarsType s
getHMVarsType (ForAll s a) = Set.delete s $ getHMVarsType a
    
getVarsType :: ExprType -> SSet
getVarsType (TypeVar s) = Set.singleton s
getVarsType (Implication a b) = Set.union (getVarsType a) (getVarsType b)

getHMType :: HMExpr -> Maybe (HMType, Context)
getHMType e = fmap (\(a, b, c, d, e) -> (Mon a, b)) $ algoritmW e (Map.empty, 0)
--  where
--    (et, context, c, f) = algoritmW e (Map.empty, 0)

runThirdTask :: String -> String -> IO ()
runThirdTask input output = writeToFile output $ (\q -> maybe "Лямбда-выражение не имеет типа." printHMTypeAns q) <$> getHMType <$> e 
  where
    e = myHMRun exprHMParser <$> readFile input
    
myHMRun :: Parser HMExpr -> String -> HMExpr
myHMRun p s = either (\_ -> HMVar "Error") id (runParser p "" s)
    
printHMTypeAns :: (HMType, Context) -> String
printHMTypeAns (et, m) = show et ++ "\n" ++  foldMap (\(a, b) -> a ++ ":" ++ show b ++ "\n") (Map.toList m)