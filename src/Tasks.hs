{-# LANGUAGE MultiParamTypeClasses #-}

module Tasks where

import           BetaReduction                       (startSimplify)
import           ParseExpr                           (parseExpr)
import           Expr                                (Expr(..))
import           Control.Monad.State.Lazy            (join)

simplifyIOString :: IO String -> IO String
simplifyIOString str = (show . startSimplify . parseExpr) <$> str

writeToFile :: String -> IO String -> IO ()
writeToFile fileName res = join $ writeFile fileName <$> res

runFirstTask :: String -> String -> IO ()
runFirstTask input output = writeToFile output $ simplifyIOString $ readFile input