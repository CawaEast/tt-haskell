{-# LANGUAGE MultiParamTypeClasses #-}

module Expr where

data Expr = Lambda String Expr  
          | Application Expr Expr
          | Var String
          deriving Eq

instance Show Expr where
    show (Var t)           = t
    show (Application a b) = "(" ++ show a ++ " " ++ show b ++  ")"
    show (Lambda v b)      = "\\" ++  v ++ "." ++ show b