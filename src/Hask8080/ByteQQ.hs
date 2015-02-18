{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}

-- |

module Hask8080.ByteQQ where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import qualified Data.Bits.Bitwise         as Bits
import           Data.Char                 (isLower)
import           Data.Data
import           Data.List                 (groupBy)
import           Data.Word                 (Word8)

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote


byte :: QuasiQuoter
byte = QuasiQuoter { quoteExp = quoteByteExp
                   , quotePat = quoteBytePat
                   , quoteType = error "no byte quasiquoter for types"
                   , quoteDec = error "no byte quasiquoter for declarations"
                   }

quoteByteExp = undefined

quoteBytePat = undefined

data Expr
  = Bit Bool
  | Var String
  deriving (Show, Eq, Data, Typeable)

toMask :: [Expr] -> Word8
toMask = Bits.fromListBE . concatMap toMaskBit
  where toMaskBit (Bit _)  = [True]
        toMaskBit (Var xs) = map (const False) xs

toTest :: [Expr] -> Word8
toTest = Bits.fromListBE . concatMap toTestBit
  where toTestBit (Bit b) = [b]
        toTestBit (Var xs)= map (const False) xs

matches :: Word8 -> [Expr] -> Bool
matches b p = b .&. toMask p == toTest p

extract :: [Expr] -> Word8 -> String -> Word8
extract pat b v = extract' 0 (reverse pat)
  where
    extract' _ [] = error $ "no such pattern " ++ v
    extract' n (Bit _:xs) = extract' (n+1) xs
    extract' n (Var v':xs)
      | v /= v' = extract' (n + length v) xs
      | otherwise = Bits.mask (length v) .&. (b `shiftR` n)

extractAll :: [Expr] -> Word8 -> Maybe [Word8]
extractAll pat b = map (extract pat b) (vars pat) <$ guard (b `matches` pat)

toExpQ :: [Expr] -> ExpQ
toExpQ xs = undefined

toViewP :: [Expr] -> PatQ
toViewP = viewP <$> toViewExpQ <*> toViewPatQ

toViewExpQ :: [Expr] -> ExpQ
toViewExpQ xs = undefined

toViewPatQ :: [Expr] -> PatQ
toViewPatQ xs = tupP [ varP (mkName n) | Var n <- xs]

vars :: [Expr] -> [String]
vars p = [n | Var n <- p]

toExpr :: String -> Expr
toExpr "0" = Bit False
toExpr "1" = Bit True
toExpr s
  | all isLower s = Var s
  | otherwise = error $ "error: " ++ show s ++ " is not a valid variable name"

parse :: String -> [Expr]
parse = map toExpr . groupBy exps
  where exps '0' '0' = False
        exps '1' '1' = False
        exps  c   d  = c == d
