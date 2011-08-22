{-# LANGUAGE FlexibleInstances #-}
module Properties where

import Sindre.Sindre

import Test.QuickCheck
import Text.Printf

import Control.Applicative
import Control.Monad
import Data.Monoid

main :: IO ()
main = mapM_ (\(s,a) -> putStr (s++": ") >> a) tests

instance Arbitrary Rectangle where
  arbitrary = pure Rectangle
              <*> choose (0,1000) <*> choose (0,1000)
              <*> choose (0,1000) <*> choose (0,1000)

-- Transposing a rectangle twice is the same as identity.
prop_transposetranspose :: Rectangle -> Bool
prop_transposetranspose r =
  (rectTranspose . rectTranspose) r == r

prop_rectangle_mempty :: Rectangle -> Bool
prop_rectangle_mempty r =
  r `mappend` mempty == r && mempty `mappend` r == r

prop_rectangle_mappend_associative :: Rectangle -> Rectangle -> Rectangle -> Bool
prop_rectangle_mappend_associative r1 r2 r3 =
  r1 `mappend` (r2 `mappend` r3) ==  (r1 `mappend` r2) `mappend` r3

prop_rectangle_mappend_idempotent :: Rectangle -> Rectangle -> Bool
prop_rectangle_mappend_idempotent r1 r2 =
  r1 `mappend` r2 `mappend` r2 == r1 `mappend` r2 &&
  r1 `mappend` r2 `mappend` r1 == r1 `mappend` r2 &&
  r2 `mappend` r1 `mappend` r1 == r1 `mappend` r2 &&
  r2 `mappend` r1 `mappend` r2 == r1 `mappend` r2

instance Arbitrary DimNeed where
  arbitrary = oneof [ liftM Min (choose (0,100))
                    , liftM Max (choose (0,100))
                    , return Unlimited
                    , liftM Exact (choose (0,100)) ]

newtype BigEnoughDim = BigEnoughDim ([DimNeed], Integer)
  deriving (Show)

-- The dimension is guaranteed to be able to satisfy the requirements.
instance Arbitrary BigEnoughDim where
  arbitrary = do needs <- arbitrary
                 let (x1,x2) = foldl (\(x1,y1) (x2,y2) -> (x1+x2,y1+y2)) (0,0)
                               $ map range needs
                 dim <- choose (x1, x2)
                 return $ BigEnoughDim (needs, dim)
    where range (Min x)   = (x,2*x)
          range (Max x)   = (0,x)
          range (Exact x) = (x,x)
          range Unlimited = (0,100)

satisfied :: DimNeed -> Integer -> Bool
satisfied (Min x) y = x <= y
satisfied (Max x) y = x >= y
satisfied (Exact x) y = x == y
satisfied Unlimited _ = True

prop_hsplit_satisfies :: BigEnoughDim  -> Bool
prop_hsplit_satisfies (BigEnoughDim (needs, dim)) =
  length needs == length rs &&
  all (uncurry satisfied) (zip needs $ map rectHeight rs)
  where rs = splitHoriz (Rectangle 0 0 10 dim) needs

prop_hsplit_union :: [DimNeed] -> Rectangle  -> Bool
prop_hsplit_union needs r =
  length needs == length rs && (null needs || mconcat rs == r)
  where rs = splitHoriz r needs

prop_vsplit_satisfies :: BigEnoughDim  -> Bool
prop_vsplit_satisfies (BigEnoughDim (needs, dim)) =
  length needs == length rs &&
  all (uncurry satisfied) (zip needs $ map rectWidth rs)
  where rs = splitVert (Rectangle 0 0 dim 10) needs

prop_vsplit_union :: [DimNeed] -> Rectangle  -> Bool
prop_vsplit_union needs r =
  length needs == length rs && (null needs || mconcat rs == r)
  where rs = splitVert r needs

prop_constrains_idempotent :: SpaceNeed -> Constraints -> Bool
prop_constrains_idempotent s c =
  constrainNeed (constrainNeed s c) c == constrainNeed s c

prop_fitRect_idempotent :: Rectangle -> SpaceNeed -> Bool
prop_fitRect_idempotent r s =
  fitRect (fitRect r s) s == fitRect r s

prop_fitRect_subrect :: Rectangle -> SpaceNeed -> Bool
prop_fitRect_subrect r s =
  fitRect r s `mappend` r == r

prop_fitRect_fits :: Rectangle -> SpaceNeed -> Bool
prop_fitRect_fits r s = check rectWidth (fst s) && check rectHeight (snd s)
  where check f (Exact x) | x <= f r = f (fitRect r s) == x
                          | otherwise = f (fitRect r s) == f r
        check f (Min x) | x <= f r = f (fitRect r s) >= x
                        | otherwise = f (fitRect r s) == f r
        check f (Max x) = f (fitRect r s) <= x
        check _ Unlimited = True

instance Arbitrary Align where
  arbitrary = elements [AlignCenter, AlignNeg, AlignPos]

prop_align_fits :: Align -> Property
prop_align_fits a = do
  minp <- arbitrary `suchThat` (>=(0::Integer))
  d    <- arbitrary `suchThat` (>=0)
  maxp <- arbitrary `suchThat` (>=d+minp)
  let d' = align a minp d maxp
  d' >= minp .&. d' <= maxp .&. d'+d <= maxp .&.
     case a of AlignCenter -> abs ((d'-minp)-(maxp-d'-d)) <= 1
               AlignNeg    -> d'==minp
               AlignPos    -> d'==maxp-d

tests :: [(String, IO ())]
tests  = [ ( "Transposing twice is identity"
           , quickCheck prop_transposetranspose)
         , ( "Rectangle mempty is identity"
           , quickCheck prop_rectangle_mempty)
         , ( "Rectangle mappend is associative"
           , quickCheck prop_rectangle_mappend_associative)
         , ( "Rectangle mappend is idempotent"
           , quickCheck prop_rectangle_mappend_idempotent)
         , ( "Horizontal split fulfills constraints"
           , quickCheck prop_hsplit_satisfies )
         , ( "Union is inverse of horizontal split"
           , quickCheck prop_hsplit_union )
         , ( "Vertical split fulfills constraints"
           , quickCheck prop_vsplit_satisfies )
         , ( "Union is inverse of vertical split"
           , quickCheck prop_vsplit_union )
         , ( "Constraining is idempotent"
           , quickCheck prop_constrains_idempotent )
         , ( "Rectangle fitting is idempotent"
           , quickCheck prop_fitRect_idempotent )
         , ( "Fitted rectangle is a subrectangle"
           , quickCheck prop_fitRect_subrect )
         , ( "Rectangle fitting fits"
           , quickCheck prop_fitRect_fits )
         , ( "Aligning fits and cannot be improved"
           , quickCheck prop_align_fits)
         ]
