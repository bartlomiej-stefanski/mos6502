module Tests.SanityCheck where

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_am_i_sane :: H.Property
prop_am_i_sane = H.property do
  x <- H.forAll $ Gen.integral $ Range.linear (1 :: Integer) 1000
  y <- H.forAll $ Gen.integral $ Range.linear (1 :: Integer) 1000
  (x + y) H.=== (y + x)

amISane :: TestTree
amISane = $(testGroupGenerator)

main :: IO ()
main = defaultMain amISane
