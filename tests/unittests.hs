import Prelude

import Test.Tasty

import qualified Tests.SanityCheck

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.SanityCheck.amISane
  ]
