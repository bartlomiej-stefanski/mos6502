import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Tests.Alu
import qualified Tests.SanityCheck
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ testTreeLimit 1 Tests.SanityCheck.amISane,
        testTreeLimit 10000 Tests.Alu.aluTests
      ]
  where
    testTreeLimit n = localOption (HedgehogTestLimit (Just n))
