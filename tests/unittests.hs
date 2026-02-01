import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Tests.Alu
import qualified Tests.CpuExecutor
import qualified Tests.CpuInstructions
import qualified Tests.SanityCheck
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ testTreeLimit 1 Tests.SanityCheck.amISane,
        testTreeLimit 1000 Tests.CpuInstructions.decodeCoverageTests,
        testTreeLimit 10000 Tests.Alu.aluTests,
        testTreeLimit 100000 Tests.CpuExecutor.cpuExecutorTests
      ]
  where
    testTreeLimit n = localOption (HedgehogTestLimit (Just n))
