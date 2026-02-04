import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Tests.Alu
import qualified Tests.CpuDecoder
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
        testTreeLimit 10000 Tests.Alu.aluTests,
        testTreeLimit 1000 Tests.CpuDecoder.decodeCoverageTests,
        testTreeLimit 100000 Tests.CpuExecutor.cpuExecutorTests,
        testTreeLimit 10000 Tests.CpuInstructions.cpuInstructionTests
      ]
  where
    testTreeLimit n = localOption (HedgehogTestLimit (Just n))
