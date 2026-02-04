module Tests.CpuDecoder where

import Clash.Prelude
import Cpu.Instructions
import qualified Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Tests.CpuGenerator

-- | Checks whether 'decode' can be executed for all possible instructions.
-- Result of decode is verified only to force evaluation.
prop_should_decode_all_instructions :: H.Property
prop_should_decode_all_instructions = H.property $ do
  opcode <- H.forAll genOpcode
  let instruction = fst $ decode opcode
  -- Just ensure that decode does not crash for any instruction.
  if opcode == 0xEA
    then instruction H.=== NOP
    else NOP H./== instruction

decodeCoverageTests :: TestTree
decodeCoverageTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain decodeCoverageTests
