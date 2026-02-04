{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tests.CpuInstructions where

import Clash.Prelude
import Cpu.Cpu
import Cpu.CpuState
import Cpu.Data
import Cpu.Instructions
import Cpu.Microcode.Map
import qualified Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Tests.CpuGenerator

prop_reset_works :: H.Property
prop_reset_works = H.property $ do
  resetPC <- H.forAll genAddr

  let (pcHigh, pcLow) = splitAddr resetPC

  let op1 : op2 : op3 : _ = microcodeInstructions (JMP, Absolute None)

  let (syncState, _) = cpuWithBus initCpuStateWithBus (0, op1)

  syncState
    H.=== initCpuStateWithBus
      { _cpuState =
          (_cpuState initCpuStateWithBus)
            { _sync_after_reset = False
            }
      }

  let (stateWithBus, directBus) = cpuWithBus syncState (0, op1)

  _regPC (_cpuState stateWithBus) H.=== resetVector + 1
  _addressToQuery directBus H.=== resetVector

  let oldStateWithBus = stateWithBus
  let (stateWithBus, directBus) = cpuWithBus oldStateWithBus (pcLow, op2)

  _regPC (_cpuState stateWithBus) H.=== resetVector + 2
  _addressToQuery directBus H.=== resetVector + 1
  _dataLatch (_cpuState stateWithBus) H.=== pcLow

  let oldStateWithBus = stateWithBus
  let (stateWithBus, directBus) = cpuWithBus oldStateWithBus (pcHigh, op3)

  _regPC (_cpuState stateWithBus) H.=== resetPC + 1
  _addressToQuery directBus H.=== resetPC
  _dataLatch (_cpuState stateWithBus) H.=== pcHigh

cpuInstructionTests :: TestTree
cpuInstructionTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain cpuInstructionTests
