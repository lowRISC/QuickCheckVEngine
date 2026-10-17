--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2024 lowRISC contributors
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
--

-- Interrupt tests

module QuickCheckVEngine.Templates.GenInterrupt (
  genInterruptTest
) where

import RISCV.RV32_I
import qualified RISCV.RV32_M as M
import RISCV.RV32_Zicsr
import RISCV.RV32_Xcheri
import RISCV.RV_CSRs
import RISCV.ArchDesc
import QuickCheckVEngine.Instrlike
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General
import QuickCheckVEngine.Templates.GenAll
import Data.Bits
import Test.QuickCheck (Gen, choose, elements, frequency, oneof)

msiFixedTest :: Template
msiFixedTest = random $ do
  return $ mconcat [
                     inst $ auipc 8 4, -- read pcc capability, offset to create some target address
                     inst $ cspecialrw 4 28 8, -- write new trap capability in mtcc
                     inst $ csrrsi 4 (unsafe_csrs_indexFromName "mie") (1 `shiftL` 3) , -- set Machine Software Interrupt Enable
                     inst $ csrrsi 4 (unsafe_csrs_indexFromName "mstatus") (1 `shiftL` 3) , -- set global Machine Interrupt Enable
                     intReq 3, -- 3 = Machine Software Interrupt
                     intBar,
                     inst $ csrrs 10 (unsafe_csrs_indexFromName "mcause") 0, -- check mcause
                     inst $ csrrs 10 (unsafe_csrs_indexFromName "mtval") 0, -- check mtval
                     inst $ csrrs 10 (unsafe_csrs_indexFromName "mip") 0, -- check Machine Interrupts Pending
                     inst $ cspecialrw 10 31 0, -- check mepcc
                     inst $ cspecialrw 5 29 0, -- load mtdc
                     inst $ lui 6 (0x02000000 `shiftR` 12), -- load CLINT base address
                     inst $ csetaddr 7 5 6, -- build capability to CLINT base
                     inst $ lw 10 7 0, -- check Machine Software Interrupt
                     inst $ sw 7 0 0, -- clear Machine Software Interrupt
                     inst $ mret, -- return from trap
                     inst $ addi 0 0 0 -- nop
                   ]

msiRandTest :: Template
msiRandTest = random $ do
  tmpReg1   <- src
  tmpReg2   <- src
  tmpReg3   <- dest
  imm       <- bits 12
  return $ mconcat  [ (noShrink $ instSeq [
                        auipc tmpReg1 0, -- load pcc capability
                        addi tmpReg2 0 ((imm .&. 0xFFF0) `shiftR` 4), -- | load randomised trap address offset,
                        slli tmpReg2 tmpReg2 4,                       -- | fit within in RVFI 64KiB sram (usually)
                        addi tmpReg2 tmpReg2 (imm .&. 0xC),           -- | and 4-byte aligned
                        cincaddr tmpReg3 tmpReg1 tmpReg2, -- create capability to trap entry
                        cspecialrw tmpReg1 28 tmpReg3, -- write new trap capability to mtcc
                        csrrsi tmpReg1 (unsafe_csrs_indexFromName "mie") (1 `shiftL` 3), -- set Machine Software Interrupt Enable
                        csrrsi tmpReg1 (unsafe_csrs_indexFromName "mstatus") (1 `shiftL` 3) -- set global Machine Interrupt Enable
                      ]),
                      genAll,
                      intReq 3, -- set mip[3] (Machine Software Interrupt)
                      intBar,
                      genAll,
                      (noShrink $ instSeq [
                        cspecialrw tmpReg1 29 0, -- load mtdc
                        lui tmpReg2 (0x02000000 `shiftR` 12), -- load CLINT base address
                        csetaddr tmpReg3 tmpReg1 tmpReg2, -- create capability to CLINT base
                        sw tmpReg3 0 0, -- clear Machine Software Interrupt
                        mret -- return from trap
                      ]),
                      genAll
                    ]

-- | Interrupt tests for both flavours. A prologue points the trap vector into RAM (MTCC with
--   CHERI, mtvec otherwise); then interrupt events follow until the test is full ('irqEvent').
--   The interrupt lines are the DII packets: intReq raises one (3 software, 7 timer, 11 external)
--   and keeps it raised, intBar retires a NOP and drops them all. An interrupt the hart takes is
--   taken before that NOP, so what follows the barrier is the handler, wherever the vector
--   points. Fast interrupts and NMI are not driven by the TestRIG bench.
genInterruptTest :: Template
genInterruptTest = readParams $ \p ->
  let cheri = has_cheri (archDesc p)
  in mconcat [ if cheri then trapVectorCheri else trapVectorRV
             , repeatTillEnd (irqEvent cheri) ]

-- | MTCC: a capability to a 16-byte aligned, randomised offset from PCC, as 'msiRandTest' builds.
trapVectorCheri :: Template
trapVectorCheri = random $ do
  tmp1 <- tmpReg
  tmp2 <- tmpReg
  tmp3 <- tmpReg
  imm  <- bits 12
  return $ noShrink $ instSeq
    [ auipc tmp1 0
    , addi tmp2 0 ((imm .&. 0xFFF0) `shiftR` 4)
    , slli tmp2 tmp2 4
    , cincaddr tmp3 tmp1 tmp2
    , cspecialrw 0 28 tmp3 ]

-- | mtvec (0x305, by number: RV_CSRs leaves it unnamed for CHERIoT): vectored mode with a 256-byte
--   aligned base in RAM, the only form Ibex keeps.
trapVectorRV :: Template
trapVectorRV = random $ do
  tmp  <- tmpReg
  page <- choose (0, 0x7fff :: Integer)
  return $ noShrink $ mconcat [ li32 tmp (0x80000001 + page * 0x100), inst $ csrrw 0 0x305 tmp ]

data IrqWait = Plain | Two | Wfi | WfiDisabled deriving Eq

-- | One interrupt event.
--   Plain:       one interrupt, its mie bit set or clear, mstatus.MIE set or clear.
--   Two:         two interrupts, both enabled, raised while mstatus.MIE is clear, then MIE set:
--                the hart takes one with both pending, and the model must pick the same one.
--                (The lines rise one after the other; with MIE set the hart would take the
--                first before the second rose, while the model is given both.)
--   Wfi:         wfi with the interrupt enabled in mie (which alone wakes the hart, whatever
--                mstatus.MIE), then the interrupt.
--   WfiDisabled: wfi with the first interrupt disabled and a second enabled: the first leaves
--                the hart asleep, the second wakes it.
--   The mie bits are set right before each wfi: the random instructions in between may clear
--   them, and a wfi nothing can wake would hang the test. The instruction retired just before the
--   request is random, often a multiply or divide (the instruction class an interrupt follows is
--   covered). Without CHERI, a third of the events run in User mode, where an enabled machine
--   interrupt is taken whatever mstatus.MIE; there they are Plain (Two cannot hold two pending
--   there, and no wfi in User mode, whose legality depends on mstatus.TW), and an ecall returns
--   to Machine mode afterwards. Two, Wfi and WfiDisabled shrink as one unit: a shrunk test that
--   lost the MIE clear would race again, one that kept a wfi but lost its wake-up would hang.
--
--   Privilege: every event starts with an ecall, which traps into Machine mode from either mode, so
--   the event knows where it runs whatever the random instructions before it did (genAll has mret,
--   and CSR writes to mstatus). And only an event whose interrupt is taken gets the handler and
--   its mret: the mret of an interrupt that was not taken returns to mstatus.MPP, User after any
--   earlier mret or ecall from User mode. Without both, a test drifted into User mode while the
--   template assumed Machine mode, the mie writes of a later Wfi event trapped, and the wfi slept
--   for good (2026-10-16, testrig-riscv-interrupt: 43 "No dii_ack" aborts).
irqEvent :: Bool -> Template
irqEvent cheri = random $ do
  tmp     <- tmpReg
  ch      <- elements channels
  other   <- elements (filter (/= ch) channels)
  global  <- frequency [(3, return True), (1, return False)]
  enabled <- frequency [(5, return True), (1, return False)]
  umode   <- if cheri then return False else frequency [(1, return True), (2, return False)]
  kind    <- if umode then return Plain
             else frequency [(4, return Plain), (2, return Two), (2, return Wfi),
                             (1, return WfiDisabled)]
  pre     <- precede
  let mie     = unsafe_csrs_indexFromName "mie"
      mstatus = unsafe_csrs_indexFromName "mstatus"
      mcause  = unsafe_csrs_indexFromName "mcause"
      setMie b = mconcat [li32 tmp (bit (fromInteger b)), inst $ csrrs 0 mie tmp]
      clrMie b = mconcat [li32 tmp (bit (fromInteger b)), inst $ csrrc 0 mie tmp]
      mieFor b on = if on then setMie b else clrMie b
      globalMie = inst $ if global then csrrsi 0 mstatus 8 else csrrci 0 mstatus 8
      -- mcause first: RVFI carries no cause, but the register write is compared with the model,
      -- so taking a different interrupt than the model shows even where every cause has the same
      -- handler address (MTCC).
      handler = mconcat [noShrink $ inst $ csrrs tmp mcause 0, genAll, noShrink $ inst mret]
      enables = case kind of
        Plain       -> mconcat [mieFor ch enabled, globalMie]
        Two         -> mconcat [setMie ch, setMie other, inst $ csrrci 0 mstatus 8]
        Wfi         -> mconcat [setMie ch, globalMie]
        WfiDisabled -> mconcat [clrMie ch, setMie other, globalMie]
      event = case kind of
        Plain       -> mconcat [pre, intReq ch, intBar]
        Two         -> mconcat [intReq ch, intReq other, inst $ csrrsi 0 mstatus 8, intBar]
        Wfi         -> mconcat [inst wfi, intReq ch, intBar]
        WfiDisabled -> mconcat [inst wfi, intReq ch, intReq other, intBar]
      body = if kind `elem` [Two, Wfi, WfiDisabled] then noShrink (enables <> event)
             else noShrink enables <> event
      -- Whether the hart takes the interrupt. Machine interrupts are always enabled in User mode;
      -- in Machine mode only with mstatus.MIE. Two sets MIE after raising both; a wfi wakes on
      -- the enabled one whatever MIE, but traps only with it.
      taken = case kind of
        Plain       -> enabled && (global || umode)
        Two         -> True
        Wfi         -> global
        WfiDisabled -> global
      after = if taken then handler else mempty
      toMachine = noShrink $ inst ecall
  return $ if umode
    then mconcat [ toMachine, noShrink enables, enterUMode tmp, event, after, genAll
                 , noShrink $ inst ecall ]
    else mconcat [ toMachine, body, after, genAll ]
  where channels = [3, 7, 11]

-- | The instruction retired before the interrupt request.
precede :: Gen Template
precede = do
  rd  <- tmpReg
  rs1 <- tmpReg
  rs2 <- tmpReg
  frequency [ (3, return genAll)
            , (2, return $ instUniform [ M.mul rd rs1 rs2, M.div rd rs1 rs2, M.divu rd rs1 rs2
                                       , M.rem rd rs1 rs2 ])
            , (1, return mempty) ]

-- | Machine to User mode as 'genUMode' does: PMP region 0 gets the whole address space (a core
--   with PMP denies User mode every access no region matches), mstatus.MPP := User, mepc := the
--   auipc's address (the next instruction is the next one injected), mret. mepc (0x341), pmpcfg0
--   (0x3a0) and pmpaddr0 (0x3b0) by number, as RV_CSRs leaves them unnamed for CHERIoT.
enterUMode :: Integer -> Template
enterUMode tmp = noShrink $ mconcat
  [ li32 tmp 0xffffffff
  , inst $ csrrw 0 0x3b0 tmp
  , li32 tmp 0x0f
  , inst $ csrrw 0 0x3a0 tmp
  , li32 tmp 0x1800
  , inst $ csrrc 0 (unsafe_csrs_indexFromName "mstatus") tmp
  , inst $ auipc tmp 0
  , inst $ csrrw 0 0x341 tmp
  , inst mret ]

-- | A temporary register both flavours have (x1-x4, x11-x15; CHERIoT has 16).
tmpReg :: Gen Integer
tmpReg = oneof [choose (1, 4), choose (11, 15)]
