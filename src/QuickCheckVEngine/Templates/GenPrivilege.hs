--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2026 lowRISC contributors
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

-- Privilege mode tests: User mode entry, execution and return, for cores with U-mode and without
-- CHERI (CHERIoT is M-mode only).

module QuickCheckVEngine.Templates.GenPrivilege (
  genUMode
) where

import RISCV.RV32_I
import RISCV.RV32_Zicsr
import RISCV.RV_CSRs
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General
import QuickCheckVEngine.Templates.GenAll
import Test.QuickCheck

-- | 'genUMode' enters User mode with an mret and returns to Machine mode with an ecall. Before the
--   mret, mstatus.MPP is cleared (User) and mcounteren gets a random value, so U-mode reads of
--   cycle and instret are allowed or illegal depending on it; after it, random instructions run in
--   U-mode, followed by the counter reads and an mstatus read, which is illegal in U-mode. Any trap
--   returns to M-mode, so only the instructions up to the first one run in U-mode; the reference
--   model follows the same privilege, which the scoreboard compares on every retirement.
--   A core with PMP regions denies User mode every access no region matches, so most tests first
--   give it the whole address space (region 0, TOR up to the top, RWX); the rest leave PMP alone
--   and the first User mode fetch faults.
--   mepc is the auipc's address: the next instruction is the next one injected, wherever mepc
--   points. mepc (0x341), pmpcfg0 (0x3a0) and pmpaddr0 (0x3b0) are given by number, as RV_CSRs
--   leaves them unnamed for CHERIoT.
genUMode :: Template
genUMode = random $ do
  tmp   <- oneof [choose (1, 4), choose (11, 15)]
  tmp2  <- oneof [choose (1, 4), choose (11, 15)]
  ctr   <- elements [0x0, 0x1, 0x4, 0x5, 0xffffffff]
  grant <- frequency [(4, return True), (1, return False)]
  let mstatus    = unsafe_csrs_indexFromName "mstatus"
      mcounteren = unsafe_csrs_indexFromName "mcounteren"
      cycle      = unsafe_csrs_indexFromName "cycle"
      instret    = unsafe_csrs_indexFromName "instret"
      mepc       = 0x341
      pmpcfg0    = 0x3a0
      pmpaddr0   = 0x3b0
      pmpGrant   = if not grant then mempty else mconcat
        [ li32 tmp 0xffffffff
        , inst $ csrrw 0 pmpaddr0 tmp          -- top of region 0
        , li32 tmp 0x0f
        , inst $ csrrw 0 pmpcfg0 tmp ]         -- region 0: TOR, RWX; regions 1-3 off
  return $ mconcat
    [ noShrink $ mconcat
        [ pmpGrant
        , li32 tmp 0x1800                      -- mstatus.MPP
        , inst $ csrrc 0 mstatus tmp           -- MPP := User
        , li32 tmp ctr
        , inst $ csrrw 0 mcounteren tmp        -- counters User mode may read
        , inst $ auipc tmp 0
        , inst $ csrrw 0 mepc tmp
        , inst mret ]                          -- now in User mode
    , genAll
    , noShrink $ instSeq
        [ csrrs tmp2 cycle 0                   -- allowed if mcounteren.CY
        , csrrs tmp2 instret 0                 -- allowed if mcounteren.IR
        , csrrs tmp2 mstatus 0 ]               -- illegal in User mode
    , genAll
    , noShrink $ inst ecall ]                  -- back to Machine mode
