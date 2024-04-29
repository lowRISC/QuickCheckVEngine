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
  simpleInterruptTest
) where

import Data.Bits
import QuickCheckVEngine.Instrlike
import QuickCheckVEngine.Template
import RISCV.RV32_I
import RISCV.RV32_Zicsr
import RISCV.RV_CSRs
import RISCV.RV32_Xcheri

simpleInterruptTest :: Template
simpleInterruptTest = random $ do
  return $ mconcat [
                     inst $ auipc 8 4, -- load address, offset from pcc
                     inst $ cspecialrw 0 28 8, -- write new trap address in mtcc
                     inst $ cgettag 8 8, -- check tag of new trap
                     inst $ addi 9 0 ((1 `shiftL` 11) - 1), -- Set bit 11 only (without unintended sign extension)
                     inst $ addi 9 9 1,                     -- Set bit 11 only (without unintended sign extension)
                     inst $ csrrs 0 (unsafe_csrs_indexFromName "mie") 9, -- set Machine External Interrupt Enable
                     inst $ csrrsi 0 (unsafe_csrs_indexFromName "mstatus") (1 `shiftL` 3) , -- set global Machine Interrupt Enable
                     intReq 11, -- 11 = Machine External Interrupt
                     intBar,
                     inst $ csrrs 10 (unsafe_csrs_indexFromName "mcause") 0, -- check mcause
                     inst $ csrrs 10 (unsafe_csrs_indexFromName "mip") 0, -- check Machine Interrupts Pending
                     inst $ cspecialrw 10 31 0, -- check mepcc
                     -- TODO: clear Machine External Interrupt Pending
                     inst $ mret, -- return from trap
                     inst $ addi 0 0 0 -- nop
                   ]
