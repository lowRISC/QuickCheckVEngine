--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
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

module QuickCheckVEngine.Templates.GenCapRep (
  genRandomCHERIoTCapRepTest
) where

import InstrCodec
import Test.QuickCheck hiding ((.&.))
import RISCV.RV32_Xcheri
import RISCV.RV32_I
import Data.Bits
import RISCV
import InstrCodec
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General

-- Capability bounds setting helper template.
cheriot_set_bounds :: Integer -> Integer -> Integer -> Integer -> Template
cheriot_set_bounds capReg tmpReg base len = random $ do
  return $ li tmpReg base <>
           (inst $ csetaddr capReg capReg tmpReg) <>
           li tmpReg len <>
           (inst $ csetbounds capReg capReg tmpReg)

-- Addresses before, at, and beyond the *lower* representability edge.
cheriot_rep_lo_edge :: Integer -> Integer -> Integer -> Template
cheriot_rep_lo_edge capReg tmpReg cleanReg = random $ do
  base <- bits 32
  exp <- chooseInteger (0, 14)  -- exclude the 'all of memory' special exponent
  scale <- chooseInteger (256, 511)
  steps <- chooseInteger (2, 10)
  let len = (2^exp) * scale
  return $ (cheriot_set_bounds capReg tmpReg base len) <>
           instSeq [ cgetbase tmpReg capReg
                   , addi tmpReg tmpReg (steps `shiftR` 1)
                   , csetaddr capReg capReg tmpReg
                   , addi tmpReg 0 (-1) ] <>
           repeatN (fromInteger steps) (inst $ cincaddr capReg capReg tmpReg) <>
           instSeq [ cmove capReg cleanReg
                   , cmove tmpReg cleanReg ]

-- Addresses before, at, and beyond the *upper* representability edge.
cheriot_rep_hi_edge :: Integer -> Integer -> Integer -> Template
cheriot_rep_hi_edge capReg tmpReg cleanReg = random $ do
  base <- bits 32
  exp <- chooseInteger (0, 14)  -- exclude the 'all of memory' special exponent
  scale <- chooseInteger (256, 511)
  offset <- chooseInteger (-6, -2)
  let expInt = fromIntegral exp
  let alignedBase = ((base `shiftR` expInt) `shiftL` expInt)
  let len = (2^exp) * scale
  let repLen = 2^(exp + 9)
  return $ cheriot_set_bounds capReg tmpReg alignedBase len <>
           instSeq [ cgetbase tmpReg capReg
                   , csetaddr capReg capReg tmpReg ] <>
           li tmpReg repLen <>
           instSeq [ addi tmpReg tmpReg offset
                   , cincaddr capReg capReg tmpReg
                   , addi tmpReg 0 1 ] <>
           repeatN 6 (inst $ cincaddr capReg capReg tmpReg) <>
           instSeq [ cmove capReg cleanReg
                   , cmove tmpReg cleanReg ]

-- Addresses before, at, and beyond the *lower* representability edge via AUIPCC.
-- We have to keep PCC in-bounds to avoid a trap, so use a PC address that is
-- 2^11 (the minimum AUIPCC offset) above the desired address.
cheriot_rep_lo_edge_auipcc :: Integer -> Integer -> Integer -> Template
cheriot_rep_lo_edge_auipcc capReg tmpReg cleanReg = random $ do
  base <- bits 32
  exp <- chooseInteger (3, 14)  -- mostly length > auipcc minimum offset (2^11)
  scale <- chooseInteger (256, 511)
  offset <- chooseInteger (-8, 0)
  let len = (2^exp) * scale
  return $ (inst $ auipc capReg 0) <>
           cheriot_set_bounds capReg tmpReg base len <>
           instSeq [ cgetbase tmpReg capReg
                   , addi tmpReg tmpReg 1024  -- 1024 = 2^10 = 11th bit set.
                   , addi tmpReg tmpReg 1024  -- Fits in 12-bit signed imm.
                   , csetaddr capReg capReg tmpReg
                   , jalr 0 capReg offset ] <>
           repeatN 3 (inst $ auipc tmpReg (-1)) <>
           instSeq [ cmove capReg cleanReg
                   , cmove tmpReg cleanReg ]

-- Addresses before, at, and beyond the *upper* representability edge via AUIPCC.
-- We have to keep PCC in-bounds to avoid a trap, so use a PC address that is
-- a multiple of 2^11 (AUIPCC offset units) below the desired address.
-- How many multiples depends on the distance between the top bound
-- (which we set to the max) and the upper address representability limit.
cheriot_rep_hi_edge_auipcc :: Integer -> Integer -> Integer -> Template
cheriot_rep_hi_edge_auipcc capReg tmpReg cleanReg = random $ do
  base <- bits 32
  exp <- chooseInteger (3, 14) -- keep length > auipcc minimum offset (2^11)
  offset <- chooseInteger (-8, 0)
  let expInt = fromIntegral exp
  let alignedBase = ((base `shiftR` expInt) `shiftL` expInt)
  let auipccImm = if (exp >= 11) then 2^(exp - 11) + 1 else 1
  let maxLen = (2^exp) * 511
  let addrInc = 2^(exp + 9) - (auipccImm * (2^11))
  return $ (inst $ auipc capReg 0) <>
           cheriot_set_bounds capReg tmpReg alignedBase maxLen <>
           li tmpReg addrInc <>
           instSeq [ cincaddr capReg capReg tmpReg
                   , jalr 0 capReg offset ] <>
           repeatN 3 (inst $ auipc tmpReg auipccImm) <>
           instSeq [ cmove capReg cleanReg
                   , cmove tmpReg cleanReg ]

-- Super-template of all the test templates in this file.
-- The templates should use cleanReg to restore the other registers they use
-- to avoid the next template being left with narrow/invalidated capabilities.
genRandomCHERIoTCapRepTest :: Template
genRandomCHERIoTCapRepTest = random $ do
  -- Choose unique random register indices
  regIdxs <- shuffle [1..15]
  let capReg1 = regIdxs !! 0
  let tmpReg1 = regIdxs !! 1
  let cleanReg = regIdxs !! 14
  return $ dist [ (5, cheriot_rep_lo_edge capReg1 tmpReg1 cleanReg)
                , (5, cheriot_rep_hi_edge capReg1 tmpReg1 cleanReg)
                , (3, cheriot_rep_lo_edge_auipcc capReg1 tmpReg1 cleanReg)
                , (3, cheriot_rep_hi_edge_auipcc capReg1 tmpReg1 cleanReg) ]
