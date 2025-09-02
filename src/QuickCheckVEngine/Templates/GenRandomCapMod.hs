--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
-- Copyright (c) 2025 lowRISC contributors
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

module QuickCheckVEngine.Templates.GenRandomCapMod (
  genRandomCHERIoTCapModTest,
  genRandomCHERIoTCSetBoundsExactTest,
  genRandomCHERIoTCSetBoundsImmTest,
  genRandomCHERIoTCIncAddrImmTest,
  genRandomCHERIoTCIncAddrTest,
  genRandomCHERIoTCSetAddrTest,
  genRandomCHERIoTCSetBoundsTest,
  genRandomCHERIoTCSealTest,
  genRandomCHERIoTCUnsealTest,
  genRandomCHERIoTCAndPermTest
) where

import InstrCodec
import Test.QuickCheck
import RISCV.RV32_Xcheri
import RISCV.RV32_I
import RISCV.RV64_I
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils
import Test.QuickCheck
import Data.Bits
import RISCV
import InstrCodec
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General

random_candperm :: Integer -> Integer -> Integer -> Template
random_candperm ca0 ca1 a2 = random $ do
    return $ prepReg32 a2 <> instSeq [ candperm ca0 ca1 a2 ]

random_cclear_tag :: Integer -> Integer -> Template
random_cclear_tag ca0 ca1 = random $ do
    return $ instSeq [ccleartag ca0 ca1]

random_cincaddr :: Integer -> Integer -> Integer -> Template
random_cincaddr ca0 ca1 a2 = random $ do
    return $ prepReg32 a2 <> instSeq [ cincaddr ca0 ca1 a2 ]

random_cincaddrimm :: Integer -> Integer -> Integer -> Template
random_cincaddrimm ca0 ca1 imm = random $ do
    return $ instSeq [ cincaddrimm ca0 ca1 imm ]

random_cseal :: Integer -> Integer -> Integer -> Template
random_cseal ca0 ca1 ca2 = random $ do
    return $ instSeq [ cseal ca0 ca1 ca2 ]

random_csetaddr :: Integer -> Integer -> Integer -> Template
random_csetaddr ca0 ca1 a2 = random $ do
    return $ prepReg32 a2 <> instSeq [ csetaddr ca0 ca1 a2]

random_csetbounds :: Integer -> Integer -> Integer -> Template
random_csetbounds ca0 ca1 a2 = random $ do
    return $ prepReg32 a2 <> instSeq [ csetbounds ca0 ca1 a2 ]

random_csetboundsexact :: Integer -> Integer -> Integer -> Template
random_csetboundsexact ca0 ca1 a2 = random $ do
    return $ prepReg32 a2 <> instSeq [ csetboundsexact ca0 ca1 a2 ]

random_csetboundsimm :: Integer -> Integer -> Integer -> Template
random_csetboundsimm ca0 ca1 imm = random $ do
    return $ instSeq [ csetboundsimmediate ca0 ca1 imm ]

random_cunseal :: Integer -> Integer -> Integer -> Template
random_cunseal ca0 ca1 ca2 = random $ do
    return $ instSeq [ cunseal ca0 ca1 ca2 ]

genRandomCHERIoTCSetBoundsExactTest :: Template
genRandomCHERIoTCSetBoundsExactTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  a2  <- src
  return $ random_csetboundsexact ca0 ca1 a2

genRandomCHERIoTCSetBoundsImmTest :: Template
genRandomCHERIoTCSetBoundsImmTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  imm  <- bits 12
  return $ random_csetboundsimm ca0 ca1 imm

genRandomCHERIoTCIncAddrImmTest :: Template
genRandomCHERIoTCIncAddrImmTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  imm  <- bits 12
  return $ random_cincaddrimm ca0 ca1 imm

genRandomCHERIoTCIncAddrTest :: Template
genRandomCHERIoTCIncAddrTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  a2  <- src
  return $ random_cincaddr ca0 ca1 a2

genRandomCHERIoTCSetAddrTest :: Template
genRandomCHERIoTCSetAddrTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  a2  <- src
  return $ random_csetaddr ca0 ca1 a2

genRandomCHERIoTCSetBoundsTest :: Template
genRandomCHERIoTCSetBoundsTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  a2  <- src
  return $ random_csetbounds ca0 ca1 a2

genRandomCHERIoTCSealTest :: Template
genRandomCHERIoTCSealTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  ca2  <- src
  return $ random_cseal ca0 ca1 ca2

genRandomCHERIoTCUnsealTest :: Template
genRandomCHERIoTCUnsealTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  ca2  <- src
  return $ random_cunseal ca0 ca1 ca2

genRandomCHERIoTCAndPermTest :: Template
genRandomCHERIoTCAndPermTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  a2  <- src
  return $ random_candperm ca0 ca1 a2

genRandomCHERIoTCapModTest :: Template
genRandomCHERIoTCapModTest = readParams $ \param -> random $ do
  let arch = archDesc param
  ca0 <- src
  ca1 <- src
  ca2 <- src
  a2  <- src
  imm <- bits 12
  return $ dist [
                  (1, random_candperm ca0 ca1 a2),
                  (1, random_cclear_tag ca0 ca1),
                  (1, random_cincaddr ca0 ca1 a2),
                  (1, random_cincaddrimm ca0 ca1 imm),
                  (1, random_cseal ca0 ca1 ca2),
                  (1, random_csetaddr ca0 ca1 a2),
                  (1, random_csetbounds ca0 ca1 a2),
                  (1, random_csetboundsexact ca0 ca1 a2),
                  (1, random_csetboundsimm ca0 ca1 imm),
                  (1, random_cunseal ca0 ca1 ca2)
                ]