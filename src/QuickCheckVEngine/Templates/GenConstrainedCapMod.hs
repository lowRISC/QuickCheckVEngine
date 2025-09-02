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

module QuickCheckVEngine.Templates.GenConstrainedCapMod (
  genRandomCHERIoTConstrainedCapModTest,
  genRandomCHERIoTConstrainedCAndPermTest,
  genRandomCHERIoTConstrainedCIncAddrImmTest,
  genRandomCHERIoTConstrainedCIncAddrTest,
  genRandomCHERIoTConstrainedCSetAddrTest,
  genRandomCHERIoTConstrainedCSetBoundsExactTest,
  genRandomCHERIoTConstrainedCSetBoundsImmTest,
  genRandomCHERIoTConstrainedCSetBoundsTest,
  genRandomCHERIoTConstrainedCSealTest,
  genRandomCHERIoTConstrainedCUnsealTest
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

inUseRegLow :: Gen Integer
inUseRegLow = oneof [choose (2, 4)]

-- | 'src_low' generates an arbitrary source register index between 2 and 4
src_low :: Gen Integer
src_low = inUseRegLow

inUseRegHigh :: Gen Integer
inUseRegHigh = oneof [choose (11, 15)]

-- | 'src_high' generates an arbitrary source register index between 11 and 15
src_high :: Gen Integer
src_high = inUseRegHigh

-- The following constrained tests all execute a capability modification instruction with constrained random inputs
-- The templates ensure that the capability modification instruction clears the tag bit as infrequently as possible
-- Since DII does not allow for branching, it is impossible to guarantee that the tag bits are never cleared
-- Templates are adapted from: https://github.com/lowRISC/sonata-system/blob/main/sw/cheri/cap_modification_instructions_testing/capability_modification_reference_guide.md

constrained_csetboundsexact :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Template
constrained_csetboundsexact cmem ca0 a2 a3 a4 t0 = random $ do
    return $ prep cmem ca0 a2 <>
             prepReg32 a2 <>
             instSeq [ cgetaddr a4 ca0
                     , cgettop a3 ca0
                     , sub a3 a3 a4
                     , remu a2 a2 a3
                     , addi a3 0 1 -- clz a3 a4, Zbb included in CHERIoT Ibex but not TestRIG
                     , addi t0 0 1
                     , sll t0 t0 a3
                     , addi t0 t0 (-1)
                     , xori t0 t0 (-1) -- not t0
                     , RISCV.and a2 a2 t0
                     , addi t0 0 1
                     , addi a3 a3 9
                     , sll t0 t0 a3
                     , addi t0 t0 (-1)
                     , RISCV.and a2 a2 t0
                     , csetboundsexact ca0 ca0 a2 ] <>
             post cmem ca0 a2

constrained_csetboundsimm :: Integer -> Integer -> Integer -> Integer -> Template
constrained_csetboundsimm cmem ca0 a2 imm = random $ do
    return $ prep cmem ca0 a2 <>
             instSeq [ csetboundsimmediate ca0 ca0 imm ] <>
             post cmem ca0 a2

constrained_cincaddrimm :: Integer -> Integer -> Integer -> Integer -> Template
constrained_cincaddrimm cmem ca0 a2 imm = random $ do
    return $ prep cmem ca0 a2 <>
             instSeq [ cincaddrimm ca0 ca0 imm ] <>
             post cmem ca0 a2

constrained_cincaddr :: Integer -> Integer -> Integer -> Integer -> Integer -> Template
constrained_cincaddr cmem ca0 a2 a3 a4 = random $ do
    return $ prep cmem ca0 a2 <>
             prepReg32 a2 <>
             instSeq [ cgettop a3 ca0
                     , cgetbase a4 ca0
                     , sub a3 a3 a4
                     , remu a2 a2 a3
                     , cgetaddr a3 ca0
                     , sub a3 a3 a4
                     , sub a2 a2 a3
                     , cincaddr ca0 ca0 a2 ] <>
             post cmem ca0 a2

constrained_csetaddr :: Integer -> Integer -> Integer -> Integer -> Integer -> Template
constrained_csetaddr cmem ca0 a2 a3 a4 = random $ do
    return $ prep cmem ca0 a2 <>
             prepReg32 a2 <>
             instSeq [ cgetbase a3 ca0
                     , cgettop a4 ca0
                     , sub a3 a4 a3
                     , remu a2 a2 a3
                     , cgetbase a3 ca0
                     , add a2 a2 a3
                     , csetaddr ca0 ca0 a2 ] <>
             post cmem ca0 a2

constrained_csetbounds :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Template
constrained_csetbounds cmem ca0 a2 a3 a4 t0 = random $ do
    return $ prep cmem ca0 a2 <>
             prepReg32 a3 <>
             instSeq [ cgetbase a4 ca0
                     , cgettop a4 ca0
                     , cgetaddr t0 ca0
                     , sub a4 a4 t0
                     , remu a2 a3 a4
                     , csetbounds ca0 ca0 a2 ] <>
             post cmem ca0 a2

constrained_cseal :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Template
constrained_cseal cmem ca0 ca1 a2 a3 a4 = random $ do
    --cspecialrw ca1 30 0 moves the sealing root capability into ca1
    return $ prep cmem ca0 a2 <>
             prepReg32 a2 <>
             li a3 7 <>
             li a4 0x100 <>
             instSeq [ cspecialrw ca1 30 0
                     , remu a2 a2 a3
                     , addi a2 a2 9
                     , cgetperm a3 ca0
                     , RISCV.and a3 a4 a3
                     , srli a3 a3 5
                     , sub a2 a2 a3
                     , csetaddr ca1 ca1 a2
                     , cseal ca0 ca0 ca1 ] <>
             post cmem ca0 a2

constrained_cunseal :: Integer -> Integer -> Integer -> Integer -> Template
constrained_cunseal cmem ca0 ca1 a2 = random $ do
    -- cspecialrw ca1 30 0 moves the sealing root capability into ca1
    return $ prep cmem ca0 a2 <>
             instSeq [ cspecialrw ca1 30 0
                     , cgettype a2 ca0
                     , csetaddr ca1 ca1 a2
                     , cunseal ca0 ca0 ca1
                     , cgettag a2 ca0 ] <>
             post cmem ca0 a2

constrained_candperm :: Integer -> Integer -> Integer -> Template
constrained_candperm cmem ca0 a2 = random $ do
    return $ prep cmem ca0 a2 <>
             prepReg32 a2 <>
             instSeq [ candperm ca0 ca0 a2 ] <>
             post cmem ca0 a2


prep :: Integer -> Integer -> Integer -> Template
prep cmem ca0 a2 = random $ do
    return $ instSeq [ lui a2 0x40004
                     , slli a2 a2 1
                     , csetaddr cmem cmem a2
                     , csc ca0 cmem 0 ]

post :: Integer -> Integer -> Integer -> Template
post cmem ca0 a2 = random $ do
    return $ instSeq [ csc ca0 cmem 8
                     , lui a2 0x40004
                     , slli a2 a2 1
                     , csetaddr cmem cmem a2
                     , cgettag a2 ca0
                     , slli a2 a2 3
                     , cincaddr cmem cmem a2
                     , clc ca0 cmem 0 ]

load_executable_capability :: Integer -> Template
load_executable_capability ca0 = random $ do
    return $ instSeq [ cspecialrw ca0 28 0 ]

load_rw_capability :: Integer -> Template
load_rw_capability ca0 = random $ do
    return $ instSeq [ cspecialrw ca0 29 0 ]

genRandomCHERIoTConstrainedCSetBoundsExactTest :: Template
genRandomCHERIoTConstrainedCSetBoundsExactTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  a2  <- src_high
  a3  <- src_high
  a4  <- src_high
  t0  <- src_high
  return $ constrained_csetboundsexact cmem ca0 a2 a3 a4 t0

genRandomCHERIoTConstrainedCSetBoundsImmTest :: Template
genRandomCHERIoTConstrainedCSetBoundsImmTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  a2  <- src_high
  imm <- bits 12
  return $ constrained_csetboundsimm cmem ca0 a2 imm

genRandomCHERIoTConstrainedCIncAddrImmTest :: Template
genRandomCHERIoTConstrainedCIncAddrImmTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  a2  <- src_high
  imm <- bits 12
  return $ constrained_cincaddrimm cmem ca0 a2 imm

genRandomCHERIoTConstrainedCIncAddrTest :: Template
genRandomCHERIoTConstrainedCIncAddrTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  a2  <- src_high
  a3  <- src_high
  a4  <- src_high
  return $ constrained_cincaddr cmem ca0 a2 a3 a4

genRandomCHERIoTConstrainedCSetAddrTest :: Template
genRandomCHERIoTConstrainedCSetAddrTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  a2  <- src_high
  a3  <- src_high
  a4  <- src_high
  return $ constrained_csetaddr cmem ca0 a2 a3 a4

genRandomCHERIoTConstrainedCSetBoundsTest :: Template
genRandomCHERIoTConstrainedCSetBoundsTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  a2  <- src_high
  a3  <- src_high
  a4  <- src_high
  t0  <- src_high
  return $ constrained_csetbounds cmem ca0 a2 a3 a4 t0

genRandomCHERIoTConstrainedCSealTest :: Template
genRandomCHERIoTConstrainedCSealTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  ca1 <- src_low
  a2  <- src_high
  a3  <- src_high
  a4  <- src_high
  return $ constrained_cseal cmem ca0 ca1 a2 a3 a4

genRandomCHERIoTConstrainedCUnsealTest :: Template
genRandomCHERIoTConstrainedCUnsealTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  ca1 <- src_low
  a2  <- src_high
  return $ constrained_cunseal cmem ca0 ca1 a2

genRandomCHERIoTConstrainedCAndPermTest :: Template
genRandomCHERIoTConstrainedCAndPermTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  a2  <- src_high
  return $ constrained_candperm cmem ca0 a2

genRandomCHERIoTConstrainedCapModTest :: Template
genRandomCHERIoTConstrainedCapModTest = readParams $ \param -> random $ do
  let arch = archDesc param
  cmem <- pure 1
  ca0 <- src_low
  ca1 <- src_low
  ca2 <- src_low
  a2  <- src_high
  a3  <- src_high
  a4  <- src_high
  t0  <- src_high
  t1  <- src_high
  imm <- bits 12
  return $ dist [ (5, constrained_cincaddr cmem ca0 a2 a3 a4 )
                , (5, constrained_csetaddr cmem ca0 a2 a3 a4)
                , (5, constrained_csetbounds cmem ca0 a2 a3 a4 t0 )
                , (1, constrained_cseal cmem ca0 ca1 a2 a3 a4)
                , (5, constrained_cunseal cmem ca0 ca1 a2)
                , (1, constrained_candperm cmem ca0 a2)
                , (5, constrained_cincaddrimm cmem ca0 a2 imm)
                , (5, constrained_csetboundsimm cmem ca0 a2 imm)
                , (5, constrained_csetboundsexact cmem ca0 a2 a3 a4 t0)
                , (1, load_executable_capability ca0)
                , (1, load_rw_capability ca0)
                , (1, constrained_cseal cmem ca0 ca1 a2 a3 a4 <> constrained_cunseal cmem ca0 ca1 a2) -- Paired CSeal and CUnseal
                ]