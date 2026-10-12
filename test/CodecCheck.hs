--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright lowRISC contributors.
--

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
    Self-check of the instruction encoders and decoders in 'RISCV'.

    'InstrCodec.encode' takes its arguments, and a '-->' right-hand side
    receives its operands, in the order each field variable first appears in
    the format string counting from the MSB. Nothing checks that an encoder's
    arguments or a printer's parameters follow that order: 'encode' zips the
    argument list with the field list (a surplus argument is dropped) and
    truncates each value to its field, so a mistake still builds and still
    produces legal-looking instructions.

    Each entry names an encoder and, in its parameter order, the field each
    parameter must land in. The encoder must route every parameter to that
    field (static check); for random values, the module's disassembly table
    must print the expected text and, for 32-bit instructions with registers,
    'rv_extract' must return them and re-encode the instruction. The 'rv_shrink'
    tables are not checked. The table is hand-written: a new encoder is covered
    only once it is added here.
-}

module Main (main) where

import Control.Monad (filterM, unless)
import Data.Bits ((.&.), (.|.), bit, complement, shiftL, testBit)
import Data.List (elemIndex, intercalate, isInfixOf, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Exit (exitFailure)
import Test.QuickCheck hiding ((.&.))
import Text.Printf (printf)

import InstrCodec ( Encode (..), Instruction (..), DecodeBranch, Token (..), tokenise, tokenWidth
                  , rangedVars, decode )
import RISCV.Helpers (reg, reg', fpReg, fpReg', int, toSigned)
import RISCV.InstInspect (rv_extract, rv_pretty)
import qualified RISCV.RV_C        as C
import qualified RISCV.RV32_I      as I
import qualified RISCV.RV32_M      as M
import qualified RISCV.RV32_Xcheri as X

-- * Probing an encoder

-- | A result type for 'encode' that records the format string and the
--   argument list the encoder passed, instead of building an instruction.
data Probe = Probe String [Integer]

instance Encode Integer Probe where
  enc args fmt = Probe fmt args

-- | An encoder, at its arity. Nullary encoders are monomorphic
--   'Instruction's (monomorphism restriction) and cannot be probed.
data Enc = E0 Instruction
         | E1 (Integer -> Probe)
         | E2 (Integer -> Integer -> Probe)
         | E3 (Integer -> Integer -> Integer -> Probe)

arity :: Enc -> Int
arity (E0 _) = 0
arity (E1 _) = 1
arity (E2 _) = 2
arity (E3 _) = 3

runEnc :: Enc -> [Integer] -> Probe
runEnc (E1 f) [a]       = f a
runEnc (E2 f) [a, b]    = f a b
runEnc (E3 f) [a, b, c] = f a b c
runEnc _ _ = error "runEnc: arity mismatch"

-- | The instruction the encoder builds from these arguments
instrOf :: Enc -> [Integer] -> Instruction
instrOf (E0 i) _  = i
instrOf e      vs = let Probe fmt args = runEnc e vs in enc args fmt

-- * Formats

fields :: String -> [String]
fields = rangedVars . tokenise

fieldMask :: String -> String -> Integer
fieldMask fmt v =
  foldl (.|.) 0 [bit i | Range w hi lo <- tokenise fmt, w == v, i <- [lo .. hi]]

-- | The bits of the encoding that hold the named fields ('tokenise' lists the
--   tokens LSB first)
encodingMask :: String -> [String] -> Integer
encodingMask fmt names = go 0 (tokenise fmt) 0
  where
    go _ [] m = m
    go pos (t : ts) m =
      let w  = tokenWidth t
          m' = case t of
                 Range n _ _ | n `elem` names -> foldl (.|.) m [bit i | i <- [pos .. pos + w - 1]]
                 _                            -> m
      in  go (pos + w) ts m'

-- * Table entries

-- | Field name -> value as the field holds it (masked to the field's bits)
type Env = [(String, Integer)]

type Table = (String, [DecodeBranch String])

data Entry = Entry
  { eName   :: String
  , eEnc    :: Enc
  , eParams :: [String]                      -- ^ field of each encoder parameter, in parameter order
  , eDis    :: Maybe (Table, Env -> String)  -- ^ disassembly table and expected text
  , eAssume :: Env -> Bool                   -- ^ restricts values to this encoding (see 'assuming')
  }

ent :: String -> Enc -> [String] -> Table -> (Env -> String) -> Entry
ent name e ps tbl expect = Entry name e ps (Just (tbl, expect)) (const True)

-- | No disassembly check: the table decodes this encoding as something else
noDis :: String -> Enc -> [String] -> Entry
noDis name e ps = Entry name e ps Nothing (const True)

-- | Values outside this predicate decode as an earlier, overlapping entry of
--   the disassembly table. Fields named *nz* are kept non-zero and fields
--   named *n2* /= 2 by 'genArgs', which infers that from the name.
assuming :: (Env -> Bool) -> Entry -> Entry
assuming p e = e { eAssume = p }

-- ** Expected text, by field name. Register and immediate spelling comes from
--    RISCV.Helpers: the check is which field lands in which operand slot.

val :: Env -> String -> Integer
val env n = fromMaybe (error $ "expected text refers to unknown field " ++ n)
                      (lookup n env)

ops :: String -> [String] -> String
ops m [] = m
ops m os = m ++ " " ++ intercalate ", " os

mem :: String -> String -> String
mem base off = base ++ "[" ++ off ++ "]"

xr, cr, fr, cfr, un, scr :: Env -> String -> String
xr  v n = reg    (val v n)         -- integer register
cr  v n = reg'   (val v n)         -- RVC 3-bit integer register
fr  v n = fpReg  (val v n)         -- FP register
cfr v n = fpReg' (val v n)         -- RVC 3-bit FP register
un  v n = int    (val v n)         -- unsigned immediate
scr v n = X.name_scr (val v n)     -- special capability register

-- | Signed immediate of width w
sg :: Int -> Env -> String -> String
sg w v n = int (toSigned w (val v n))

-- | Signed immediate of width w held in units of 2 bytes (jal, branches)
sg2 :: Int -> Env -> String -> String
sg2 w v n = int (toSigned w (val v n `shiftL` 1))

-- * The tables

rv32c, rv64c, rv128c, rv32i, rv32m, xcheri :: Table
rv32c  = ("rv_c_disass (Just 1)", C.rv_c_disass (Just 1))
rv64c  = ("rv_c_disass (Just 2)", C.rv_c_disass (Just 2))
rv128c = ("rv_c_disass (Just 3)", C.rv_c_disass (Just 3))
rv32i  = ("rv32_i_disass",        I.rv32_i_disass)
rv32m  = ("rv32_m_disass",        M.rv32_m_disass)
xcheri = ("rv32_xcheri_disass",   X.rv32_xcheri_disass)

-- | Encoders exported by RISCV.RV_C
rvcEntries :: [Entry]
rvcEntries =
  -- Quadrant 0
  [ ent "c_illegal"      (E0 C.c_illegal)      []                   rv32c  $ \_ -> "c.illegal"
  , ent "c_addi4spn"     (E2 C.c_addi4spn)     ["rd'", "nzuimm"]    rv32c  $ \v -> ops "c.addi4spn" [cr v "rd'", un v "nzuimm"]
  , assuming (\v -> val v "rd'" /= 0) $ -- rd' == 0 is c.illegal
    ent "c_addi4spn_res" (E1 C.c_addi4spn_res) ["rd'"]              rv32c  $ \v -> ops "c.addi4spn.res" [cr v "rd'"]
  , ent "c_fld"          (E3 C.c_fld)    ["rd'", "rs1'", "uimm"]    rv32c  $ \v -> ops "c.fld" [cfr v "rd'", mem (cr v "rs1'") (un v "uimm")]
  , ent "c_lq"           (E3 C.c_lq)     ["rd'", "rs1'", "uimm"]    rv128c $ \v -> ops "c.lq"  [cr  v "rd'", mem (cr v "rs1'") (un v "uimm")]
  , ent "c_lw"           (E3 C.c_lw)     ["rd'", "rs1'", "uimm"]    rv32c  $ \v -> ops "c.lw"  [cr  v "rd'", mem (cr v "rs1'") (un v "uimm")]
  , noDis "c_flw"        (E3 C.c_flw)    ["rd'", "rs1'", "uimm"]    -- CHERIoT: the RV32 table decodes this encoding as c.clc
  , ent "c_clc"          (E3 C.c_clc)    ["rd'", "rs1'", "uimm"]    rv32c  $ \v -> ops "c.clc" [cr  v "rd'", mem (cr v "rs1'") (un v "uimm")]
  , ent "c_res_a"        (E1 C.c_res_a)  ["ignr"]                   rv32c  $ \_ -> "c.reserved"
  , ent "c_fsd"          (E3 C.c_fsd)    ["rs1'", "rs2'", "uimm"]   rv32c  $ \v -> ops "c.fsd" [cfr v "rs2'", mem (cr v "rs1'") (un v "uimm")]
  , ent "c_sq"           (E3 C.c_sq)     ["rs1'", "rs2'", "uimm"]   rv128c $ \v -> ops "c.sq"  [cr  v "rs2'", mem (cr v "rs1'") (un v "uimm")]
  , ent "c_sw"           (E3 C.c_sw)     ["rs1'", "rs2'", "uimm"]   rv32c  $ \v -> ops "c.sw"  [cr  v "rs2'", mem (cr v "rs1'") (un v "uimm")]
  , noDis "c_fsw"        (E3 C.c_fsw)    ["rs1'", "rs2'", "uimm"]   -- CHERIoT: the RV32 table decodes this encoding as c.csc
  , ent "c_csc"          (E3 C.c_csc)    ["rs1'", "rs2'", "uimm"]   rv32c  $ \v -> ops "c.csc" [cr  v "rs2'", mem (cr v "rs1'") (un v "uimm")]
  -- Quadrant 1
  , ent "c_nop"          (E0 C.c_nop)          []                     rv32c  $ \_ -> "c.nop"
  , ent "c_nop_hint"     (E1 C.c_nop_hint)     ["nzimm"]              rv32c  $ \v -> ops "c.nop.hint"  [un v "nzimm"]
  , ent "c_addi_hint"    (E1 C.c_addi_hint)    ["rs1_rd_nz"]          rv32c  $ \v -> ops "c.addi.hint" [xr v "rs1_rd_nz"]
  , ent "c_addi"         (E2 C.c_addi)         ["rs1_rd_nz", "nzimm"] rv32c  $ \v -> ops "c.addi"      [xr v "rs1_rd_nz", sg 6 v "nzimm"]
  , ent "c_jal"          (E1 C.c_jal)          ["imm"]                rv32c  $ \v -> ops "c.jal"       [sg 12 v "imm"]
  , ent "c_addiw"        (E2 C.c_addiw)        ["rs1_rd_nz", "imm"]   rv64c  $ \v -> ops "c.addiw"     [xr v "rs1_rd_nz", sg 6 v "imm"]
  , ent "c_addiw_res"    (E1 C.c_addiw_res)    ["imm"]                rv64c  $ \v -> ops "c.addiw.res" [sg 6 v "imm"]
  , ent "c_li"           (E2 C.c_li)           ["rd_nz", "imm"]       rv32c  $ \v -> ops "c.li"        [xr v "rd_nz", sg 6 v "imm"]
  , ent "c_li_hint"      (E1 C.c_li_hint)      ["imm"]                rv32c  $ \v -> ops "c.li.hint"   [sg 6 v "imm"]
  , ent "c_addi16sp_res" (E0 C.c_addi16sp_res) []                     rv32c  $ \_ -> "c.addi16sp.res"
  , ent "c_addi16sp"     (E1 C.c_addi16sp)     ["nzimm"]              rv32c  $ \v -> ops "c.addi16sp"  [sg 10 v "nzimm"]
  , ent "c_lui"          (E2 C.c_lui)          ["rd_nz_n2", "nzimm"]  rv32c  $ \v -> ops "c.lui"       [xr v "rd_nz_n2", sg 18 v "nzimm"]
  , ent "c_lui_hint"     (E1 C.c_lui_hint)     ["nzimm"]              rv32c  $ \v -> ops "c.lui.hint"  [sg 18 v "nzimm"]
  , ent "c_lui_res"      (E1 C.c_lui_res)      ["rd_nz_n2"]           rv32c  $ \v -> ops "c.lui.res"   [xr v "rd_nz_n2"]
  , ent "c_srli64"       (E1 C.c_srli64)       ["rs1'_rd'"]           rv128c $ \v -> ops "c.srli64"      [cr v "rs1'_rd'"]
  , ent "c_srli64_hint"  (E1 C.c_srli64_hint)  ["rs1'_rd'"]           rv32c  $ \v -> ops "c.srli64.hint" [cr v "rs1'_rd'"]
  , ent "c_srli"         (E2 C.c_srli)         ["rs1'_rd'", "nzuimm"] rv32c  $ \v -> ops "c.srli"        [cr v "rs1'_rd'", un v "nzuimm"]
  , ent "c_srli_nse"     (E2 C.c_srli_nse)     ["rs1'_rd'", "nzuimm"] rv32c  $ \v -> ops "c.srli.nse"    [cr v "rs1'_rd'", un v "nzuimm"]
  , ent "c_srai64"       (E1 C.c_srai64)       ["rs1'_rd'"]           rv128c $ \v -> ops "c.srai64"      [cr v "rs1'_rd'"]
  , ent "c_srai64_hint"  (E1 C.c_srai64_hint)  ["rs1'_rd'"]           rv32c  $ \v -> ops "c.srai64.hint" [cr v "rs1'_rd'"]
  , ent "c_srai"         (E2 C.c_srai)         ["rs1'_rd'", "nzuimm"] rv32c  $ \v -> ops "c.srai"        [cr v "rs1'_rd'", un v "nzuimm"]
  , ent "c_srai_nse"     (E2 C.c_srai_nse)     ["rs1'_rd'", "nzuimm"] rv32c  $ \v -> ops "c.srai.nse"    [cr v "rs1'_rd'", un v "nzuimm"]
  , ent "c_andi"         (E2 C.c_andi)         ["rs1'_rd'", "imm"]    rv32c  $ \v -> ops "c.andi"        [cr v "rs1'_rd'", sg 6 v "imm"]
  , ent "c_sub"          (E2 C.c_sub)          ["rs1'_rd'", "rs2'"]   rv32c  $ \v -> ops "c.sub"      [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_xor"          (E2 C.c_xor)          ["rs1'_rd'", "rs2'"]   rv32c  $ \v -> ops "c.xor"      [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_or"           (E2 C.c_or)           ["rs1'_rd'", "rs2'"]   rv32c  $ \v -> ops "c.or"       [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_and"          (E2 C.c_and)          ["rs1'_rd'", "rs2'"]   rv32c  $ \v -> ops "c.and"      [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_subw"         (E2 C.c_subw)         ["rs1'_rd'", "rs2'"]   rv64c  $ \v -> ops "c.subw"     [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_subw_res"     (E2 C.c_subw_res)     ["rs1'_rd'", "rs2'"]   rv32c  $ \v -> ops "c.subw.res" [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_addw"         (E2 C.c_addw)         ["rs1'_rd'", "rs2'"]   rv64c  $ \v -> ops "c.addw"     [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_addw_res"     (E2 C.c_addw_res)     ["rs1'_rd'", "rs2'"]   rv32c  $ \v -> ops "c.addw.res" [cr v "rs1'_rd'", cr v "rs2'"]
  , ent "c_res_b"        (E1 C.c_res_b)        ["ignr"]               rv32c  $ \_ -> "c.reserved"
  , ent "c_res_c"        (E1 C.c_res_c)        ["ignr"]               rv32c  $ \_ -> "c.reserved"
  , ent "c_j"            (E1 C.c_j)            ["imm"]                rv32c  $ \v -> ops "c.j"    [sg 12 v "imm"]
  , ent "c_beqz"         (E2 C.c_beqz)         ["rs1'", "imm"]        rv32c  $ \v -> ops "c.beqz" [cr v "rs1'", sg 9 v "imm"]
  , ent "c_bnez"         (E2 C.c_bnez)         ["rs1'", "imm"]        rv32c  $ \v -> ops "c.bnez" [cr v "rs1'", sg 9 v "imm"]
  -- Quadrant 2
  , ent "c_slli64"       (E1 C.c_slli64)       ["rs1_rd_nz"]           rv128c $ \v -> ops "c.slli64"      [xr v "rs1_rd_nz"]
  , ent "c_slli64_hint0" (E0 C.c_slli64_hint0) []                      rv32c  $ \_ -> "c.slli64.hint"
  , ent "c_slli64_hintx" (E1 C.c_slli64_hintx) ["rs1_rd_nz"]           rv32c  $ \v -> ops "c.slli64.hint" [xr v "rs1_rd_nz"]
  , ent "c_slli"         (E2 C.c_slli)         ["rs1_rd_nz", "nzuimm"] rv32c  $ \v -> ops "c.slli"        [xr v "rs1_rd_nz", un v "nzuimm"]
  , assuming (\v -> not (testBit (val v "nzuimm") 5)) $ -- shamt[5] set is c.slli.nse x0 on RV32
    ent "c_slli_hint"    (E1 C.c_slli_hint)    ["nzuimm"]              rv32c  $ \v -> ops "c.slli.hint"   [un v "nzuimm"]
  , ent "c_slli_nse"     (E2 C.c_slli_nse)     ["rs1_rd_nz", "nzuimm"] rv32c  $ \v -> ops "c.slli.nse"    [xr v "rs1_rd_nz", un v "nzuimm"]
  , ent "c_fldsp"        (E2 C.c_fldsp)        ["rd", "uimm"]          rv32c  $ \v -> ops "c.fldsp"    [fr v "rd", un v "uimm"]
  , ent "c_lqsp"         (E2 C.c_lqsp)         ["rd_nz", "uimm"]       rv128c $ \v -> ops "c.lqsp"     [xr v "rd_nz", un v "uimm"]
  , ent "c_lwsp"         (E2 C.c_lwsp)         ["rd_nz", "uimm"]       rv32c  $ \v -> ops "c.lwsp"     [xr v "rd_nz", un v "uimm"]
  , ent "c_lwsp_res"     (E1 C.c_lwsp_res)     ["uimm"]                rv32c  $ \v -> ops "c.lwsp.res" [un v "uimm"]
  , ent "c_flwsp"        (E2 C.c_flwsp)        ["rd", "uimm"]          rv32c  $ \v -> ops "c.flwsp"    [fr v "rd", un v "uimm"]
  , ent "c_ldsp"         (E2 C.c_ldsp)         ["rd_nz", "uimm"]       rv64c  $ \v -> ops "c.ldsp"     [xr v "rd_nz", un v "uimm"]
  , ent "c_ldsp_res"     (E1 C.c_ldsp_res)     ["uimm"]                rv64c  $ \v -> ops "c.ldsp.res" [un v "uimm"]
  , ent "c_jr_res"       (E0 C.c_jr_res)       []                      rv32c  $ \_ -> "c.jr.res"
  , ent "c_jr"           (E1 C.c_jr)           ["rs1_nz"]              rv32c  $ \v -> ops "c.jr"       [xr v "rs1_nz"]
  , ent "c_mv_hint"      (E1 C.c_mv_hint)      ["rs2_nz"]              rv32c  $ \v -> ops "c.mv.hint"  [xr v "rs2_nz"]
  , ent "c_mv"           (E2 C.c_mv)           ["rd_nz", "rs2_nz"]     rv32c  $ \v -> ops "c.mv"       [xr v "rd_nz", xr v "rs2_nz"]
  , ent "c_ebreak"       (E0 C.c_ebreak)       []                      rv32c  $ \_ -> "c.ebreak"
  , ent "c_jalr"         (E1 C.c_jalr)         ["rs1_nz"]              rv32c  $ \v -> ops "c.jalr"     [xr v "rs1_nz"]
  , ent "c_add_hint"     (E1 C.c_add_hint)     ["rs2_nz"]              rv32c  $ \v -> ops "c.add.hint" [xr v "rs2_nz"]
  , ent "c_add"          (E2 C.c_add)          ["rs1_rd_nz", "rs2_nz"] rv32c  $ \v -> ops "c.add"      [xr v "rs1_rd_nz", xr v "rs2_nz"]
  , ent "c_fsdsp"        (E2 C.c_fsdsp)        ["rs2", "uimm"]         rv32c  $ \v -> ops "c.fsdsp"    [fr v "rs2", un v "uimm"]
  , ent "c_sqsp"         (E2 C.c_sqsp)         ["rs2", "uimm"]         rv128c $ \v -> ops "c.sqsp"     [xr v "rs2", un v "uimm"]
  , ent "c_swsp"         (E2 C.c_swsp)         ["rs2", "uimm"]         rv32c  $ \v -> ops "c.swsp"     [xr v "rs2", un v "uimm"]
  , ent "c_fswsp"        (E2 C.c_fswsp)        ["rs2", "uimm"]         rv32c  $ \v -> ops "c.fswsp"    [fr v "rs2", un v "uimm"]
  , ent "c_sdsp"         (E2 C.c_sdsp)         ["rs2", "uimm"]         rv64c  $ \v -> ops "c.sdsp"     [xr v "rs2", un v "uimm"]
  ]

-- | Encoders exported by RISCV.RV32_I
rv32iEntries :: [Entry]
rv32iEntries =
     [ ent n e ["rd", "rs1", "rs2"] rv32i $ \v -> ops n [xr v "rd", xr v "rs1", xr v "rs2"]
     | (n, e) <- [ ("add", E3 I.add), ("slt", E3 I.slt), ("sltu", E3 I.sltu)
                 , ("and", E3 I.and), ("or", E3 I.or), ("xor", E3 I.xor)
                 , ("sll", E3 I.sll), ("srl", E3 I.srl), ("sub", E3 I.sub)
                 , ("sra", E3 I.sra) ] ]
  ++ [ ent n e ["rd", "rs1", "imm"] rv32i $ \v -> ops n [xr v "rd", xr v "rs1", sg 12 v "imm"]
     | (n, e) <- [("addi", E3 I.addi), ("slti", E3 I.slti), ("jalr", E3 I.jalr)] ]
  ++ [ ent n e ["rd", "rs1", "imm"] rv32i $ \v -> ops n [xr v "rd", xr v "rs1", un v "imm"]
     | (n, e) <- [ ("sltiu", E3 I.sltiu), ("andi", E3 I.andi), ("ori", E3 I.ori)
                 , ("xori", E3 I.xori), ("slli", E3 I.slli), ("srli", E3 I.srli)
                 , ("srai", E3 I.srai) ] ]
  ++ [ ent n e ["rd", "imm"] rv32i $ \v -> ops n [xr v "rd", un v "imm"]
     | (n, e) <- [("lui", E2 I.lui), ("auipc", E2 I.auipc)] ]
  ++ [ ent "jal" (E2 I.jal) ["rd", "imm"] rv32i $ \v -> ops "jal" [xr v "rd", sg2 21 v "imm"] ]
  ++ [ ent n e ["rs1", "rs2", "im"] rv32i $ \v -> ops n [xr v "rs1", xr v "rs2", sg2 13 v "im"]
     | (n, e) <- [ ("beq", E3 I.beq), ("bne", E3 I.bne), ("blt", E3 I.blt)
                 , ("bltu", E3 I.bltu), ("bge", E3 I.bge), ("bgeu", E3 I.bgeu) ] ]
  ++ [ ent n e ["rd", "rs1", "imm"] rv32i $ \v -> ops n [xr v "rd", mem (xr v "rs1") (sg 12 v "imm")]
     | (n, e) <- [ ("lb", E3 I.lb), ("lbu", E3 I.lbu), ("lh", E3 I.lh)
                 , ("lhu", E3 I.lhu), ("lw", E3 I.lw) ] ]
  ++ [ ent n e ["rs1", "rs2", "imm"] rv32i $ \v -> ops n [xr v "rs2", mem (xr v "rs1") (sg 12 v "imm")]
     | (n, e) <- [("sb", E3 I.sb), ("sh", E3 I.sh), ("sw", E3 I.sw)] ]
  ++ [ ent "fence"  (E2 I.fence)  ["pred", "succ"] rv32i $ \v -> ops "fence" [un v "pred", un v "succ"]
     , ent "sfence" (E2 I.sfence) ["rs1", "rs2"]   rv32i $ \v -> ops "sfence.vma" [xr v "rs1", xr v "rs2"]
     , ent "fence_tso" (E0 I.fence_tso) [] rv32i $ \_ -> "fence.tso"
     , ent "resrvd"    (E0 I.resrvd)    [] rv32i $ \_ -> "reserved"
     , ent "mret"      (E0 I.mret)      [] rv32i $ \_ -> "mret"
     , ent "ecall"     (E0 I.ecall)     [] rv32i $ \_ -> "ecall"
     , ent "ebreak"    (E0 I.ebreak)    [] rv32i $ \_ -> "ebreak" ]

-- | Encoders exported by RISCV.RV32_M
rv32mEntries :: [Entry]
rv32mEntries =
  [ ent n e ["rd", "rs1", "rs2"] rv32m $ \v -> ops n [xr v "rd", xr v "rs1", xr v "rs2"]
  | (n, e) <- [ ("mul", E3 M.mul), ("mulh", E3 M.mulh), ("mulhsu", E3 M.mulhsu)
              , ("mulhu", E3 M.mulhu), ("div", E3 M.div), ("divu", E3 M.divu)
              , ("rem", E3 M.rem), ("remu", E3 M.remu) ] ]

-- | Encoders exported by RISCV.RV32_Xcheri
xcheriEntries :: [Entry]
xcheriEntries =
     [ ent n e ["rd", "cs1"] xcheri $ \v -> ops n [xr v "rd", xr v "cs1"]
     | (n, e) <- [ ("cgetperm", E2 X.cgetperm), ("cgettype", E2 X.cgettype)
                 , ("cgetbase", E2 X.cgetbase), ("cgetlen", E2 X.cgetlen)
                 , ("cgettag", E2 X.cgettag), ("cgetaddr", E2 X.cgetaddr)
                 , ("cgethigh", E2 X.cgethigh), ("cgettop", E2 X.cgettop)
                 , ("cloadtags", E2 X.cloadtags) ] ]
  ++ [ ent n e ["cd", "cs1"] xcheri $ \v -> ops n [xr v "cd", xr v "cs1"]
     | (n, e) <- [("ccleartag", E2 X.ccleartag), ("cmove", E2 X.cmove)] ]
  ++ [ ent n e ["rd", "rs1"] xcheri $ \v -> ops n [xr v "rd", xr v "rs1"]
     | (n, e) <- [ ("croundrepresentablelength", E2 X.croundrepresentablelength)
                 , ("crepresentablealignmentmask", E2 X.crepresentablealignmentmask) ] ]
  ++ [ ent n e ["cd", "cs1", "cs2"] xcheri $ \v -> ops n [xr v "cd", xr v "cs1", xr v "cs2"]
     | (n, e) <- [("cseal", E3 X.cseal), ("cunseal", E3 X.cunseal), ("csub", E3 X.csub)] ]
  ++ [ ent n e ["cd", "cs1", "rs2"] xcheri $ \v -> ops n [xr v "cd", xr v "cs1", xr v "rs2"]
     | (n, e) <- [ ("candperm", E3 X.candperm), ("csetaddr", E3 X.csetaddr)
                 , ("csethigh", E3 X.csethigh), ("cincaddr", E3 X.cincaddr)
                 , ("csetbounds", E3 X.csetbounds)
                 , ("csetboundsexact", E3 X.csetboundsexact) ] ]
  ++ [ ent n e ["rd", "cs1", "cs2"] xcheri $ \v -> ops n [xr v "rd", xr v "cs1", xr v "cs2"]
     | (n, e) <- [("ctestsubset", E3 X.ctestsubset), ("csetequalexact", E3 X.csetequalexact)] ]
  ++ [ ent n e ["cd", "cs1", "imm"] xcheri $ \v -> ops n [xr v "cd", xr v "cs1", un v "imm"]
     | (n, e) <- [ ("cincaddrimm", E3 X.cincaddrimm)
                 , ("csetboundsimmediate", E3 X.csetboundsimmediate) ] ]
  ++ [ ent "cspecialrw" (E3 X.cspecialrw) ["cd", "cSP", "cs1"] xcheri $ \v -> ops "cspecialrw" [xr v "cd", scr v "cSP", xr v "cs1"]
     , ent "auicgp"     (E2 X.auicgp)     ["cd", "imm"]        xcheri $ \v -> ops "auicgp" [xr v "cd", un v "imm"]
     , ent "clc"        (E3 X.clc)        ["cd", "cs1", "imm"] xcheri $ \v -> ops "clc" [xr v "cd", mem (xr v "cs1") (sg 12 v "imm")]
     , ent "csc"        (E3 X.csc)        ["cs2", "cs1", "imm"] xcheri $ \v -> ops "csc" [xr v "cs2", mem (xr v "cs1") (sg 12 v "imm")] ]

entries :: [Entry]
entries = rvcEntries ++ rv32iEntries ++ rv32mEntries ++ xcheriEntries

-- * Static check

-- | The encoder must pass one argument per field, routing parameter k to the
--   field the table names for it. Returns the errors, or the format string.
staticCheck :: Entry -> Either String String
staticCheck e@Entry{ eEnc = E0 _ }
  | null (eParams e) = Right ""  -- a nullary encoder has no format to probe
  | otherwise        = Left (eName e ++ ": the test table names parameters for a nullary encoder\n")
staticCheck e =
  let ec = eEnc e
      n  = arity ec
      Probe fmt args = runEnc ec (map fromIntegral [0 .. n - 1])
      fs = fields fmt
      ps = eParams e
      pname i = if i >= 0 && i < length ps then ps !! i else "?"
      param i = printf "parameter %d (%s)" (i + 1) (pname i) :: String
      hdr = printf "%s: format \"%s\", fields %s" (eName e) fmt (show fs) :: String
      arityErr = [ printf "  the test table names %d parameter(s) %s for an encoder of arity %d"
                          (length ps) (show ps) n
                 | length ps /= n ]
      namesErr = [ printf "  the test table's parameter names %s are not the format's fields"
                          (show ps)
                 | null arityErr, sort ps /= sort fs ]
      countErr = [ printf "  the encoder passes %d argument(s) to encode, the format has %d field(s): %s"
                          (length args) (length fs)
                          (intercalate ", " (map (param . fromInteger) args))
                 | length args /= length fs ]
      routeErr = [ printf "  field %s receives %s, expected %s" f (param a') want
                 | null (arityErr ++ namesErr)
                 , (f, a) <- zip fs args
                 , let a' = fromInteger a
                 , let wantIdx = elemIndex f ps
                 , wantIdx /= Just a'
                 , let want = maybe "no parameter" param wantIdx ]
      errs = arityErr ++ namesErr ++ countErr ++ routeErr
  in  if null errs then Right fmt else Left (unlines (hdr : errs))

-- * Random check

-- | Values for the parameters, masked to their fields by 'mkEnv'. A bounded
--   retry, so that an 'assuming' no value can meet fails instead of hanging.
genArgs :: Entry -> String -> Gen (Maybe [Integer])
genArgs e fmt = draw (1000 :: Int)
  where
    draw 0 = return Nothing
    draw k = do
      vs <- mapM (\p -> choose (0, fieldMask fmt p)) (eParams e)
      if ok vs then return (Just vs) else draw (k - 1)
    ok vs = let env = mkEnv e fmt vs in all valid env && eAssume e env
    valid (p, v) =  (not ("nz" `isInfixOf` p) || v /= 0)
                 && (not ("n2" `isInfixOf` p) || v /= 2)

mkEnv :: Entry -> String -> [Integer] -> Env
mkEnv e fmt vs = [ (p, v .&. fieldMask fmt p) | (p, v) <- zip (eParams e) vs ]

prop_entry :: Entry -> String -> Property
prop_entry e fmt = forAllShow (genArgs e fmt) (maybe "no values" show) $ \mvs -> case mvs of
  Nothing -> counterexample (eName e ++ ": no values satisfy the field constraints and 'assuming'") False
  Just vs ->
    let i   = instrOf (eEnc e) vs
        env = mkEnv e fmt vs
        hdr = printf "%s %s\n  format:   \"%s\"\n  encoding: 0x%08x\n"
                     (eName e) (unwords [p ++ "=" ++ show v | (p, v) <- zip (eParams e) vs])
                     fmt (unMkInstruction i) :: String
        disP = case eDis e of
          Nothing -> property True
          Just ((tname, tbl), expect) ->
            let want = expect env
                got  = decode 32 i tbl
            in  counterexample
                  (hdr ++ printf "  disassembly (%s)\n    expected: %s\n    got:      %s"
                                 tname want (maybe "no match" id got))
                  (got == Just want)
        look ns = listToMaybe [v | (p, v) <- env, p `elem` ns]
        want = (look ["rs2", "cs2"], look ["rs1", "cs1"], look ["rd", "cd"])
        regFields = [p | p <- eParams e, p `elem` ["rs2", "cs2", "rs1", "cs1", "rd", "cd"]]
        keep = complement (encodingMask fmt regFields)
        -- every 32-bit instruction with registers must be in rv_extract
        extractP
          | sum (map tokenWidth (tokenise fmt)) /= 32 || null regFields = property True
          | otherwise = case rv_extract i of
              (bypass, rs2, rs1, rd, reenc) ->
                let i'   = reenc (fromMaybe 0 rs2) (fromMaybe 0 rs1) (fromMaybe 0 rd)
                    regs = choose (0, 31) :: Gen Integer
                in       counterexample
                           (hdr ++ printf "  rv_extract bypass flag = %s, expected %s"
                                          (show bypass) (show (bypassExpected (eName e) env)))
                           (bypass == bypassExpected (eName e) env)
                    .&&. counterexample
                           (hdr ++ printf "  rv_extract (rs2, rs1, rd) = %s, expected %s"
                                          (show (rs2, rs1, rd)) (show want))
                           ((rs2, rs1, rd) == want)
                    .&&. counterexample
                           (hdr ++ printf "  rv_extract re-encodes as 0x%08x" (unMkInstruction i'))
                           (i' == i)
                    -- bypassShrink re-encodes with other registers: they must land in the
                    -- register fields, and no other bit may change
                    .&&. forAll ((,,) <$> regs <*> regs <*> regs) (\(a, b, c) ->
                           let i2 = reenc a b c
                               (_, rs2', rs1', rd', _) = rv_extract i2
                               slot old new = new <$ old
                           in  counterexample
                                 (hdr ++ printf "  re-encoding with (rs2, rs1, rd) = %s gives 0x%08x, extracted as %s"
                                                (show (a, b, c)) (unMkInstruction i2) (show (rs2', rs1', rd')))
                                 (    (rs2', rs1', rd') == (slot rs2 a, slot rs1 b, slot rd c)
                                   && unMkInstruction i2 .&. keep == unMkInstruction i .&. keep))
    in  disP .&&. extractP

-- | rv_pretty: CHERI clc/csc share their encodings with RV64 ld/sd. With no ixl (Show, saved
-- reproducers) or an RV32 trace they must print as clc/csc; only an RV64 trace (ixl 2) gives
-- ld/sd. Only the mnemonic is compared: the operands are the disassembly check's job.
prettyChecks :: [String]
prettyChecks =
  [ printf "rv_pretty %s (ixl %s): expected %s, got \"%s\"\n" n (show ixl) want got
  | (n, i, rv64) <- [("clc", X.clc 9 10 16 :: Instruction, "ld"), ("csc", X.csc 9 10 16, "sd")]
  , (ixl, want) <- [(Nothing, n), (Just 1, n), (Just 2, rv64)]
  , let got = rv_pretty i ixl
  , takeWhile (/= ' ') got /= want ]

-- | rv_extract's bypass flag, which makes bypassShrink replace the instruction by a move: set
-- only for the plain register moves, cmove and addi with immediate 0.
bypassExpected :: String -> Env -> Bool
bypassExpected n v = n == "cmove" || (n == "addi" && val v "imm" == 0)

-- | Cases the random values almost never reach: addi with immediate 0 (the bypass), and the
-- c.slli64.hint (rd == 0) entry in the RV64 and RV128 tables (the entries check RV32 only).
fixedChecks :: [String]
fixedChecks =
     [ printf "rv_extract (addi x%d, x%d, 0): bypass flag False, expected True\n" rd rs1
     | (rd, rs1) <- [(1, 2), (0, 0), (31, 17)] :: [(Integer, Integer)]
     , let (bypass, _, _, _, _) = rv_extract (I.addi rd rs1 0)
     , not bypass ]
  ++ [ printf "%s: c_slli64_hint0 disassembles as %s, expected c.slli64.hint\n" tname (show got)
     | (tname, tbl) <- [rv64c, rv128c]
     , let got = decode 32 (C.c_slli64_hint0 :: Instruction) tbl
     , got /= Just "c.slli64.hint" ]

-- * Driver

main :: IO ()
main = do
  let checked    = [ (e, staticCheck e) | e <- entries ]
      staticErrs = [ err | (_, Left err) <- checked ] ++ prettyChecks ++ fixedChecks
  mapM_ putStr staticErrs
  failed <- flip filterM [ (e, fmt) | (e, Right fmt) <- checked ] $ \(e, fmt) -> do
    let runs = if arity (eEnc e) == 0 then 1 else 200
    r <- quickCheckWithResult stdArgs { maxSuccess = runs, chatty = False } (prop_entry e fmt)
    unless (isSuccess r) (putStrLn (output r))
    return (not (isSuccess r))
  printf "CodecCheck: %d encoders, %d static/fixed-case failure(s), %d random-check failure(s)\n"
         (length entries) (length staticErrs) (length failed)
  unless (null staticErrs && null failed) exitFailure
