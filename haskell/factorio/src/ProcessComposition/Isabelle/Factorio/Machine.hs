{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Factorio.Machine(Machine(..), Mach_block(..),
        mblockIn, mblockOut, machineConsu, machineDrain, machineLabel,
        machineSpeed, mblockMach, mblockCount, equal_machine, equal_mach_block)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.Rat;
import qualified ProcessComposition.Isabelle.Arith;

data Machine =
  Machine String ProcessComposition.Isabelle.Rat.Rat
    ProcessComposition.Isabelle.Arith.Nat ProcessComposition.Isabelle.Arith.Nat
  deriving (Prelude.Read, Prelude.Show);

data Mach_block =
  MachBlock Machine ProcessComposition.Isabelle.Arith.Nat String String
  deriving (Prelude.Read, Prelude.Show);

mblockIn :: Mach_block -> String;
mblockIn (MachBlock x1 x2 x3 x4) = x3;

mblockOut :: Mach_block -> String;
mblockOut (MachBlock x1 x2 x3 x4) = x4;

machineConsu :: Machine -> ProcessComposition.Isabelle.Arith.Nat;
machineConsu (Machine x1 x2 x3 x4) = x4;

machineDrain :: Machine -> ProcessComposition.Isabelle.Arith.Nat;
machineDrain (Machine x1 x2 x3 x4) = x3;

machineLabel :: Machine -> String;
machineLabel (Machine x1 x2 x3 x4) = x1;

machineSpeed :: Machine -> ProcessComposition.Isabelle.Rat.Rat;
machineSpeed (Machine x1 x2 x3 x4) = x2;

mblockMach :: Mach_block -> Machine;
mblockMach (MachBlock x1 x2 x3 x4) = x1;

mblockCount :: Mach_block -> ProcessComposition.Isabelle.Arith.Nat;
mblockCount (MachBlock x1 x2 x3 x4) = x2;

equal_machine :: Machine -> Machine -> Bool;
equal_machine (Machine x1 x2 x3 x4) (Machine y1 y2 y3 y4) =
  x1 == y1 &&
    ProcessComposition.Isabelle.Rat.equal_rat x2 y2 &&
      ProcessComposition.Isabelle.Arith.equal_nat x3 y3 &&
        ProcessComposition.Isabelle.Arith.equal_nat x4 y4;

equal_mach_block :: Mach_block -> Mach_block -> Bool;
equal_mach_block (MachBlock x1 x2 x3 x4) (MachBlock y1 y2 y3 y4) =
  equal_machine x1 y1 &&
    ProcessComposition.Isabelle.Arith.equal_nat x2 y2 && x3 == y3 && x4 == y4;

}
