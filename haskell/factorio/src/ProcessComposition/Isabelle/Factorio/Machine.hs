{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Factorio.Machine(Machine(..), Mach_block(..),
        mblockIn, mblockOut, machineConsu, machineDrain, machineLabel,
        machineSpeed, mblockMach, mblockCount)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
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

}
