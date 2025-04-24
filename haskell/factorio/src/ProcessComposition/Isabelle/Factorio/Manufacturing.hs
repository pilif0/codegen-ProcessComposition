{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Factorio.Manufacturing(Recipe(..), Manu_meta(..),
              stacksPerTimeAtSpeed, recTime, recMach, recOut, recLab, recIn,
              perform)
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
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Process;
import qualified ProcessComposition.Isabelle.Sum_Type;
import qualified ProcessComposition.Isabelle.Factorio.Machine;
import qualified ProcessComposition.Isabelle.Factorio.Item;
import qualified ProcessComposition.Isabelle.Rat;
import qualified ProcessComposition.Isabelle.Arith;

data Recipe =
  Recipe
    [(ProcessComposition.Isabelle.Factorio.Item.Item,
       ProcessComposition.Isabelle.Arith.Nat)]
    [(ProcessComposition.Isabelle.Factorio.Item.Item,
       ProcessComposition.Isabelle.Arith.Nat)]
    ProcessComposition.Isabelle.Rat.Rat
    ProcessComposition.Isabelle.Factorio.Machine.Machine String
  deriving (Prelude.Read, Prelude.Show);

data Manu_meta =
  Perform Recipe ProcessComposition.Isabelle.Arith.Nat String String
  deriving (Prelude.Read, Prelude.Show);

stacksPerTimeAtSpeed ::
  ProcessComposition.Isabelle.Rat.Rat ->
    String ->
      ProcessComposition.Isabelle.Arith.Nat ->
        ProcessComposition.Isabelle.Rat.Rat ->
          (ProcessComposition.Isabelle.Factorio.Item.Item,
            ProcessComposition.Isabelle.Arith.Nat) ->
            ProcessComposition.Isabelle.Factorio.Item.Flow;
stacksPerTimeAtSpeed t l n s (u, v) =
  ProcessComposition.Isabelle.Factorio.Item.Flow u
    (ProcessComposition.Isabelle.Rat.divide_rat
      (ProcessComposition.Isabelle.Rat.times_rat s
        (ProcessComposition.Isabelle.Arith.of_nat
          (ProcessComposition.Isabelle.Arith.times_nat n v)))
      t)
    l;

recTime :: Recipe -> ProcessComposition.Isabelle.Rat.Rat;
recTime (Recipe x1 x2 x3 x4 x5) = x3;

recMach :: Recipe -> ProcessComposition.Isabelle.Factorio.Machine.Machine;
recMach (Recipe x1 x2 x3 x4 x5) = x4;

recOut ::
  Recipe ->
    [(ProcessComposition.Isabelle.Factorio.Item.Item,
       ProcessComposition.Isabelle.Arith.Nat)];
recOut (Recipe x1 x2 x3 x4 x5) = x2;

recLab :: Recipe -> String;
recLab (Recipe x1 x2 x3 x4 x5) = x5;

recIn ::
  Recipe ->
    [(ProcessComposition.Isabelle.Factorio.Item.Item,
       ProcessComposition.Isabelle.Arith.Nat)];
recIn (Recipe x1 x2 x3 x4 x5) = x1;

perform ::
  Recipe ->
    ProcessComposition.Isabelle.Arith.Nat ->
      String ->
        String ->
          ProcessComposition.Isabelle.Process.Process
            (ProcessComposition.Isabelle.Sum_Type.Sum
              ProcessComposition.Isabelle.Factorio.Item.Flow
              ProcessComposition.Isabelle.Factorio.Machine.Mach_block)
            () String
            (ProcessComposition.Isabelle.Sum_Type.Sum
              ProcessComposition.Isabelle.Factorio.Item.Flow_meta Manu_meta);
perform r n l1 l2 =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.resource_par
      (ProcessComposition.Isabelle.Resource.parallel
        (map ((ProcessComposition.Isabelle.Resource.res .
                ProcessComposition.Isabelle.Sum_Type.Inl) .
               stacksPerTimeAtSpeed (recTime r) l1 n
                 (ProcessComposition.Isabelle.Factorio.Machine.machineSpeed
                   (recMach r)))
          (recIn r)))
      (ProcessComposition.Isabelle.Resource.res
        (ProcessComposition.Isabelle.Sum_Type.Inr
          (ProcessComposition.Isabelle.Factorio.Machine.MachBlock (recMach r) n
            l1 l2))))
    (ProcessComposition.Isabelle.Resource.parallel
      (map ((ProcessComposition.Isabelle.Resource.res .
              ProcessComposition.Isabelle.Sum_Type.Inl) .
             stacksPerTimeAtSpeed (recTime r) l2 n
               (ProcessComposition.Isabelle.Factorio.Machine.machineSpeed
                 (recMach r)))
        (recOut r)))
    (recLab r) (ProcessComposition.Isabelle.Sum_Type.Inr (Perform r n l1 l2));

}
