{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.Factorio.FourGears(fourGears) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.Factorio.TestingPrelims;
import qualified ProcessComposition.Isabelle.Factorio.Manufacturing;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Process;
import qualified ProcessComposition.Isabelle.Factorio.Machine;
import qualified ProcessComposition.Isabelle.Sum_Type;
import qualified ProcessComposition.Isabelle.Factorio.Item;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Rat;

fourGears ::
  String ->
    String ->
      String ->
        String ->
          ProcessComposition.Isabelle.Process.Process
            (ProcessComposition.Isabelle.Sum_Type.Sum
              ProcessComposition.Isabelle.Factorio.Item.Flow
              ProcessComposition.Isabelle.Factorio.Machine.Mach_block)
            () String
            (ProcessComposition.Isabelle.Sum_Type.Sum
              ProcessComposition.Isabelle.Factorio.Item.Flow_meta
              ProcessComposition.Isabelle.Factorio.Manufacturing.Manu_meta);
fourGears lIPlateIn lIPlateOut lGearIn lGearOut =
  ProcessComposition.Isabelle.Process.seq_process_list
    [ProcessComposition.Isabelle.Process.Par
       (ProcessComposition.Isabelle.Factorio.Manufacturing.perform
         ProcessComposition.Isabelle.Factorio.TestingPrelims.smeltIronOre
         (ProcessComposition.Isabelle.Arith.nat_of_integer (13 :: Integer))
         lIPlateIn lIPlateOut)
       (ProcessComposition.Isabelle.Process.Identity
         (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
           ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
           (ProcessComposition.Isabelle.Arith.nat_of_integer (4 :: Integer))
           lGearIn lGearOut)),
      ProcessComposition.Isabelle.Process.Par
        (ProcessComposition.Isabelle.Process.map_process
          ProcessComposition.Isabelle.Sum_Type.Inl id id
          ProcessComposition.Isabelle.Sum_Type.Inl
          (ProcessComposition.Isabelle.Factorio.Item.split
            ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
            lIPlateOut
            (ProcessComposition.Isabelle.Rat.divide_rat
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (125 :: Integer)))
              (ProcessComposition.Isabelle.Arith.power
                (ProcessComposition.Isabelle.Rat.of_int
                  (ProcessComposition.Isabelle.Arith.Int_of_integer
                    (10 :: Integer)))
                (ProcessComposition.Isabelle.Arith.nat_of_integer
                  (3 :: Integer))))
            (ProcessComposition.Isabelle.Rat.of_int
              (ProcessComposition.Isabelle.Arith.Int_of_integer
                (8 :: Integer)))))
        (ProcessComposition.Isabelle.Process.Identity
          (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
            ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
            (ProcessComposition.Isabelle.Arith.nat_of_integer (4 :: Integer))
            lGearIn lGearOut)),
      ProcessComposition.Isabelle.Process.par_process_list
        [ProcessComposition.Isabelle.Process.Identity
           (ProcessComposition.Isabelle.Factorio.TestingPrelims.rFlow
             ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
             (ProcessComposition.Isabelle.Rat.divide_rat
               (ProcessComposition.Isabelle.Rat.of_int
                 (ProcessComposition.Isabelle.Arith.Int_of_integer
                   (125 :: Integer)))
               (ProcessComposition.Isabelle.Arith.power
                 (ProcessComposition.Isabelle.Rat.of_int
                   (ProcessComposition.Isabelle.Arith.Int_of_integer
                     (10 :: Integer)))
                 (ProcessComposition.Isabelle.Arith.nat_of_integer
                   (3 :: Integer))))
             lIPlateOut),
          ProcessComposition.Isabelle.Process.map_process
            ProcessComposition.Isabelle.Sum_Type.Inl id id
            ProcessComposition.Isabelle.Sum_Type.Inl
            (ProcessComposition.Isabelle.Factorio.Item.move
              ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (8 :: Integer)))
              lIPlateOut lGearIn),
          ProcessComposition.Isabelle.Process.Identity
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
              ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
              (ProcessComposition.Isabelle.Arith.nat_of_integer (4 :: Integer))
              lGearIn lGearOut)],
      ProcessComposition.Isabelle.Process.Par
        (ProcessComposition.Isabelle.Process.Identity
          (ProcessComposition.Isabelle.Factorio.TestingPrelims.rFlow
            ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
            (ProcessComposition.Isabelle.Rat.divide_rat
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (125 :: Integer)))
              (ProcessComposition.Isabelle.Arith.power
                (ProcessComposition.Isabelle.Rat.of_int
                  (ProcessComposition.Isabelle.Arith.Int_of_integer
                    (10 :: Integer)))
                (ProcessComposition.Isabelle.Arith.nat_of_integer
                  (3 :: Integer))))
            lIPlateOut))
        (ProcessComposition.Isabelle.Factorio.Manufacturing.perform
          ProcessComposition.Isabelle.Factorio.TestingPrelims.craftGear
          (ProcessComposition.Isabelle.Arith.nat_of_integer (4 :: Integer))
          lGearIn lGearOut)];

}
