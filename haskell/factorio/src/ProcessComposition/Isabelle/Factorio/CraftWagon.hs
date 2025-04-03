{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.Factorio.CraftWagon(makeWagon) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.Factorio.TestingPrelims;
import qualified ProcessComposition.Isabelle.Factorio.Manufacturing;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Process;
import qualified ProcessComposition.Isabelle.Factorio.Machine;
import qualified ProcessComposition.Isabelle.Sum_Type;
import qualified ProcessComposition.Isabelle.Factorio.Item;
import qualified ProcessComposition.Isabelle.Rat;
import qualified ProcessComposition.Isabelle.Arith;

makeWagon ::
  String ->
    String ->
      String ->
        String ->
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
makeWagon lIPlateIn lIPlateOut lSPlateIn lSPlateOut lGearIn lGearOut lWagonIn
  lWagonOut =
  ProcessComposition.Isabelle.Process.seq_process_list
    [ProcessComposition.Isabelle.Process.Par
       (ProcessComposition.Isabelle.Factorio.Manufacturing.perform
         ProcessComposition.Isabelle.Factorio.TestingPrelims.smeltIronOre
         (ProcessComposition.Isabelle.Arith.nat_of_integer (224 :: Integer))
         lIPlateIn lIPlateOut)
       (ProcessComposition.Isabelle.Process.Identity
         (ProcessComposition.Isabelle.Resource.resource_par
           (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
             ProcessComposition.Isabelle.Factorio.TestingPrelims.electric_furnace
             (ProcessComposition.Isabelle.Arith.nat_of_integer (160 :: Integer))
             lSPlateIn lSPlateOut)
           (ProcessComposition.Isabelle.Resource.resource_par
             (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
               ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
               (ProcessComposition.Isabelle.Arith.nat_of_integer
                 (10 :: Integer))
               lGearIn lGearOut)
             (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
               ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
               (ProcessComposition.Isabelle.Arith.nat_of_integer (2 :: Integer))
               lWagonIn lWagonOut)))),
      ProcessComposition.Isabelle.Process.Par
        (ProcessComposition.Isabelle.Process.map_process
          ProcessComposition.Isabelle.Sum_Type.Inl id id
          ProcessComposition.Isabelle.Sum_Type.Inl
          (ProcessComposition.Isabelle.Factorio.Item.splits
            ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
            lIPlateOut
            [ProcessComposition.Isabelle.Rat.of_int
               (ProcessComposition.Isabelle.Arith.Int_of_integer
                 (20 :: Integer)),
              ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (20 :: Integer)),
              ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (100 :: Integer))]))
        (ProcessComposition.Isabelle.Process.Identity
          (ProcessComposition.Isabelle.Resource.resource_par
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
              ProcessComposition.Isabelle.Factorio.TestingPrelims.electric_furnace
              (ProcessComposition.Isabelle.Arith.nat_of_integer
                (160 :: Integer))
              lSPlateIn lSPlateOut)
            (ProcessComposition.Isabelle.Resource.resource_par
              (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
                ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
                (ProcessComposition.Isabelle.Arith.nat_of_integer
                  (10 :: Integer))
                lGearIn lGearOut)
              (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
                ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
                (ProcessComposition.Isabelle.Arith.nat_of_integer
                  (2 :: Integer))
                lWagonIn lWagonOut)))),
      ProcessComposition.Isabelle.Process.par_process_list
        [ProcessComposition.Isabelle.Process.map_process
           ProcessComposition.Isabelle.Sum_Type.Inl id id
           ProcessComposition.Isabelle.Sum_Type.Inl
           (ProcessComposition.Isabelle.Factorio.Item.move
             ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
             (ProcessComposition.Isabelle.Rat.of_int
               (ProcessComposition.Isabelle.Arith.Int_of_integer
                 (20 :: Integer)))
             lIPlateOut lGearIn),
          ProcessComposition.Isabelle.Process.map_process
            ProcessComposition.Isabelle.Sum_Type.Inl id id
            ProcessComposition.Isabelle.Sum_Type.Inl
            (ProcessComposition.Isabelle.Factorio.Item.move
              ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (20 :: Integer)))
              lIPlateOut lWagonIn),
          ProcessComposition.Isabelle.Process.map_process
            ProcessComposition.Isabelle.Sum_Type.Inl id id
            ProcessComposition.Isabelle.Sum_Type.Inl
            (ProcessComposition.Isabelle.Factorio.Item.move
              ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (100 :: Integer)))
              lIPlateOut lSPlateIn),
          ProcessComposition.Isabelle.Process.Identity
            (ProcessComposition.Isabelle.Resource.resource_par
              (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
                ProcessComposition.Isabelle.Factorio.TestingPrelims.electric_furnace
                (ProcessComposition.Isabelle.Arith.nat_of_integer
                  (160 :: Integer))
                lSPlateIn lSPlateOut)
              (ProcessComposition.Isabelle.Resource.resource_par
                (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
                  ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
                  (ProcessComposition.Isabelle.Arith.nat_of_integer
                    (10 :: Integer))
                  lGearIn lGearOut)
                (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
                  ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
                  (ProcessComposition.Isabelle.Arith.nat_of_integer
                    (2 :: Integer))
                  lWagonIn lWagonOut)))],
      ProcessComposition.Isabelle.Process.par_process_list
        [ProcessComposition.Isabelle.Process.Identity
           (ProcessComposition.Isabelle.Factorio.TestingPrelims.rFlow
             ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
             (ProcessComposition.Isabelle.Rat.of_int
               (ProcessComposition.Isabelle.Arith.Int_of_integer
                 (20 :: Integer)))
             lGearIn),
          ProcessComposition.Isabelle.Process.Swap
            (ProcessComposition.Isabelle.Resource.resource_par
              (ProcessComposition.Isabelle.Factorio.TestingPrelims.rFlow
                ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
                (ProcessComposition.Isabelle.Rat.of_int
                  (ProcessComposition.Isabelle.Arith.Int_of_integer
                    (20 :: Integer)))
                lWagonIn)
              (ProcessComposition.Isabelle.Resource.resource_par
                (ProcessComposition.Isabelle.Factorio.TestingPrelims.rFlow
                  ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
                  (ProcessComposition.Isabelle.Rat.of_int
                    (ProcessComposition.Isabelle.Arith.Int_of_integer
                      (100 :: Integer)))
                  lSPlateIn)
                (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
                  ProcessComposition.Isabelle.Factorio.TestingPrelims.electric_furnace
                  (ProcessComposition.Isabelle.Arith.nat_of_integer
                    (160 :: Integer))
                  lSPlateIn lSPlateOut)))
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
              ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
              (ProcessComposition.Isabelle.Arith.nat_of_integer (10 :: Integer))
              lGearIn lGearOut),
          ProcessComposition.Isabelle.Process.Identity
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
              ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
              (ProcessComposition.Isabelle.Arith.nat_of_integer (2 :: Integer))
              lWagonIn lWagonOut)],
      ProcessComposition.Isabelle.Process.par_process_list
        [ProcessComposition.Isabelle.Factorio.Manufacturing.perform
           ProcessComposition.Isabelle.Factorio.TestingPrelims.craftGear
           (ProcessComposition.Isabelle.Arith.nat_of_integer (10 :: Integer))
           lGearIn lGearOut,
          ProcessComposition.Isabelle.Process.Identity
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rFlow
              ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (20 :: Integer)))
              lWagonIn),
          ProcessComposition.Isabelle.Factorio.Manufacturing.perform
            ProcessComposition.Isabelle.Factorio.TestingPrelims.smeltIronPlate
            (ProcessComposition.Isabelle.Arith.nat_of_integer (160 :: Integer))
            lSPlateIn lSPlateOut,
          ProcessComposition.Isabelle.Process.Identity
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
              ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
              (ProcessComposition.Isabelle.Arith.nat_of_integer (2 :: Integer))
              lWagonIn lWagonOut)],
      ProcessComposition.Isabelle.Process.par_process_list
        [ProcessComposition.Isabelle.Process.map_process
           ProcessComposition.Isabelle.Sum_Type.Inl id id
           ProcessComposition.Isabelle.Sum_Type.Inl
           (ProcessComposition.Isabelle.Factorio.Item.move
             ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_gear
             (ProcessComposition.Isabelle.Rat.of_int
               (ProcessComposition.Isabelle.Arith.Int_of_integer
                 (10 :: Integer)))
             lGearOut lWagonIn),
          ProcessComposition.Isabelle.Process.Identity
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rFlow
              ProcessComposition.Isabelle.Factorio.TestingPrelims.iron_plate
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (20 :: Integer)))
              lWagonIn),
          ProcessComposition.Isabelle.Process.map_process
            ProcessComposition.Isabelle.Sum_Type.Inl id id
            ProcessComposition.Isabelle.Sum_Type.Inl
            (ProcessComposition.Isabelle.Factorio.Item.move
              ProcessComposition.Isabelle.Factorio.TestingPrelims.steel_plate
              (ProcessComposition.Isabelle.Rat.of_int
                (ProcessComposition.Isabelle.Arith.Int_of_integer
                  (20 :: Integer)))
              lSPlateOut lWagonIn),
          ProcessComposition.Isabelle.Process.Identity
            (ProcessComposition.Isabelle.Factorio.TestingPrelims.rCentre
              ProcessComposition.Isabelle.Factorio.TestingPrelims.assembling_machine_1
              (ProcessComposition.Isabelle.Arith.nat_of_integer (2 :: Integer))
              lWagonIn lWagonOut)],
      ProcessComposition.Isabelle.Factorio.Manufacturing.perform
        ProcessComposition.Isabelle.Factorio.TestingPrelims.craftWagon
        (ProcessComposition.Isabelle.Arith.nat_of_integer (2 :: Integer))
        lWagonIn lWagonOut];

}
