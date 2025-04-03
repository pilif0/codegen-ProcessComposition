{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Factorio.TestingPrelims(rFlow, rCentre, iron_ore,
               assembling_machine_1, iron_plate, iron_gear, craftGear,
               steel_plate, cargo_wagon, craftWagon, electric_furnace,
               smeltIronOre, smeltIronPlate)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.Factorio.Manufacturing;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Factorio.Machine;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Sum_Type;
import qualified ProcessComposition.Isabelle.Factorio.Item;
import qualified ProcessComposition.Isabelle.Rat;

rFlow ::
  forall a.
    ProcessComposition.Isabelle.Factorio.Item.Item ->
      ProcessComposition.Isabelle.Rat.Rat ->
        String ->
          ProcessComposition.Isabelle.Resource.Resource
            (ProcessComposition.Isabelle.Sum_Type.Sum
              ProcessComposition.Isabelle.Factorio.Item.Flow
              ProcessComposition.Isabelle.Factorio.Machine.Mach_block)
            a;
rFlow i r l =
  ProcessComposition.Isabelle.Resource.res
    (ProcessComposition.Isabelle.Sum_Type.Inl
      (ProcessComposition.Isabelle.Factorio.Item.Flow i r l));

rCentre ::
  forall a.
    ProcessComposition.Isabelle.Factorio.Machine.Machine ->
      ProcessComposition.Isabelle.Arith.Nat ->
        String ->
          String ->
            ProcessComposition.Isabelle.Resource.Resource
              (ProcessComposition.Isabelle.Sum_Type.Sum
                ProcessComposition.Isabelle.Factorio.Item.Flow
                ProcessComposition.Isabelle.Factorio.Machine.Mach_block)
              a;
rCentre m n il ol =
  ProcessComposition.Isabelle.Resource.res
    (ProcessComposition.Isabelle.Sum_Type.Inr
      (ProcessComposition.Isabelle.Factorio.Machine.MachBlock m n il ol));

iron_ore :: ProcessComposition.Isabelle.Factorio.Item.Item;
iron_ore = ProcessComposition.Isabelle.Factorio.Item.Item "Iron Ore";

assembling_machine_1 :: ProcessComposition.Isabelle.Factorio.Machine.Machine;
assembling_machine_1 =
  ProcessComposition.Isabelle.Factorio.Machine.Machine "Assembling Machine 1"
    (ProcessComposition.Isabelle.Rat.divide_rat
      (ProcessComposition.Isabelle.Rat.of_int
        (ProcessComposition.Isabelle.Arith.Int_of_integer (5 :: Integer)))
      (ProcessComposition.Isabelle.Rat.of_int
        (ProcessComposition.Isabelle.Arith.Int_of_integer (10 :: Integer))))
    (ProcessComposition.Isabelle.Arith.nat_of_integer (2500 :: Integer))
    (ProcessComposition.Isabelle.Arith.nat_of_integer (75000 :: Integer));

iron_plate :: ProcessComposition.Isabelle.Factorio.Item.Item;
iron_plate = ProcessComposition.Isabelle.Factorio.Item.Item "Iron Plate";

iron_gear :: ProcessComposition.Isabelle.Factorio.Item.Item;
iron_gear = ProcessComposition.Isabelle.Factorio.Item.Item "Iron Gear";

craftGear :: ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe;
craftGear =
  ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe
    [(iron_plate,
       ProcessComposition.Isabelle.Arith.nat_of_integer (2 :: Integer))]
    [(iron_gear, ProcessComposition.Isabelle.Arith.one_nat)]
    (ProcessComposition.Isabelle.Rat.divide_rat
      (ProcessComposition.Isabelle.Rat.of_int
        (ProcessComposition.Isabelle.Arith.Int_of_integer (5 :: Integer)))
      (ProcessComposition.Isabelle.Rat.of_int
        (ProcessComposition.Isabelle.Arith.Int_of_integer (10 :: Integer))))
    assembling_machine_1 "Craft Iron Plates into Iron Gear";

steel_plate :: ProcessComposition.Isabelle.Factorio.Item.Item;
steel_plate = ProcessComposition.Isabelle.Factorio.Item.Item "Steel Plate";

cargo_wagon :: ProcessComposition.Isabelle.Factorio.Item.Item;
cargo_wagon = ProcessComposition.Isabelle.Factorio.Item.Item "Cargo Wagon";

craftWagon :: ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe;
craftWagon =
  ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe
    [(iron_gear,
       ProcessComposition.Isabelle.Arith.nat_of_integer (10 :: Integer)),
      (iron_plate,
        ProcessComposition.Isabelle.Arith.nat_of_integer (20 :: Integer)),
      (steel_plate,
        ProcessComposition.Isabelle.Arith.nat_of_integer (20 :: Integer))]
    [(cargo_wagon, ProcessComposition.Isabelle.Arith.one_nat)]
    ProcessComposition.Isabelle.Rat.one_rat assembling_machine_1
    "Craft Iron Plates, Steel Plates and Iron Gears into Cargo Wagon";

electric_furnace :: ProcessComposition.Isabelle.Factorio.Machine.Machine;
electric_furnace =
  ProcessComposition.Isabelle.Factorio.Machine.Machine "Electric Furnace"
    (ProcessComposition.Isabelle.Rat.of_int
      (ProcessComposition.Isabelle.Arith.Int_of_integer (2 :: Integer)))
    (ProcessComposition.Isabelle.Arith.nat_of_integer (6000 :: Integer))
    (ProcessComposition.Isabelle.Arith.nat_of_integer (180000 :: Integer));

smeltIronOre :: ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe;
smeltIronOre =
  ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe
    [(iron_ore, ProcessComposition.Isabelle.Arith.one_nat)]
    [(iron_plate, ProcessComposition.Isabelle.Arith.one_nat)]
    (ProcessComposition.Isabelle.Rat.divide_rat
      (ProcessComposition.Isabelle.Rat.of_int
        (ProcessComposition.Isabelle.Arith.Int_of_integer (32 :: Integer)))
      (ProcessComposition.Isabelle.Rat.of_int
        (ProcessComposition.Isabelle.Arith.Int_of_integer (10 :: Integer))))
    electric_furnace "Smelt Iron Ore to Iron Plate";

smeltIronPlate :: ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe;
smeltIronPlate =
  ProcessComposition.Isabelle.Factorio.Manufacturing.Recipe
    [(iron_plate,
       ProcessComposition.Isabelle.Arith.nat_of_integer (5 :: Integer))]
    [(steel_plate, ProcessComposition.Isabelle.Arith.one_nat)]
    (ProcessComposition.Isabelle.Rat.of_int
      (ProcessComposition.Isabelle.Arith.Int_of_integer (16 :: Integer)))
    electric_furnace "Smelt Iron Plates to Steel Plate";

}
