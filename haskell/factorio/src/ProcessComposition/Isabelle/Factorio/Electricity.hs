{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Factorio.Electricity(elec_res, sum_parallel_t,
            sum_parallel, electricity)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.Groups_List;
import qualified ProcessComposition.Isabelle.ResTerm;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Process;
import qualified ProcessComposition.Isabelle.Factorio.Machine;
import qualified ProcessComposition.Isabelle.Sum_Type;
import qualified ProcessComposition.Isabelle.Factorio.Item;
import qualified ProcessComposition.Isabelle.Arith;

elec_res ::
  ProcessComposition.Isabelle.Sum_Type.Sum
    ProcessComposition.Isabelle.Factorio.Item.Flow
    ProcessComposition.Isabelle.Factorio.Machine.Mach_block ->
    ProcessComposition.Isabelle.Arith.Nat;
elec_res (ProcessComposition.Isabelle.Sum_Type.Inl uu) =
  ProcessComposition.Isabelle.Arith.zero_nat;
elec_res
  (ProcessComposition.Isabelle.Sum_Type.Inr
    (ProcessComposition.Isabelle.Factorio.Machine.MachBlock m n uv uw))
  = ProcessComposition.Isabelle.Arith.times_nat n
      (ProcessComposition.Isabelle.Arith.plus_nat
        (ProcessComposition.Isabelle.Factorio.Machine.machineDrain m)
        (ProcessComposition.Isabelle.Factorio.Machine.machineConsu m));

sum_parallel_t ::
  forall a b.
    (ProcessComposition.Isabelle.Arith.Monoid_add a) => ProcessComposition.Isabelle.ResTerm.Res_term
                  a b ->
                  a;
sum_parallel_t ProcessComposition.Isabelle.ResTerm.Empty =
  ProcessComposition.Isabelle.Arith.zero;
sum_parallel_t ProcessComposition.Isabelle.ResTerm.Anything = error "undefined";
sum_parallel_t (ProcessComposition.Isabelle.ResTerm.Res u) = u;
sum_parallel_t (ProcessComposition.Isabelle.ResTerm.Copyable a) =
  error "undefined";
sum_parallel_t (ProcessComposition.Isabelle.ResTerm.Parallel xs) =
  ProcessComposition.Isabelle.Groups_List.sum_list (map sum_parallel_t xs);
sum_parallel_t (ProcessComposition.Isabelle.ResTerm.NonD a b) =
  error "undefined";
sum_parallel_t (ProcessComposition.Isabelle.ResTerm.Executable a b) =
  error "undefined";
sum_parallel_t (ProcessComposition.Isabelle.ResTerm.Repeatable a b) =
  error "undefined";

sum_parallel ::
  forall a b.
    (ProcessComposition.Isabelle.Arith.Monoid_add a) => ProcessComposition.Isabelle.Resource.Resource
                  a b ->
                  a;
sum_parallel (ProcessComposition.Isabelle.Resource.Abs_resource x) =
  sum_parallel_t x;

electricity ::
  forall a b c.
    ProcessComposition.Isabelle.Process.Process
      (ProcessComposition.Isabelle.Sum_Type.Sum
        ProcessComposition.Isabelle.Factorio.Item.Flow
        ProcessComposition.Isabelle.Factorio.Machine.Mach_block)
      a b c ->
      ProcessComposition.Isabelle.Arith.Nat;
electricity p =
  ProcessComposition.Isabelle.Arith.minus_nat
    (sum_parallel
      (ProcessComposition.Isabelle.Resource.map_resource elec_res id
        (ProcessComposition.Isabelle.Process.input p)))
    (sum_parallel
      (ProcessComposition.Isabelle.Resource.map_resource elec_res id
        (ProcessComposition.Isabelle.Process.output p)));

}
