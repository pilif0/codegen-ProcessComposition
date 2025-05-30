{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.ForkPortGraph(forkPortGraph) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.PortGraph;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Port;

forkPortGraph ::
  forall a b c d.
    (ProcessComposition.Isabelle.Port.Side_in_out b) => a ->
                  ProcessComposition.Isabelle.PortGraph.Port_graph b a c d;
forkPortGraph r =
  ProcessComposition.Isabelle.PortGraph.PGraph []
    [ProcessComposition.Isabelle.PortGraph.Edge
       (ProcessComposition.Isabelle.PortGraph.OpenPort
         (ProcessComposition.Isabelle.Port.Port
           ProcessComposition.Isabelle.Port.ina
           ProcessComposition.Isabelle.Arith.zero_nat r))
       (ProcessComposition.Isabelle.PortGraph.OpenPort
         (ProcessComposition.Isabelle.Port.Port
           ProcessComposition.Isabelle.Port.out
           ProcessComposition.Isabelle.Arith.zero_nat r)),
      ProcessComposition.Isabelle.PortGraph.Edge
        (ProcessComposition.Isabelle.PortGraph.OpenPort
          (ProcessComposition.Isabelle.Port.Port
            ProcessComposition.Isabelle.Port.ina
            ProcessComposition.Isabelle.Arith.zero_nat r))
        (ProcessComposition.Isabelle.PortGraph.OpenPort
          (ProcessComposition.Isabelle.Port.Port
            ProcessComposition.Isabelle.Port.out
            ProcessComposition.Isabelle.Arith.one_nat r))]
    [ProcessComposition.Isabelle.Port.Port ProcessComposition.Isabelle.Port.ina
       ProcessComposition.Isabelle.Arith.zero_nat r,
      ProcessComposition.Isabelle.Port.Port ProcessComposition.Isabelle.Port.out
        ProcessComposition.Isabelle.Arith.zero_nat r,
      ProcessComposition.Isabelle.Port.Port ProcessComposition.Isabelle.Port.out
        ProcessComposition.Isabelle.Arith.one_nat r];

}
