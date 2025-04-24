{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.SwapPortGraph(swapPortGraph) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Product_Type;
import qualified ProcessComposition.Isabelle.PortGraph;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Port;

swapPortGraph ::
  forall a b c d.
    (ProcessComposition.Isabelle.Port.Side_in_out b) => [a] ->
                  [a] ->
                    ProcessComposition.Isabelle.PortGraph.Port_graph b a c d;
swapPortGraph a b =
  ProcessComposition.Isabelle.PortGraph.PGraph []
    (map (\ (aa, ba) -> ProcessComposition.Isabelle.PortGraph.Edge aa ba)
       (zip (map ProcessComposition.Isabelle.PortGraph.OpenPort
              (ProcessComposition.Isabelle.Port.listPorts
                ProcessComposition.Isabelle.Arith.zero_nat
                ProcessComposition.Isabelle.Port.ina a))
         (map ProcessComposition.Isabelle.PortGraph.OpenPort
           (ProcessComposition.Isabelle.Port.listPorts
             (ProcessComposition.Isabelle.List.size_list b)
             ProcessComposition.Isabelle.Port.out a))) ++
      map (\ (aa, ba) -> ProcessComposition.Isabelle.PortGraph.Edge aa ba)
        (zip (map ProcessComposition.Isabelle.PortGraph.OpenPort
               (ProcessComposition.Isabelle.Port.listPorts
                 (ProcessComposition.Isabelle.List.size_list a)
                 ProcessComposition.Isabelle.Port.ina b))
          (map ProcessComposition.Isabelle.PortGraph.OpenPort
            (ProcessComposition.Isabelle.Port.listPorts
              ProcessComposition.Isabelle.Arith.zero_nat
              ProcessComposition.Isabelle.Port.out b))))
    (ProcessComposition.Isabelle.Port.listPorts
       ProcessComposition.Isabelle.Arith.zero_nat
       ProcessComposition.Isabelle.Port.ina (a ++ b) ++
      ProcessComposition.Isabelle.Port.listPorts
        ProcessComposition.Isabelle.Arith.zero_nat
        ProcessComposition.Isabelle.Port.out (b ++ a));

}
