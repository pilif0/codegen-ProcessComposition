{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.NodePortGraph(nodePortGraph) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.Product_Type;
import qualified ProcessComposition.Isabelle.PortGraph;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Port;

nodePortGraph ::
  forall a b c d.
    (ProcessComposition.Isabelle.Port.Side_in_out d) => [a] ->
                  b -> [c] ->
                         [c] ->
                           ProcessComposition.Isabelle.PortGraph.Port_graph d c
                             a b;
nodePortGraph n l ins outs =
  ProcessComposition.Isabelle.PortGraph.PGraph
    [ProcessComposition.Isabelle.PortGraph.Node n l
       (ProcessComposition.Isabelle.Port.listPorts
          ProcessComposition.Isabelle.Arith.zero_nat
          ProcessComposition.Isabelle.Port.ina ins ++
         ProcessComposition.Isabelle.Port.listPorts
           ProcessComposition.Isabelle.Arith.zero_nat
           ProcessComposition.Isabelle.Port.out outs)]
    (map (\ (a, b) -> ProcessComposition.Isabelle.PortGraph.Edge a b)
       (zip (map ProcessComposition.Isabelle.PortGraph.OpenPort
              (ProcessComposition.Isabelle.Port.listPorts
                ProcessComposition.Isabelle.Arith.zero_nat
                ProcessComposition.Isabelle.Port.ina ins))
         (map (\ p ->
                ProcessComposition.Isabelle.PortGraph.GroundPort
                  (ProcessComposition.Isabelle.Port.QPort p n))
           (ProcessComposition.Isabelle.Port.listPorts
             ProcessComposition.Isabelle.Arith.zero_nat
             ProcessComposition.Isabelle.Port.ina ins))) ++
      map (\ (a, b) -> ProcessComposition.Isabelle.PortGraph.Edge a b)
        (zip (map (\ p ->
                    ProcessComposition.Isabelle.PortGraph.GroundPort
                      (ProcessComposition.Isabelle.Port.QPort p n))
               (ProcessComposition.Isabelle.Port.listPorts
                 ProcessComposition.Isabelle.Arith.zero_nat
                 ProcessComposition.Isabelle.Port.out outs))
          (map ProcessComposition.Isabelle.PortGraph.OpenPort
            (ProcessComposition.Isabelle.Port.listPorts
              ProcessComposition.Isabelle.Arith.zero_nat
              ProcessComposition.Isabelle.Port.out outs))))
    (ProcessComposition.Isabelle.Port.listPorts
       ProcessComposition.Isabelle.Arith.zero_nat
       ProcessComposition.Isabelle.Port.ina ins ++
      ProcessComposition.Isabelle.Port.listPorts
        ProcessComposition.Isabelle.Arith.zero_nat
        ProcessComposition.Isabelle.Port.out outs);

}
