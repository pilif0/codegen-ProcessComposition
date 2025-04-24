{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.IdentityPortGraph(idPortGraph) where {

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

idPortGraph ::
  forall a b c d.
    (ProcessComposition.Isabelle.Port.Side_in_out b) => [a] ->
                  ProcessComposition.Isabelle.PortGraph.Port_graph b a c d;
idPortGraph asa =
  ProcessComposition.Isabelle.PortGraph.PGraph []
    (map (\ (a, b) -> ProcessComposition.Isabelle.PortGraph.Edge a b)
      (zip (map ProcessComposition.Isabelle.PortGraph.OpenPort
             (ProcessComposition.Isabelle.Port.listPorts
               ProcessComposition.Isabelle.Arith.zero_nat
               ProcessComposition.Isabelle.Port.ina asa))
        (map ProcessComposition.Isabelle.PortGraph.OpenPort
          (ProcessComposition.Isabelle.Port.listPorts
            ProcessComposition.Isabelle.Arith.zero_nat
            ProcessComposition.Isabelle.Port.out asa))))
    (ProcessComposition.Isabelle.Port.listPorts
       ProcessComposition.Isabelle.Arith.zero_nat
       ProcessComposition.Isabelle.Port.ina asa ++
      ProcessComposition.Isabelle.Port.listPorts
        ProcessComposition.Isabelle.Arith.zero_nat
        ProcessComposition.Isabelle.Port.out asa);

}
