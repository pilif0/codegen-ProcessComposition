{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.Juxtapose(juxtapose) where {

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
import qualified ProcessComposition.Isabelle.PortGraph;
import qualified ProcessComposition.Isabelle.Port;
import qualified ProcessComposition.Isabelle.Arith;

juxtapose ::
  forall a b c d.
    (Eq a) => ProcessComposition.Isabelle.PortGraph.Port_graph a b c d ->
                ProcessComposition.Isabelle.PortGraph.Port_graph a b c d ->
                  ProcessComposition.Isabelle.PortGraph.Port_graph a b c d;
juxtapose p q =
  ProcessComposition.Isabelle.PortGraph.PGraph
    (ProcessComposition.Isabelle.PortGraph.pg_nodes p ++
      ProcessComposition.Isabelle.PortGraph.pg_nodes q)
    (ProcessComposition.Isabelle.PortGraph.pg_edges p ++
      map (ProcessComposition.Isabelle.PortGraph.shiftOpenInEdge
            (\ s ->
              ProcessComposition.Isabelle.List.size_list
                (filter (\ x -> ProcessComposition.Isabelle.Port.side x == s)
                  (ProcessComposition.Isabelle.PortGraph.pg_ports p)))
            (\ s ->
              ProcessComposition.Isabelle.List.size_list
                (filter (\ x -> ProcessComposition.Isabelle.Port.side x == s)
                  (ProcessComposition.Isabelle.PortGraph.pg_ports p))))
        (ProcessComposition.Isabelle.PortGraph.pg_edges q))
    (ProcessComposition.Isabelle.PortGraph.pg_ports p ++
      map (ProcessComposition.Isabelle.PortGraph.shiftPort
            (\ s ->
              ProcessComposition.Isabelle.List.size_list
                (filter (\ pa -> ProcessComposition.Isabelle.Port.side pa == s)
                  (ProcessComposition.Isabelle.PortGraph.pg_ports p))))
        (ProcessComposition.Isabelle.PortGraph.pg_ports q));

}
