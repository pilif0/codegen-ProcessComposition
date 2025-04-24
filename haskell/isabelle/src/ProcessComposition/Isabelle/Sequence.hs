{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Sequence(allEdges, edgesByOpenTo,
edgesFromPortMapping, edgesByOpenFrom, seqInterfaceEdges, seqPortGraphs)
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
import qualified ProcessComposition.Isabelle.Option;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Mapping;
import qualified ProcessComposition.Isabelle.Port;
import qualified ProcessComposition.Isabelle.PortGraph;

allEdges ::
  forall a b c.
    [ProcessComposition.Isabelle.PortGraph.Place a b c] ->
      [ProcessComposition.Isabelle.PortGraph.Place a b c] ->
        [ProcessComposition.Isabelle.PortGraph.Edge a b c];
allEdges [] ts = [];
allEdges (f : fs) ts =
  map (ProcessComposition.Isabelle.PortGraph.Edge f) ts ++ allEdges fs ts;

edgesByOpenTo ::
  forall a b c.
    (Eq a,
      Eq b) => [ProcessComposition.Isabelle.PortGraph.Edge a b c] ->
                 ProcessComposition.Isabelle.Mapping.Mapping
                   (ProcessComposition.Isabelle.Port.Port a b)
                   [ProcessComposition.Isabelle.PortGraph.Edge a b c];
edgesByOpenTo [] = ProcessComposition.Isabelle.Mapping.empty;
edgesByOpenTo (e : es) =
  (if ProcessComposition.Isabelle.PortGraph.place_open
        (ProcessComposition.Isabelle.PortGraph.edge_to e)
    then ProcessComposition.Isabelle.Mapping.map_default
           (ProcessComposition.Isabelle.PortGraph.place_port
             (ProcessComposition.Isabelle.PortGraph.edge_to e))
           [] (\ a -> e : a) (edgesByOpenTo es)
    else edgesByOpenTo es);

edgesFromPortMapping ::
  forall a b c.
    (Eq a, ProcessComposition.Isabelle.Port.Side_in_out a,
      Eq b) => [ProcessComposition.Isabelle.Port.Port a b] ->
                 ProcessComposition.Isabelle.Mapping.Mapping
                   (ProcessComposition.Isabelle.Port.Port a b)
                   [ProcessComposition.Isabelle.PortGraph.Place a b c] ->
                   ProcessComposition.Isabelle.Mapping.Mapping
                     (ProcessComposition.Isabelle.Port.Port a b)
                     [ProcessComposition.Isabelle.PortGraph.Place a b c] ->
                     [ProcessComposition.Isabelle.PortGraph.Edge a b c];
edgesFromPortMapping [] x y = [];
edgesFromPortMapping (p : ps) x y =
  (case ProcessComposition.Isabelle.Mapping.lookup x
          (ProcessComposition.Isabelle.Port.portSetSide
            ProcessComposition.Isabelle.Port.out p)
    of {
    Nothing -> [];
    Just xs ->
      (case ProcessComposition.Isabelle.Mapping.lookup y
              (ProcessComposition.Isabelle.Port.portSetSide
                ProcessComposition.Isabelle.Port.ina p)
        of {
        Nothing -> [];
        Just a -> allEdges xs a;
      });
  }) ++
    edgesFromPortMapping ps x y;

edgesByOpenFrom ::
  forall a b c.
    (Eq a,
      Eq b) => [ProcessComposition.Isabelle.PortGraph.Edge a b c] ->
                 ProcessComposition.Isabelle.Mapping.Mapping
                   (ProcessComposition.Isabelle.Port.Port a b)
                   [ProcessComposition.Isabelle.PortGraph.Edge a b c];
edgesByOpenFrom [] = ProcessComposition.Isabelle.Mapping.empty;
edgesByOpenFrom (e : es) =
  (if ProcessComposition.Isabelle.PortGraph.place_open
        (ProcessComposition.Isabelle.PortGraph.edge_from e)
    then ProcessComposition.Isabelle.Mapping.map_default
           (ProcessComposition.Isabelle.PortGraph.place_port
             (ProcessComposition.Isabelle.PortGraph.edge_from e))
           [] (\ a -> e : a) (edgesByOpenFrom es)
    else edgesByOpenFrom es);

seqInterfaceEdges ::
  forall a b c d.
    (Eq a, ProcessComposition.Isabelle.Port.Side_in_out a, Eq b,
      Eq c) => ProcessComposition.Isabelle.PortGraph.Port_graph a b c d ->
                 ProcessComposition.Isabelle.PortGraph.Port_graph a b c d ->
                   [ProcessComposition.Isabelle.PortGraph.Edge a b c];
seqInterfaceEdges x y =
  ProcessComposition.Isabelle.List.remdups
    (edgesFromPortMapping
      (filter
        (\ xa ->
          ProcessComposition.Isabelle.Port.side xa ==
            ProcessComposition.Isabelle.Port.out)
        (ProcessComposition.Isabelle.PortGraph.pg_ports x))
      (ProcessComposition.Isabelle.Mapping.map_values
        (\ _ -> map ProcessComposition.Isabelle.PortGraph.edge_from)
        (edgesByOpenTo (ProcessComposition.Isabelle.PortGraph.pg_edges x)))
      (ProcessComposition.Isabelle.Mapping.map_values
        (\ _ -> map ProcessComposition.Isabelle.PortGraph.edge_to)
        (edgesByOpenFrom (ProcessComposition.Isabelle.PortGraph.pg_edges y))));

seqPortGraphs ::
  forall a b c d.
    (Eq a, ProcessComposition.Isabelle.Port.Side_in_out a, Eq b,
      Eq c) => ProcessComposition.Isabelle.PortGraph.Port_graph a b c d ->
                 ProcessComposition.Isabelle.PortGraph.Port_graph a b c d ->
                   ProcessComposition.Isabelle.PortGraph.Port_graph a b c d;
seqPortGraphs x y =
  ProcessComposition.Isabelle.PortGraph.PGraph
    (ProcessComposition.Isabelle.PortGraph.pg_nodes x ++
      ProcessComposition.Isabelle.PortGraph.pg_nodes y)
    (seqInterfaceEdges x y ++
      ProcessComposition.Isabelle.PortGraph.disconnectFromPlaces
        (ProcessComposition.Isabelle.List.map_filter
          (\ xa ->
            (if ProcessComposition.Isabelle.Port.side xa ==
                  ProcessComposition.Isabelle.Port.out
              then Just (ProcessComposition.Isabelle.PortGraph.OpenPort xa)
              else Nothing))
          (ProcessComposition.Isabelle.PortGraph.pg_ports x))
        (ProcessComposition.Isabelle.PortGraph.pg_edges x) ++
        map (ProcessComposition.Isabelle.PortGraph.shiftOpenInEdge
              (\ s ->
                (if s == ProcessComposition.Isabelle.Port.ina ||
                      s == ProcessComposition.Isabelle.Port.out
                  then ProcessComposition.Isabelle.Arith.zero_nat
                  else ProcessComposition.Isabelle.List.size_list
                         (filter
                           (\ xa ->
                             ProcessComposition.Isabelle.Port.side xa == s)
                           (ProcessComposition.Isabelle.PortGraph.pg_ports x))))
              (\ s ->
                (if s == ProcessComposition.Isabelle.Port.ina ||
                      s == ProcessComposition.Isabelle.Port.out
                  then ProcessComposition.Isabelle.Arith.zero_nat
                  else ProcessComposition.Isabelle.List.size_list
                         (filter
                           (\ xa ->
                             ProcessComposition.Isabelle.Port.side xa == s)
                           (ProcessComposition.Isabelle.PortGraph.pg_ports
                             x)))))
          (ProcessComposition.Isabelle.PortGraph.disconnectFromPlaces
            (ProcessComposition.Isabelle.List.map_filter
              (\ xa ->
                (if ProcessComposition.Isabelle.Port.side xa ==
                      ProcessComposition.Isabelle.Port.ina
                  then Just (ProcessComposition.Isabelle.PortGraph.OpenPort xa)
                  else Nothing))
              (ProcessComposition.Isabelle.PortGraph.pg_ports y))
            (ProcessComposition.Isabelle.PortGraph.pg_edges y)))
    (filter
       (\ xa ->
         not (ProcessComposition.Isabelle.Port.side xa ==
               ProcessComposition.Isabelle.Port.out))
       (ProcessComposition.Isabelle.PortGraph.pg_ports x) ++
      filter
        (\ xa ->
          ProcessComposition.Isabelle.Port.side xa ==
            ProcessComposition.Isabelle.Port.out)
        (ProcessComposition.Isabelle.PortGraph.pg_ports y) ++
        ProcessComposition.Isabelle.List.map_filter
          (\ xa ->
            (if not (ProcessComposition.Isabelle.Port.side xa ==
                      ProcessComposition.Isabelle.Port.ina) &&
                  not (ProcessComposition.Isabelle.Port.side xa ==
                        ProcessComposition.Isabelle.Port.out)
              then Just (ProcessComposition.Isabelle.PortGraph.shiftPort
                          (\ s ->
                            ProcessComposition.Isabelle.List.size_list
                              (filter
                                (\ p ->
                                  ProcessComposition.Isabelle.Port.side p == s)
                                (ProcessComposition.Isabelle.PortGraph.pg_ports
                                  x)))
                          xa)
              else Nothing))
          (ProcessComposition.Isabelle.PortGraph.pg_ports y));

}
