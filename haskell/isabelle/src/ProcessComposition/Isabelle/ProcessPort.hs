{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ProcessPort(Process_side(..), parallelPorts,
   equal_process_side)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Port;
import qualified ProcessComposition.Isabelle.Arith;

data Process_side = In | Out deriving (Prelude.Read, Prelude.Show);

parallelPorts ::
  forall a b c.
    ProcessComposition.Isabelle.Arith.Nat ->
      a -> ProcessComposition.Isabelle.Resource.Resource b c ->
             [ProcessComposition.Isabelle.Port.Port a
                (ProcessComposition.Isabelle.Resource.Resource b c)];
parallelPorts n s a =
  ProcessComposition.Isabelle.Port.listPorts n s
    (ProcessComposition.Isabelle.Resource.parallel_parts a);

equal_process_side :: Process_side -> Process_side -> Bool;
equal_process_side In Out = False;
equal_process_side Out In = False;
equal_process_side Out Out = True;
equal_process_side In In = True;

}
