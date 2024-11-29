{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Process(Process(..), input, output, valid,
                                       primitives, process_subst,
                                       par_process_list, seq_process_list,
                                       process_refineRes, map_process)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified ProcessComposition.Isabelle.Resource;

data Process a b c d =
  Primitive (ProcessComposition.Isabelle.Resource.Resource a b)
    (ProcessComposition.Isabelle.Resource.Resource a b) c d
  | Seq (Process a b c d) (Process a b c d)
  | Par (Process a b c d) (Process a b c d)
  | Opt (Process a b c d) (Process a b c d) | Represent (Process a b c d)
  | Identity (ProcessComposition.Isabelle.Resource.Resource a b)
  | Swap (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | InjectL (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | InjectR (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | OptDistrIn (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | OptDistrOut (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Duplicate b | Erase b
  | Apply (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Repeat (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Close (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Once (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Forget (ProcessComposition.Isabelle.Resource.Resource a b)
  deriving (Prelude.Read, Prelude.Show);

input ::
  forall a b c d.
    Process a b c d -> ProcessComposition.Isabelle.Resource.Resource a b;
input (Primitive ins outs l m) = ins;
input (Seq p q) = input p;
input (Par p q) =
  ProcessComposition.Isabelle.Resource.resource_par (input p) (input q);
input (Opt p q) = ProcessComposition.Isabelle.Resource.nonD (input p) (input q);
input (Represent p) = ProcessComposition.Isabelle.Resource.empty;
input (Identity a) = a;
input (Swap a b) = ProcessComposition.Isabelle.Resource.resource_par a b;
input (InjectL a b) = a;
input (InjectR a b) = b;
input (OptDistrIn a b c) =
  ProcessComposition.Isabelle.Resource.resource_par a
    (ProcessComposition.Isabelle.Resource.nonD b c);
input (OptDistrOut a b c) =
  ProcessComposition.Isabelle.Resource.nonD
    (ProcessComposition.Isabelle.Resource.resource_par a b)
    (ProcessComposition.Isabelle.Resource.resource_par a c);
input (Duplicate a) = ProcessComposition.Isabelle.Resource.copyable a;
input (Erase a) = ProcessComposition.Isabelle.Resource.copyable a;
input (Apply a b) =
  ProcessComposition.Isabelle.Resource.resource_par a
    (ProcessComposition.Isabelle.Resource.executable a b);
input (Repeat a b) = ProcessComposition.Isabelle.Resource.repeatable a b;
input (Close a b) = ProcessComposition.Isabelle.Resource.repeatable a b;
input (Once a b) = ProcessComposition.Isabelle.Resource.repeatable a b;
input (Forget a) = a;

output ::
  forall a b c d.
    Process a b c d -> ProcessComposition.Isabelle.Resource.Resource a b;
output (Primitive ins outs l m) = outs;
output (Seq p q) = output q;
output (Par p q) =
  ProcessComposition.Isabelle.Resource.resource_par (output p) (output q);
output (Opt p q) = output p;
output (Represent p) =
  ProcessComposition.Isabelle.Resource.repeatable (input p) (output p);
output (Identity a) = a;
output (Swap a b) = ProcessComposition.Isabelle.Resource.resource_par b a;
output (InjectL a b) = ProcessComposition.Isabelle.Resource.nonD a b;
output (InjectR a b) = ProcessComposition.Isabelle.Resource.nonD a b;
output (OptDistrIn a b c) =
  ProcessComposition.Isabelle.Resource.nonD
    (ProcessComposition.Isabelle.Resource.resource_par a b)
    (ProcessComposition.Isabelle.Resource.resource_par a c);
output (OptDistrOut a b c) =
  ProcessComposition.Isabelle.Resource.resource_par a
    (ProcessComposition.Isabelle.Resource.nonD b c);
output (Duplicate a) =
  ProcessComposition.Isabelle.Resource.resource_par
    (ProcessComposition.Isabelle.Resource.copyable a)
    (ProcessComposition.Isabelle.Resource.copyable a);
output (Erase a) = ProcessComposition.Isabelle.Resource.empty;
output (Apply a b) = b;
output (Repeat a b) =
  ProcessComposition.Isabelle.Resource.resource_par
    (ProcessComposition.Isabelle.Resource.repeatable a b)
    (ProcessComposition.Isabelle.Resource.repeatable a b);
output (Close a b) = ProcessComposition.Isabelle.Resource.empty;
output (Once a b) = ProcessComposition.Isabelle.Resource.executable a b;
output (Forget a) = ProcessComposition.Isabelle.Resource.anything;

valid :: forall a b c d. (Eq a, Eq b) => Process a b c d -> Bool;
valid (Primitive ins outs l m) = True;
valid (Seq p q) = output p == input q && valid p && valid q;
valid (Par p q) = valid p && valid q;
valid (Opt p q) = valid p && valid q && output p == output q;
valid (Represent p) = valid p;
valid (Identity a) = True;
valid (Swap a b) = True;
valid (InjectL a b) = True;
valid (InjectR a b) = True;
valid (OptDistrIn a b c) = True;
valid (OptDistrOut a b c) = True;
valid (Duplicate a) = True;
valid (Erase a) = True;
valid (Apply a b) = True;
valid (Repeat a b) = True;
valid (Close a b) = True;
valid (Once a b) = True;
valid (Forget a) = True;

primitives ::
  forall a b c d.
    Process a b c d ->
      [(ProcessComposition.Isabelle.Resource.Resource a b,
         (ProcessComposition.Isabelle.Resource.Resource a b, (c, d)))];
primitives (Primitive ins outs l m) = [(ins, (outs, (l, m)))];
primitives (Seq p q) = primitives p ++ primitives q;
primitives (Par p q) = primitives p ++ primitives q;
primitives (Opt p q) = primitives p ++ primitives q;
primitives (Represent p) = primitives p;
primitives (Identity a) = [];
primitives (Swap a b) = [];
primitives (InjectL a b) = [];
primitives (InjectR a b) = [];
primitives (OptDistrIn a b c) = [];
primitives (OptDistrOut a b c) = [];
primitives (Duplicate a) = [];
primitives (Erase a) = [];
primitives (Apply a b) = [];
primitives (Repeat a b) = [];
primitives (Close a b) = [];
primitives (Once a b) = [];
primitives (Forget a) = [];

process_subst ::
  forall a b c d.
    (ProcessComposition.Isabelle.Resource.Resource a b ->
      ProcessComposition.Isabelle.Resource.Resource a b -> c -> d -> Bool) ->
      (ProcessComposition.Isabelle.Resource.Resource a b ->
        ProcessComposition.Isabelle.Resource.Resource a b ->
          c -> d -> Process a b c d) ->
        Process a b c d -> Process a b c d;
process_subst p f (Primitive a b l m) =
  (if p a b l m then f a b l m else Primitive a b l m);
process_subst p f (Identity a) = Identity a;
process_subst p f (Swap a b) = Swap a b;
process_subst pa f (Seq p q) =
  Seq (process_subst pa f p) (process_subst pa f q);
process_subst pa f (Par p q) =
  Par (process_subst pa f p) (process_subst pa f q);
process_subst pa f (Opt p q) =
  Opt (process_subst pa f p) (process_subst pa f q);
process_subst p f (InjectL a b) = InjectL a b;
process_subst p f (InjectR a b) = InjectR a b;
process_subst p f (OptDistrIn a b c) = OptDistrIn a b c;
process_subst p f (OptDistrOut a b c) = OptDistrOut a b c;
process_subst p f (Duplicate a) = Duplicate a;
process_subst p f (Erase a) = Erase a;
process_subst pa f (Represent p) = Represent (process_subst pa f p);
process_subst p f (Apply a b) = Apply a b;
process_subst p f (Repeat a b) = Repeat a b;
process_subst p f (Close a b) = Close a b;
process_subst p f (Once a b) = Once a b;
process_subst p f (Forget a) = Forget a;

par_process_list :: forall a b c d. [Process a b c d] -> Process a b c d;
par_process_list [] = Identity ProcessComposition.Isabelle.Resource.empty;
par_process_list (x : xs) =
  (if null xs then x else Par x (par_process_list xs));

seq_process_list :: forall a b c d. [Process a b c d] -> Process a b c d;
seq_process_list [] = Identity ProcessComposition.Isabelle.Resource.empty;
seq_process_list (x : xs) =
  (if null xs then x else Seq x (seq_process_list xs));

process_refineRes ::
  forall a b c d e f.
    (a -> ProcessComposition.Isabelle.Resource.Resource b c) ->
      (d -> c) -> Process a d e f -> Process b c e f;
process_refineRes f g (Primitive ins outs l m) =
  Primitive (ProcessComposition.Isabelle.Resource.refine_resource f g ins)
    (ProcessComposition.Isabelle.Resource.refine_resource f g outs) l m;
process_refineRes f g (Identity a) =
  Identity (ProcessComposition.Isabelle.Resource.refine_resource f g a);
process_refineRes f g (Swap a b) =
  Swap (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Seq p q) =
  Seq (process_refineRes f g p) (process_refineRes f g q);
process_refineRes f g (Par p q) =
  Par (process_refineRes f g p) (process_refineRes f g q);
process_refineRes f g (Opt p q) =
  Opt (process_refineRes f g p) (process_refineRes f g q);
process_refineRes f g (InjectL a b) =
  InjectL (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (InjectR a b) =
  InjectR (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (OptDistrIn a b c) =
  OptDistrIn (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b)
    (ProcessComposition.Isabelle.Resource.refine_resource f g c);
process_refineRes f g (OptDistrOut a b c) =
  OptDistrOut (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b)
    (ProcessComposition.Isabelle.Resource.refine_resource f g c);
process_refineRes f g (Duplicate a) = Duplicate (g a);
process_refineRes f g (Erase a) = Erase (g a);
process_refineRes f g (Represent p) = Represent (process_refineRes f g p);
process_refineRes f g (Apply a b) =
  Apply (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Repeat a b) =
  Repeat (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Close a b) =
  Close (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Once a b) =
  Once (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Forget a) =
  Forget (ProcessComposition.Isabelle.Resource.refine_resource f g a);

map_process ::
  forall a b c d e f g h.
    (a -> b) ->
      (c -> d) -> (e -> f) -> (g -> h) -> Process a c e g -> Process b d f h;
map_process f1 f2 f3 f4 (Primitive x11 x12 x13 x14) =
  Primitive (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x11)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x12) (f3 x13)
    (f4 x14);
map_process f1 f2 f3 f4 (Seq x21 x22) =
  Seq (map_process f1 f2 f3 f4 x21) (map_process f1 f2 f3 f4 x22);
map_process f1 f2 f3 f4 (Par x31 x32) =
  Par (map_process f1 f2 f3 f4 x31) (map_process f1 f2 f3 f4 x32);
map_process f1 f2 f3 f4 (Opt x41 x42) =
  Opt (map_process f1 f2 f3 f4 x41) (map_process f1 f2 f3 f4 x42);
map_process f1 f2 f3 f4 (Represent x5) = Represent (map_process f1 f2 f3 f4 x5);
map_process f1 f2 f3 f4 (Identity x6) =
  Identity (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x6);
map_process f1 f2 f3 f4 (Swap x71 x72) =
  Swap (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x71)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x72);
map_process f1 f2 f3 f4 (InjectL x81 x82) =
  InjectL (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x81)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x82);
map_process f1 f2 f3 f4 (InjectR x91 x92) =
  InjectR (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x91)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x92);
map_process f1 f2 f3 f4 (OptDistrIn x101 x102 x103) =
  OptDistrIn (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x101)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x102)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x103);
map_process f1 f2 f3 f4 (OptDistrOut x111 x112 x113) =
  OptDistrOut (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x111)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x112)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x113);
map_process f1 f2 f3 f4 (Duplicate x12a) = Duplicate (f2 x12a);
map_process f1 f2 f3 f4 (Erase x13a) = Erase (f2 x13a);
map_process f1 f2 f3 f4 (Apply x141 x142) =
  Apply (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x141)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x142);
map_process f1 f2 f3 f4 (Repeat x151 x152) =
  Repeat (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x151)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x152);
map_process f1 f2 f3 f4 (Close x161 x162) =
  Close (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x161)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x162);
map_process f1 f2 f3 f4 (Once x171 x172) =
  Once (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x171)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x172);
map_process f1 f2 f3 f4 (Forget x18) =
  Forget (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x18);

}
