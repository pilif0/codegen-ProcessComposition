{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Marking.Refined(Lres(..), case_lres, equal_lres,
       equal_lresa, markStudent, markSplit_func, markSplit_cond, markSplit,
       refinement, submitMark, submitSplit_func, submitSplit_cond, submitSplit,
       duplicateToN, collectStudent, swapInterleave, collectionSplit_func,
       collectionSplit_cond, collectionSplit, markingProcess)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Marking.Abstract;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Process;

data Lres a = Student a | Submission a | Mark a | MarkSubmitted a
  | MarksReleased deriving (Prelude.Read, Prelude.Show);

case_lres ::
  forall a b. (a -> b) -> (a -> b) -> (a -> b) -> (a -> b) -> b -> Lres a -> b;
case_lres f1 f2 f3 f4 f5 MarksReleased = f5;
case_lres f1 f2 f3 f4 f5 (MarkSubmitted x4) = f4 x4;
case_lres f1 f2 f3 f4 f5 (Mark x3) = f3 x3;
case_lres f1 f2 f3 f4 f5 (Submission x2) = f2 x2;
case_lres f1 f2 f3 f4 f5 (Student x1) = f1 x1;

equal_lres :: forall a. (Eq a) => Lres a -> Lres a -> Bool;
equal_lres MarksReleased y =
  case_lres (\ _ -> False) (\ _ -> False) (\ _ -> False) (\ _ -> False) True y;
equal_lres (MarkSubmitted s) y =
  case_lres (\ _ -> False) (\ _ -> False) (\ _ -> False) (\ a -> s == a) False
    y;
equal_lres (Mark s) y =
  case_lres (\ _ -> False) (\ _ -> False) (\ a -> s == a) (\ _ -> False) False
    y;
equal_lres (Submission s) y =
  case_lres (\ _ -> False) (\ a -> s == a) (\ _ -> False) (\ _ -> False) False
    y;
equal_lres (Student s) y =
  case_lres (\ a -> s == a) (\ _ -> False) (\ _ -> False) (\ _ -> False) False
    y;

equal_lresa :: forall a. (Eq a) => Lres a -> Lres a -> Bool;
equal_lresa = equal_lres;

instance (Eq a) => Eq (Lres a) where {
  a == b = equal_lresa a b;
};

markStudent ::
  forall a.
    (a -> String) ->
      a -> ProcessComposition.Isabelle.Process.Process (Lres a)
             ProcessComposition.Isabelle.Marking.Abstract.Cres String ();
markStudent name s =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res (Submission s))
    (ProcessComposition.Isabelle.Resource.res (Mark s))
    ("Mark submission of " ++ name s) ();

markSplit_func ::
  forall a.
    [a] ->
      (a -> String) ->
        ProcessComposition.Isabelle.Resource.Resource (Lres a)
          ProcessComposition.Isabelle.Marking.Abstract.Cres ->
          ProcessComposition.Isabelle.Resource.Resource (Lres a)
            ProcessComposition.Isabelle.Marking.Abstract.Cres ->
            String ->
              () -> ProcessComposition.Isabelle.Process.Process (Lres a)
                      ProcessComposition.Isabelle.Marking.Abstract.Cres String
                      ();
markSplit_func students name a b l m =
  ProcessComposition.Isabelle.Process.par_process_list
    (map (markStudent name) students);

markSplit_cond ::
  forall a.
    (Eq a) => [a] ->
                ProcessComposition.Isabelle.Resource.Resource (Lres a)
                  ProcessComposition.Isabelle.Marking.Abstract.Cres ->
                  ProcessComposition.Isabelle.Resource.Resource (Lres a)
                    ProcessComposition.Isabelle.Marking.Abstract.Cres ->
                    String -> () -> Bool;
markSplit_cond students a b l m =
  a == ProcessComposition.Isabelle.Resource.parallel
         (map (ProcessComposition.Isabelle.Resource.res . Submission)
           students) &&
    b == ProcessComposition.Isabelle.Resource.parallel
           (map (ProcessComposition.Isabelle.Resource.res . Mark) students);

markSplit ::
  forall a.
    (Eq a) => [a] ->
                (a -> String) ->
                  ProcessComposition.Isabelle.Process.Process (Lres a)
                    ProcessComposition.Isabelle.Marking.Abstract.Cres String
                    () ->
                    ProcessComposition.Isabelle.Process.Process (Lres a)
                      ProcessComposition.Isabelle.Marking.Abstract.Cres String
                      ();
markSplit students name =
  ProcessComposition.Isabelle.Process.process_subst (markSplit_cond students)
    (markSplit_func students name);

refinement ::
  forall a.
    [a] ->
      ProcessComposition.Isabelle.Marking.Abstract.Lres ->
        ProcessComposition.Isabelle.Resource.Resource (Lres a)
          ProcessComposition.Isabelle.Marking.Abstract.Cres;
refinement students ProcessComposition.Isabelle.Marking.Abstract.MarksReleased =
  ProcessComposition.Isabelle.Resource.res MarksReleased;
refinement students ProcessComposition.Isabelle.Marking.Abstract.MarksSubmitted
  = ProcessComposition.Isabelle.Resource.parallel
      (map (ProcessComposition.Isabelle.Resource.res . MarkSubmitted) students);
refinement students ProcessComposition.Isabelle.Marking.Abstract.Marks =
  ProcessComposition.Isabelle.Resource.parallel
    (map (ProcessComposition.Isabelle.Resource.res . Mark) students);
refinement students ProcessComposition.Isabelle.Marking.Abstract.Submissions =
  ProcessComposition.Isabelle.Resource.parallel
    (map (ProcessComposition.Isabelle.Resource.res . Submission) students);
refinement students ProcessComposition.Isabelle.Marking.Abstract.Students =
  ProcessComposition.Isabelle.Resource.parallel
    (map (ProcessComposition.Isabelle.Resource.res . Student) students);

submitMark ::
  forall a.
    (a -> String) ->
      a -> ProcessComposition.Isabelle.Process.Process (Lres a)
             ProcessComposition.Isabelle.Marking.Abstract.Cres String ();
submitMark name s =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res (Mark s))
    (ProcessComposition.Isabelle.Resource.res (MarkSubmitted s))
    ("Submit mark of " ++ name s) ();

submitSplit_func ::
  forall a.
    [a] ->
      (a -> String) ->
        ProcessComposition.Isabelle.Resource.Resource (Lres a)
          ProcessComposition.Isabelle.Marking.Abstract.Cres ->
          ProcessComposition.Isabelle.Resource.Resource (Lres a)
            ProcessComposition.Isabelle.Marking.Abstract.Cres ->
            String ->
              () -> ProcessComposition.Isabelle.Process.Process (Lres a)
                      ProcessComposition.Isabelle.Marking.Abstract.Cres String
                      ();
submitSplit_func students name a b l m =
  ProcessComposition.Isabelle.Process.par_process_list
    (map (submitMark name) students);

submitSplit_cond ::
  forall a.
    (Eq a) => [a] ->
                ProcessComposition.Isabelle.Resource.Resource (Lres a)
                  ProcessComposition.Isabelle.Marking.Abstract.Cres ->
                  ProcessComposition.Isabelle.Resource.Resource (Lres a)
                    ProcessComposition.Isabelle.Marking.Abstract.Cres ->
                    String -> () -> Bool;
submitSplit_cond students a b l m =
  a == ProcessComposition.Isabelle.Resource.parallel
         (map (ProcessComposition.Isabelle.Resource.res . Mark) students) &&
    b == ProcessComposition.Isabelle.Resource.parallel
           (map (ProcessComposition.Isabelle.Resource.res . MarkSubmitted)
             students);

submitSplit ::
  forall a.
    (Eq a) => [a] ->
                (a -> String) ->
                  ProcessComposition.Isabelle.Process.Process (Lres a)
                    ProcessComposition.Isabelle.Marking.Abstract.Cres String
                    () ->
                    ProcessComposition.Isabelle.Process.Process (Lres a)
                      ProcessComposition.Isabelle.Marking.Abstract.Cres String
                      ();
submitSplit students name =
  ProcessComposition.Isabelle.Process.process_subst (submitSplit_cond students)
    (submitSplit_func students name);

duplicateToN ::
  forall a b c d.
    ProcessComposition.Isabelle.Arith.Nat ->
      a -> ProcessComposition.Isabelle.Process.Process b a c d;
duplicateToN v r =
  (if ProcessComposition.Isabelle.Arith.equal_nat v
        ProcessComposition.Isabelle.Arith.zero_nat
    then ProcessComposition.Isabelle.Process.Erase r
    else (if ProcessComposition.Isabelle.Arith.equal_nat
               (ProcessComposition.Isabelle.Arith.minus_nat v
                 ProcessComposition.Isabelle.Arith.one_nat)
               ProcessComposition.Isabelle.Arith.zero_nat
           then ProcessComposition.Isabelle.Process.Identity
                  (ProcessComposition.Isabelle.Resource.copyable r)
           else ProcessComposition.Isabelle.Process.Seq
                  (ProcessComposition.Isabelle.Process.Duplicate r)
                  (ProcessComposition.Isabelle.Process.Par
                    (ProcessComposition.Isabelle.Process.Identity
                      (ProcessComposition.Isabelle.Resource.copyable r))
                    (duplicateToN
                      (ProcessComposition.Isabelle.Arith.suc
                        (ProcessComposition.Isabelle.Arith.minus_nat
                          (ProcessComposition.Isabelle.Arith.minus_nat v
                            ProcessComposition.Isabelle.Arith.one_nat)
                          ProcessComposition.Isabelle.Arith.one_nat))
                      r))));

collectStudent ::
  forall a.
    (a -> String) ->
      a -> ProcessComposition.Isabelle.Process.Process (Lres a)
             ProcessComposition.Isabelle.Marking.Abstract.Cres String ();
collectStudent name s =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.resource_par
      (ProcessComposition.Isabelle.Resource.copyable
        ProcessComposition.Isabelle.Marking.Abstract.Instructions)
      (ProcessComposition.Isabelle.Resource.res (Student s)))
    (ProcessComposition.Isabelle.Resource.res (Submission s))
    ("Collect submission of " ++ name s) ();

swapInterleave ::
  forall a b c d.
    [ProcessComposition.Isabelle.Resource.Resource a b] ->
      [ProcessComposition.Isabelle.Resource.Resource a b] ->
        ProcessComposition.Isabelle.Process.Process a b c d;
swapInterleave (v : va) [] =
  ProcessComposition.Isabelle.Process.Identity
    (ProcessComposition.Isabelle.Resource.parallel (v : va));
swapInterleave [] ys =
  ProcessComposition.Isabelle.Process.Identity
    (ProcessComposition.Isabelle.Resource.parallel ys);
swapInterleave (x : xs) (y : ys) =
  ProcessComposition.Isabelle.Process.Seq
    (ProcessComposition.Isabelle.Process.par_process_list
      [ProcessComposition.Isabelle.Process.Identity x,
        ProcessComposition.Isabelle.Process.Swap
          (ProcessComposition.Isabelle.Resource.parallel xs) y,
        ProcessComposition.Isabelle.Process.Identity
          (ProcessComposition.Isabelle.Resource.parallel ys)])
    (ProcessComposition.Isabelle.Process.Par
      (ProcessComposition.Isabelle.Process.Identity
        (ProcessComposition.Isabelle.Resource.resource_par x y))
      (swapInterleave xs ys));

collectionSplit_func ::
  forall a.
    [a] ->
      (a -> String) ->
        ProcessComposition.Isabelle.Resource.Resource (Lres a)
          ProcessComposition.Isabelle.Marking.Abstract.Cres ->
          ProcessComposition.Isabelle.Resource.Resource (Lres a)
            ProcessComposition.Isabelle.Marking.Abstract.Cres ->
            String ->
              () -> ProcessComposition.Isabelle.Process.Process (Lres a)
                      ProcessComposition.Isabelle.Marking.Abstract.Cres String
                      ();
collectionSplit_func students name a b l m =
  ProcessComposition.Isabelle.Process.seq_process_list
    [ProcessComposition.Isabelle.Process.Par
       (duplicateToN (ProcessComposition.Isabelle.List.size_list students)
         ProcessComposition.Isabelle.Marking.Abstract.Instructions)
       (ProcessComposition.Isabelle.Process.Identity
         (ProcessComposition.Isabelle.Resource.parallel
           (map (ProcessComposition.Isabelle.Resource.res . Student)
             students))),
      swapInterleave
        (ProcessComposition.Isabelle.List.replicate
          (ProcessComposition.Isabelle.List.size_list students)
          (ProcessComposition.Isabelle.Resource.copyable
            ProcessComposition.Isabelle.Marking.Abstract.Instructions))
        (map (ProcessComposition.Isabelle.Resource.res . Student) students),
      ProcessComposition.Isabelle.Process.par_process_list
        (map (collectStudent name) students)];

collectionSplit_cond ::
  forall a.
    (Eq a) => [a] ->
                ProcessComposition.Isabelle.Resource.Resource (Lres a)
                  ProcessComposition.Isabelle.Marking.Abstract.Cres ->
                  ProcessComposition.Isabelle.Resource.Resource (Lres a)
                    ProcessComposition.Isabelle.Marking.Abstract.Cres ->
                    String -> () -> Bool;
collectionSplit_cond students a b l m =
  a == ProcessComposition.Isabelle.Resource.resource_par
         (ProcessComposition.Isabelle.Resource.copyable
           ProcessComposition.Isabelle.Marking.Abstract.Instructions)
         (ProcessComposition.Isabelle.Resource.parallel
           (map (ProcessComposition.Isabelle.Resource.res . Student)
             students)) &&
    b == ProcessComposition.Isabelle.Resource.parallel
           (map (ProcessComposition.Isabelle.Resource.res . Submission)
             students);

collectionSplit ::
  forall a.
    (Eq a) => [a] ->
                (a -> String) ->
                  ProcessComposition.Isabelle.Process.Process (Lres a)
                    ProcessComposition.Isabelle.Marking.Abstract.Cres String
                    () ->
                    ProcessComposition.Isabelle.Process.Process (Lres a)
                      ProcessComposition.Isabelle.Marking.Abstract.Cres String
                      ();
collectionSplit students name =
  ProcessComposition.Isabelle.Process.process_subst
    (collectionSplit_cond students) (collectionSplit_func students name);

markingProcess ::
  forall a.
    (Eq a) => [a] ->
                (a -> String) ->
                  ProcessComposition.Isabelle.Process.Process (Lres a)
                    ProcessComposition.Isabelle.Marking.Abstract.Cres String ();
markingProcess students name =
  submitSplit students name
    (markSplit students name
      (collectionSplit students name
        (ProcessComposition.Isabelle.Process.process_refineRes
          (refinement students) id
          ProcessComposition.Isabelle.Marking.Abstract.markingProcess)));

}
