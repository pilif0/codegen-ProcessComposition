{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ProcessPaths(Process_inner(..), subprocessStep,
    subprocess)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Option;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Process;

data Process_inner = SeqL | SeqR | ParL | ParR | OptL | OptR | Rep
  deriving (Prelude.Read, Prelude.Show);

subprocessStep ::
  forall a b c d.
    Process_inner ->
      ProcessComposition.Isabelle.Process.Process a b c d ->
        Maybe (ProcessComposition.Isabelle.Process.Process a b c d);
subprocessStep SeqL p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq x _ -> Just x;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep SeqR p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ a -> Just a;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep ParL p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par x _ -> Just x;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep ParR p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ a -> Just a;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep OptL p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt x _ -> Just x;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep OptR p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ a -> Just a;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep Rep p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent a -> Just a;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });

subprocess ::
  forall a b c d.
    [Process_inner] ->
      ProcessComposition.Isabelle.Process.Process a b c d ->
        Maybe (ProcessComposition.Isabelle.Process.Process a b c d);
subprocess p x =
  ProcessComposition.Isabelle.List.foldl
    (\ a b -> ProcessComposition.Isabelle.Option.bind a (subprocessStep b))
    (Just x) p;

}
