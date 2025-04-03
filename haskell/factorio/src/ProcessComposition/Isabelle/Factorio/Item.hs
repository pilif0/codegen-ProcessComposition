{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Factorio.Item(Item(..), Flow(..), Flow_meta(..),
     move, unit, merge, split, counit, splits, flowLoc, flowItem, flowRate,
     itemLabel, equal_item, equal_flow)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.Groups_List;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Process;
import qualified ProcessComposition.Isabelle.Rat;

newtype Item = Item String deriving (Prelude.Read, Prelude.Show);

data Flow = Flow Item ProcessComposition.Isabelle.Rat.Rat String
  deriving (Prelude.Read, Prelude.Show);

data Flow_meta =
  Merge Item String ProcessComposition.Isabelle.Rat.Rat
    ProcessComposition.Isabelle.Rat.Rat
  | Unit Item String
  | Split Item String ProcessComposition.Isabelle.Rat.Rat
      ProcessComposition.Isabelle.Rat.Rat
  | Counit Item String
  | Move Item ProcessComposition.Isabelle.Rat.Rat String String
  deriving (Prelude.Read, Prelude.Show);

move ::
  Item ->
    ProcessComposition.Isabelle.Rat.Rat ->
      String ->
        String ->
          ProcessComposition.Isabelle.Process.Process Flow () String Flow_meta;
move i r l k =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res (Flow i r l))
    (ProcessComposition.Isabelle.Resource.res (Flow i r k)) "Move flow"
    (Move i r l k);

unit ::
  Item ->
    String ->
      ProcessComposition.Isabelle.Process.Process Flow () String Flow_meta;
unit i l =
  ProcessComposition.Isabelle.Process.Primitive
    ProcessComposition.Isabelle.Resource.empty
    (ProcessComposition.Isabelle.Resource.res
      (Flow i ProcessComposition.Isabelle.Rat.zero_rat l))
    "Create zero flow" (Unit i l);

merge ::
  Item ->
    String ->
      ProcessComposition.Isabelle.Rat.Rat ->
        ProcessComposition.Isabelle.Rat.Rat ->
          ProcessComposition.Isabelle.Process.Process Flow () String Flow_meta;
merge i l r s =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.parallel
      [ProcessComposition.Isabelle.Resource.res (Flow i r l),
        ProcessComposition.Isabelle.Resource.res (Flow i s l)])
    (ProcessComposition.Isabelle.Resource.res
      (Flow i (ProcessComposition.Isabelle.Rat.plus_rat r s) l))
    "Merge flows" (Merge i l r s);

split ::
  Item ->
    String ->
      ProcessComposition.Isabelle.Rat.Rat ->
        ProcessComposition.Isabelle.Rat.Rat ->
          ProcessComposition.Isabelle.Process.Process Flow () String Flow_meta;
split i l r s =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res
      (Flow i (ProcessComposition.Isabelle.Rat.plus_rat r s) l))
    (ProcessComposition.Isabelle.Resource.parallel
      [ProcessComposition.Isabelle.Resource.res (Flow i r l),
        ProcessComposition.Isabelle.Resource.res (Flow i s l)])
    "Split flow" (Split i l r s);

counit ::
  Item ->
    String ->
      ProcessComposition.Isabelle.Process.Process Flow () String Flow_meta;
counit i l =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res
      (Flow i ProcessComposition.Isabelle.Rat.zero_rat l))
    ProcessComposition.Isabelle.Resource.empty "Discard zero flow" (Counit i l);

splits ::
  Item ->
    String ->
      [ProcessComposition.Isabelle.Rat.Rat] ->
        ProcessComposition.Isabelle.Process.Process Flow () String Flow_meta;
splits i l [] =
  ProcessComposition.Isabelle.Process.Identity
    (ProcessComposition.Isabelle.Resource.res
      (Flow i ProcessComposition.Isabelle.Rat.zero_rat l));
splits i l [n] =
  ProcessComposition.Isabelle.Process.Identity
    (ProcessComposition.Isabelle.Resource.res (Flow i n l));
splits i l [m, n] = split i l m n;
splits i l (a : b : c : ns) =
  ProcessComposition.Isabelle.Process.Seq
    (split i l a
      (ProcessComposition.Isabelle.Groups_List.sum_list (b : c : ns)))
    (ProcessComposition.Isabelle.Process.Par
      (ProcessComposition.Isabelle.Process.Identity
        (ProcessComposition.Isabelle.Resource.res (Flow i a l)))
      (splits i l (b : c : ns)));

flowLoc :: Flow -> String;
flowLoc (Flow x1 x2 x3) = x3;

flowItem :: Flow -> Item;
flowItem (Flow x1 x2 x3) = x1;

flowRate :: Flow -> ProcessComposition.Isabelle.Rat.Rat;
flowRate (Flow x1 x2 x3) = x2;

itemLabel :: Item -> String;
itemLabel (Item x) = x;

equal_item :: Item -> Item -> Bool;
equal_item (Item x) (Item ya) = x == ya;

equal_flow :: Flow -> Flow -> Bool;
equal_flow (Flow x1 x2 x3) (Flow y1 y2 y3) =
  equal_item x1 y1 &&
    ProcessComposition.Isabelle.Rat.equal_rat x2 y2 && x3 == y3;

}
