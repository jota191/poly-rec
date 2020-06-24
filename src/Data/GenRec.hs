{-|
Module      : Data.GenRecord
Description : polykinded extensible record library
Copyright   : (c) Juan García Garland,
                  Marcos Viera, 2019-2020
License     : GPL
Maintainer  : jpgarcia@fing.edu.uy
Stability   : experimental
Portability : POSIX

Extensible records/row polymorphism are features not implemented in
Haskell. Some other functional languages do it, like Purescript. GHC
extensions allowing type level-programming allow us to encode them in
Haskell.

Let us define records as a (partial) mapping from names (fields, wich
are static) to values.

There are many implementations out there. This is yet another one,
inspired in the HList library. It arose when programming the AspectAG
library.  Before, we depended on HList. Then we choose to implement a
record library from scratch for two reasons:

 * HList is experimental, it does not maintain a stable interface.
 * We prefer a solution that fits better in our use case.

AspectAG is a library to encode type safe attribute grammars.
Statically checked extensible records are used everywhere, knowing at
compile time the structure of the grammar, and checking if it is
well-formed.

Some example structures in AspectAG library are:

* Records: that's it, plane extensible records: Mappings from names to
  values.

* Attributions: mappings from attribute names to values. This is the
  same idea that the one for records, but we wanted to have different
  types for each structure, and for each label. This means that our
  labels must be polyinded.

* Children Records: That is a map from children to attibutions. It is
  a record of records. 

One common way to implement a record is by using a GADT. For instance indexed by
the list of pairs (label, value). We want labels polykinded, and values are
usually of kind Type, what makes sense since Type is the kind of
inhabited types, and records store values.  However, in cases such as
our children records, where we store attributions that are also
represented by an indexed GADT, we would like to be able to reflect
some of this structural information at the indexes of the record. This
can be achieved if we are polymorphic in the kind of the types
corresponding to the record fields.
-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE DataKinds,
             TypeOperators,
             PolyKinds,
             GADTs,
             TypeInType,
             RankNTypes,
             StandaloneDeriving,
             FlexibleInstances,
             FlexibleContexts,
             ConstraintKinds,
             MultiParamTypeClasses,
             FunctionalDependencies,
             UndecidableInstances,
             ScopedTypeVariables,
             TypeFamilies,
             InstanceSigs,
             AllowAmbiguousTypes,
             TypeApplications,
             PatternSynonyms
#-}

module Data.GenRec
  -- (
  --   Rec(ConsRec, EmptyRec),
  --   TagField(TagField),
  --   WrapField,
  --   UnWrap,
  --   untagField,
  --   (.=.),
  --   (#),
  --   (.*.),
  --   Cmp,
  --   ShowRec,
  --   ShowField,
  --   OpLookup(OpLookup),
  --   lookup,
  --   OpExtend(OpExtend),
  --   -- extend, TODO
  --   OpUpdate(OpUpdate),
  --   update,
  --   emptyGenRec,
  --   module Data.GenRec.Label
  -- ) 
  where

import Data.Kind
import Data.Proxy
import Prelude hiding (lookup)

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Ord


import GHC.TypeLits
import GHC.Exts (Any)
import Data.Type.Require

type family Cmp (a :: k) (b :: k') :: Ordering where
  Cmp a a = 'EQ
  Cmp x y = Compare x y


-- | Record data structure for generic records (Internal). The `c`
-- index indicates the kind of record (for each record instance, the
-- user should define an index). The `r` index represents the mapping
-- from labels to values.  Labels are of kind `k'`. Values are still
-- polykinded (`k''`) since rich information can be statically
-- represented here (for instance, when a record of records, or a
-- record of Vectors is represented).  `k'` must implement the `Cmp`
-- family, although it is not visible here for simplicity. Records are
-- built putting fields ordered according to the `Cmp` result of its
-- labels. __This constructors are not intended to be used to build__
-- __records__, (`ConsRec` is the problematic one). Use the smart
-- constructors `emptyRecord` and `.*.` instead. We export the
-- constructors to pattern match on them. Although there are solutions
-- to hide Constructors while supporting pattern matching, we kept
-- it simple

type Label l = Sing l

data Rec (c :: k) (r :: [(k', k'')]) :: Type where
  EmptyRec :: Rec c '[] -- ^ empty record
  ConsRec :: TagField c l v -> Rec c r -> Rec c ('( l, v) ': r) -- ^
-- `ConsRec` takes a tagged field (`TagField`) and a record, to build
-- a new record. Recall that fields should be ordered.

-- | The empty Record. Note that it is polymorphic on the kind of record `c`.
emptyGenRec = EmptyRec

-- | 'TagField'
data TagField (c :: k) (l :: k') (v :: k'') where
  TagField :: Proxy c -> Sing l -> WrapField c v -> TagField c l v -- ^
-- `TagField` tags a value `v` with record and label information. `v`
-- is polykinded, for instance we could be tagging some kind of
-- record, because then we would build a matrix. In that case 'k''
-- could be something as `[(kindforlabels, Type)]`. But `TagField`
-- contains inhabited values, it tags values of a type of kind `Type`.
-- In this example perhaps some value of
-- type `Rec Something [(kindforlabels, Type)]`.
-- That is the role of the `WrapField` family. Given `c`, the kind of
-- record, and `v`, ir computes the wrapper.


-- | TagField operator, note that 'c' will be ambiguous if not annotated.
infix 4 .=.
(l :: Sing l) .=. (v :: v) = TagField undefined l v


-- | Given a type of record and its index, it computes the type of
-- record inhabitants
type family  WrapField (c :: k')  (v :: k) :: Type


-- | The inverse of `WrapField`
type family UnWrap (t :: Type) :: [(k, k')]
type instance UnWrap (Rec c (r :: [(k, k')])) = (r :: [(k, k')])

-- | This is the destructor of `TagField`. Note the use of `WrapField` here.
untagField :: TagField c l v -> WrapField c v
untagField (TagField lc lv v) = v


-- | Function to show the name of records (Record, Mapping, etc):
type family ShowRec c :: Symbol

-- | Function to show the field of the record ("field named", "children", "tag:", etc)
type family ShowField c :: Symbol

-- | TODO: mover
type family FoldOrdering (cond :: Ordering)
                         (lt :: k') (eq :: k') (gt :: k') :: k' where
  FoldOrdering LT lt eq gt = lt
  FoldOrdering EQ lt eq gt = eq
  FoldOrdering GT lt eq gt = gt

type family Lookup (c :: cat) (l :: lk) (r :: [(lk, vk)])
 where
  Lookup c l '[] = TypeError (LookupError c l '[])
  Lookup c l ('(l', v) ': r ) =
    FoldOrdering (Compare l l')
                 (TypeError (LookupError c l ('(l', v) ': r ))) 
                 v
                 (Lookup c l r)

type family LookupError (c :: cat) (l :: lk) (r :: [(lk, vk)]) :: ErrorMessage
type instance LookupError c l r  =
  (Text "field not Found on " :<>: Text (ShowRec c)
   :$$: Text "looking up the " :<>: Text (ShowField c)
   :<>: Text " " :<>: ShowTE l
   :$$: Text "in the structure " :<>: ShowType r)

(#) :: forall cat lk fk (c :: cat) (r :: [(lk, fk)]) (l :: lk).
         SOrd lk => Rec c r -> Label l -> WrapField c (Lookup c l r)
EmptyRec      # l = sUndefined
(ConsRec (TagField c l' v) r) # l =
  case sCompare l l' of
    SLT -> sUndefined
    SEQ -> v
    SGT -> r # l

type family Update (c :: cat) (l :: lk) (v :: vk) (r::[(lk, fk)]) :: [(lk, fk)]
 where
  Update c l v '[] = TypeError (Text "TODO: update error")
  Update c l v ('(l', v') ': r) =
    FoldOrdering (Compare l l')
                 (TypeError (Text "TODO: update error"))
                 ('(l, v) ': r)
                 ('(l',v') ': Update c l v r)

update :: forall cat lk fk (c :: cat) (l :: lk) (v :: fk) (r :: [(lk, fk)]).
  SOrd lk => Label l -> Proxy v -> WrapField c v -> Rec c r
  -> Rec c (Update c l v r)
update l proxyv v EmptyRec = sUndefined
update l proxyv v (ConsRec lv@(TagField c l' v') r) =
  case sCompare l l' of
    SLT -> sUndefined
    SEQ -> ConsRec (TagField c l v) r
    SGT -> ConsRec lv $ update l proxyv v r

type family Extend (c :: cat) (l :: lk) (v :: vk) (r::[(lk, fk)]) :: [(lk, fk)]
 where
  Extend c l v '[] = '[ '(l, v)]
  Extend c l v ('(l',v') ': r) =
    FoldOrdering (Compare l l')
                 ('(l,v) ': '(l',v') ': r)
                 (TypeError (Text "TODO: extend Error"))
                 ('(l',v') ': Extend c l v r)

infixr 2 .*.
(.*.) :: forall cat lk fk (c :: cat) (l :: lk) (v :: fk) (r :: [(lk, fk)]).
  SOrd lk => TagField c l v -> Rec c r -> Rec c (Extend c l v r)
tf .*. EmptyRec = ConsRec tf EmptyRec
lv@(TagField _ l v) .*. re@(ConsRec lv'@(TagField _ l' v') r) =
  case sCompare l l' of
    SLT -> ConsRec lv re
    SEQ -> sUndefined
    SGT -> ConsRec lv' $ lv .*. r

