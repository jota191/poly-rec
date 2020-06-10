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
---import Data.GenRec.Label
import Data.Type.Require
import Prelude hiding (lookup)

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Ord


import GHC.TypeLits



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

-- -- | comparisson of Labels, this family is polykinded, each record-like
-- -- structure must implement this family for its labels
-- type family Cmp (a :: k) (b :: k) :: Ordering

-- -- | Instance for Symbols
-- type instance Cmp (a :: Symbol) (b :: Symbol) = CmpSymbol a b


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
  Lookup c l '[] = TypeError (Text "TODO: lookup error")
  Lookup c l ('(l', v) ': r ) =
    FoldOrdering (Compare l l')
                 (TypeError (Text "TODO: lookup error"))
                 v
                 (Lookup c l r)

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
  SOrd lk => Label l -> WrapField c v -> Rec c r -> Rec c (Update c l v r)
update l v EmptyRec = sUndefined
update l v (ConsRec lv@(TagField c l' v') r) =
  case sCompare l l' of
    SLT -> sUndefined
    SEQ -> ConsRec (TagField c l v) r
    SGT -> ConsRec lv $ update l (v :: WrapField c v) r

{-
-- | The update function. Given a `Label` and value, and a `Record`
-- containing this label, it updates the value. It could change its
-- type. It raises a custom type error if there is no field
-- labelled with l.
update (l :: Label l) (v :: v) (r :: Rec c r) =
  req Proxy (OpUpdate @l @c @v @r l v r)

-- | The lookup function. Given a `Label` and a `Record`, it returns
-- the field at that position. It raises a custom type
-- error if there is no field labelled with l.
lookup (l :: Label l) (r :: Rec c r) =
  req Proxy (OpLookup @c @l @r l r)

-- ** Extension

-- | extension operator (wrapper)
data OpExtend (c :: Type)
              (l  :: k)
              (v  :: k')
              (r  :: [(k, k')]) :: Type where
  OpExtend :: Label l -> WrapField c v -> Rec c r
           -> OpExtend c l v r

-- | Extension operator (inner)
data OpExtend' (b   :: Ordering)
               (c   :: Type)
               (l   :: k)
               (v   :: k')
               (r   :: [(k, k')]) :: Type where
  OpExtend' :: Proxy b -> Label l -> WrapField c v -> Rec c r
           -> OpExtend' b c l v r

-- | extending an empty record
instance
  Require (OpExtend c l v '[]) ctx where
  type ReqR (OpExtend c l v '[]) =
    Rec c '[ '(l , v)]
  req ctx (OpExtend l v EmptyRec) =
    ConsRec (TagField (Label @c) l v) EmptyRec

-- | wrapper instance

instance
  Require (OpExtend' (Cmp l l') c l v ('(l', v') : r)) ctx
  =>
  Require (OpExtend c l v ( '(l', v') ': r)) ctx where
  type ReqR (OpExtend c l v ( '(l', v') ': r)) =
    ReqR (OpExtend' (Cmp l l') c l v ( '(l', v') ': r))
  req ctx (OpExtend l v (r :: Rec c ( '(l', v') ': r)) ) =
    req ctx (OpExtend' @(Cmp l l') @l @c @v Proxy l v r)

-- | keep looking
instance
  (Require (OpExtend c l v r) ctx
  , ( '(l', v') ': r0 ) ~ a
  , ReqR (OpExtend c l v r) ~ Rec c r0
  )
  =>
  Require (OpExtend' 'GT c l v ( '(l', v') ': r)) ctx where
  type ReqR (OpExtend' 'GT c l v ( '(l', v') ': r)) =
    Rec c ( '(l', v') ': UnWrap (ReqR (OpExtend c l v r)))
  req ctx (OpExtend' Proxy l v (ConsRec lv r)) =
    ConsRec lv $ req ctx (OpExtend @_ @_ @v l v r)

instance
  Require (OpExtend' 'LT c l v ( '(l', v') ': r)) ctx where
  type ReqR (OpExtend' 'LT c l v ( '(l', v') ': r)) =
    Rec c ( '(l, v) ': ( '(l', v') ': r))
  req ctx (OpExtend' Proxy l v r) =
    ConsRec (TagField Label l v) r

instance
  (Require (OpError (Text "cannot extend " :<>: Text (ShowRec c)
                     -- :<>: Text " because the label (" :<>: ShowT l
                     -- :<>: Text ") already exists"
                    :$$: Text "colision in " :<>: Text (ShowField c)
                     :<>: Text " ":<>: ShowTE l)) ctx)
  =>
  Require (OpExtend' 'EQ c l v ( '(l, v') ': r)) ctx where
  type ReqR (OpExtend' 'EQ c l v ( '(l, v') ': r)) = ()
  req ctx = undefined


-- | '.*.' the pretty cons, hiding require
infixr 2 .*.
(TagField c l v :: TagField c l v) .*. (r :: Rec c r) =
  req emptyCtx (OpExtend @l @c @v @r l v r)
-}
