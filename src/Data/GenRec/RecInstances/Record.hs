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

module Data.GenRec.RecInstances.Record
  (Record, Reco,
   untag, getLabel,
   (.=.), (.*.), (#),
   emptyRecord
  )
  where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Data.GenRec
import Data.GenRec.Label


-- | * Records

-- | datatype definition
type Record        = (Rec Reco :: [(Symbol, Type)] -> Type)

-- | index type
data Reco

-- | field type
type instance  WrapField Reco     (v :: Type) = v

-- | Type level show utilities
type instance ShowRec Reco         = "Record"
type instance ShowField Reco       = "field named "


type Tagged (l :: Symbol) (v :: Type) = TagField Reco l v
pattern Tagged :: v -> Tagged l v
pattern Tagged v = TagField Label Label v


-- ** Constructors

-- | Pretty Constructor
infix 4 .=.
(.=.) :: Label l -> v -> Tagged l v
l .=. v = Tagged v

-- | For the empty Record
emptyRecord :: Record ('[] :: [(Symbol, Type)])
emptyRecord = EmptyRec

untag :: Tagged l v -> v
untag (TagField _ _ v) = v

-- * Destructors
-- | Get a label
getLabel :: Tagged l v -> Label l
getLabel _ = Label

-- | Lookup
infixl 5 #
r # (l :: Label l) = (##) @Reco @l r l

-- | extension
infixr 2 .*.
(lv :: Tagged l v) .*. r = (.**.)  lv r
-- The Tagged annotation is enough to unify everything

instance ( Show v
         , KnownSymbol l )
  =>
  Show (Tagged l v) where
  show (Tagged v :: TagField Reco l v) =
    symbolVal (proxyFrom (Label @ l)) ++ " : "++ show v
     where proxyFrom :: Label l -> Proxy l
           proxyFrom _ = Proxy

instance Show (Record '[]) where
  show _ = "{}"

instance ( Show v
         , KnownSymbol l)
  =>
  Show (Record '[ '(l, v)]) where
  show (ConsRec lv EmptyRec) =
    '{' : show lv ++ "}"

instance ( Show v
         , KnownSymbol l
         , Show (Record ( '(l', v') ': r )))
  =>
  Show (Record ( '(l, v) ': '(l', v') ': r )) where
  show (ConsRec lv r) =
    let ('{':shr) = show r
    in '{' : show lv ++ ", " ++ shr

v1 = (Label @"boolean" .=. True) .*. emptyRecord
v2 = (Label @"integer" .=. 3) .*. v1
v3 = (Label @"text" .=. "wa") .*. v2
