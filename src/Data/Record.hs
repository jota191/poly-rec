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

module Data.Record where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Data.GenRecord
import Data.GenRecord.Label


-- | * Records

-- | datatype definition
type Record        = Rec Reco

-- | index type
data Reco

-- | field type
type instance  WrapField Reco     (v :: Type) = v

-- | Type level show utilities
type instance ShowRec Reco         = "Record"
type instance ShowField Reco       = "field named "


type Tagged = TagField Reco
pattern Tagged :: v -> Tagged l v
pattern Tagged v = TagField Label Label v


-- ** Constructors

-- | Pretty Constructor
infixr 4 .=.
(.=.) :: Label l -> v -> Tagged l v
l .=. v = Tagged v

-- | For the empty Record
emptyRecord :: Record '[]
emptyRecord = EmptyRec

unTagged :: Tagged l v -> v
unTagged (TagField _ _ v) = v

-- * Destructors
-- | Get a label
label :: Tagged l v -> Label l
label _ = Label
