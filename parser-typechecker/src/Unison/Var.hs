{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}

module Unison.Var where

import Unison.Prelude

import Data.Char (toLower, isLower)
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import Unison.Util.Monoid (intercalateMap)
import Unison.Reference (Reference)

-- | A class for variables. Variables may have auxiliary information which
-- may not form part of their identity according to `Eq` / `Ord`.
class (Show v, ABT.Var v) => Var v where
  typed :: Type -> v
  name :: v -> Text
  freshenId :: Word64 -> v -> v

freshIn :: ABT.Var v => Set v -> v -> v
freshIn = ABT.freshIn

named :: Var v => Text -> v
named n = typed (User n)

uncapitalize :: Var v => v -> v
uncapitalize v = nameds $ go (nameStr v) where
  go (c:rest) = toLower c : rest
  go n = n

missingResult, blank, inferInput, inferOutput, inferAbility,
  inferPatternPureE, inferPatternPureV, inferPatternBindE, inferPatternBindV,
  inferTypeConstructor, inferTypeConstructorArg,
  inferOther :: Var v => v
missingResult = typed MissingResult
blank = typed Blank
inferInput = typed (Inference Input)
inferOutput = typed (Inference Output)
inferAbility = typed (Inference Ability)
inferPatternPureE = typed (Inference PatternPureE)
inferPatternPureV = typed (Inference PatternPureV)
inferPatternBindE = typed (Inference PatternBindE)
inferPatternBindV = typed (Inference PatternBindV)
inferTypeConstructor = typed (Inference TypeConstructor)
inferTypeConstructorArg = typed (Inference TypeConstructorArg)
inferOther = typed (Inference Other)

unnamedTest :: Var v => Text -> v
unnamedTest guid = typed (UnnamedWatch TestWatch guid)

data Type
  -- User provided variables, these should generally be left alone
  = User Text
  -- Variables created during type inference
  | Inference InferenceType
  -- Variables created in `makeSelfContained` for Evaluation
  | RefNamed Reference
  -- Variables created to finish a block that doesn't end with an expression
  | MissingResult
  -- Variables invented for placeholder values inserted by user or by TDNR
  | Blank
  -- An unnamed watch expression of the given kind, for instance:
  --
  --  test> Ok "oog"
  --    has kind "test"
  --  > 1 + 1
  --    has kind ""
  | UnnamedWatch WatchKind Text -- guid
  deriving (Eq,Ord,Show)

type WatchKind = String

pattern RegularWatch = ""
pattern TestWatch = "test"

data InferenceType =
  Ability | Input | Output |
  PatternPureE | PatternPureV |
  PatternBindE | PatternBindV |
  TypeConstructor | TypeConstructorArg |
  Other
  deriving (Eq,Ord,Show)

reset :: Var v => v -> v
reset v = freshenId 0 v

isQualified :: Var v => v -> Bool
isQualified v = Text.any (== '.') (name v)

unqualifiedName :: Var v => v -> Text
unqualifiedName = last . Text.splitOn "." . name

namespaced :: Var v => [v] -> v
namespaced vs = named $ intercalateMap "." name vs

nameStr :: Var v => v -> String
nameStr = Text.unpack . name

nameds :: Var v => String -> v
nameds s = named (Text.pack s)

joinDot :: Var v => v -> v -> v
joinDot prefix v2 =
  if name prefix == "." then named (name prefix `mappend` name v2)
  else named (name prefix `mappend` "." `mappend` name v2)

freshNamed :: Var v => Set v -> Text -> v
freshNamed used n = ABT.freshIn used (named n)

syntheticVars :: Var v => Set v
syntheticVars = Set.fromList . fmap typed $ [
  Inference Ability,
  Inference Input,
  Inference Output,
  Inference PatternPureE,
  Inference PatternPureV,
  Inference PatternBindE,
  Inference PatternBindV,
  Inference TypeConstructor,
  Inference TypeConstructorArg ]

isLowercase :: forall v . Var v => v -> Bool
isLowercase v =
  ok (name $ reset v) && not (isQualified v)
  where
  ok n = (all isLower . take 1 . Text.unpack) n ||
         Set.member n syntheticVarNames
  syntheticVarNames :: Set Text
  syntheticVarNames = Set.map name (syntheticVars @v)
