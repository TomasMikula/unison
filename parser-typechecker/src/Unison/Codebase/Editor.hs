{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Unison.Codebase.Editor where

import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( ifM )
import           Data.Foldable                  ( traverse_ )
import           Data.Sequence                  ( Seq )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Unison.Builtin                as B
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase.Branch         ( Branch
                                                , Branch0
                                                )
import qualified Unison.Codebase.Branch        as Branch
-- import           Unison.DataDeclaration     (DataDeclaration', EffectDeclaration')
import           Unison.FileParsers             ( parseAndSynthesizeFile )
import           Unison.Names                   ( Name
                                                , Names
                                                , NameTarget
                                                , Referent
                                                )
import           Unison.Parser                  ( Ann )
import qualified Unison.Parser                 as Parser
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import           Unison.Result                  ( Note
                                                , Result
                                                )
import qualified Unison.Result                 as Result
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Typechecker.Context    as Context
import qualified Unison.UnisonFile             as UF
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import qualified Unison.Util.Relation          as R
import           Unison.Var                     ( Var )

type BranchName = Name
type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type TypecheckingResult v =
  Result (Seq (Note v Ann))
         (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile' v Ann))
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data AddOutputComponent v =
  AddOutputComponent { implicatedTypes :: Set v, implicatedTerms :: Set v }
  deriving (Show)

data AddOutput v
  = NothingToAdd
  | Added {
          -- The file that we tried to add from
            originalFile :: UF.TypecheckedUnisonFile v Ann
          -- Previously existed only in the file; now added to the codebase.
          , successful :: AddOutputComponent v
          -- Exists in the branch and the file, with the same name and contents.
          , duplicates :: AddOutputComponent v
          -- Already defined in the branch, but with a different name.
          , duplicateReferents :: AddOutputComponent v
          -- Has a colliding name but a different definition than the codebase.
          , collisions :: AddOutputComponent v
          }
          deriving (Show)

data SearchType = Exact | Fuzzy deriving (Show)

data Input
  -- high-level manipulation of names
  = AliasUnconflictedI NameTarget Name Name
  | RenameUnconflictedI NameTarget Name Name
  | UnnameAllI NameTarget Name
  -- low-level manipulation of names
  | AddTermNameI Referent Name
  | AddTypeNameI Reference Name
  | AddPatternNameI Reference Int Name
  | RemoveTermNameI Referent Name
  | RemoveTypeNameI Reference Name
  | RemovePatternNameI Reference Int Name
  -- resolving naming conflicts
  | ChooseTermForNameI Referent Name
  | ChooseTypeForNameI Reference Name
  | ChoosePatternForNameI Reference Int Name
  -- create and remove update directives
  | ListAllUpdatesI
    -- update a term or type, error if it would produce a conflict
  | UpdateTermI Referent Referent
  | UpdateTypeI Reference Reference
    -- clear updates for a term or type
  | RemoveAllTermUpdatesI Referent
  | RemoveAllTypeUpdatesI Reference
  -- resolve update conflicts
  | ChooseUpdateForTermI Referent Referent
  | ChooseUpdateForTypeI Reference Reference
  -- other
  | AddI -- [Name]
  | ListBranchesI
  | SearchByNameI SearchType String
  | SwitchBranchI BranchName
  | ForkBranchI BranchName
  | MergeBranchI BranchName
  | QuitI
  deriving (Show)

data Output v
  = Success Input
  | NoUnisonFile
  | UnknownBranch BranchName
  | UnknownName BranchName NameTarget Name
  | NameAlreadyExists BranchName NameTarget Name
  -- `name` refers to more than one `nameTarget`
  | ConflictedName BranchName NameTarget Name
  | BranchAlreadyExists BranchName
  | ListOfBranches [BranchName]
  | SearchResult BranchName SearchType String [(Name, Referent, Type v Ann)] [(Name, Reference {-, Kind -})] [(Name, Reference, Int, Type v Ann)]
  | AddOutput (AddOutput v)
  | ParseErrors [Parser.Err v]
  | TypeErrors PPE.PrettyPrintEnv [Context.ErrorNote v Ann]
  | DisplayConflicts Branch0
  deriving (Show)

data Command i v a where
  Input :: Command i v i

  -- Presents some output to the user
  Notify :: Output v -> Command i v ()

  Add :: Branch -> UF.TypecheckedUnisonFile' v Ann -> Command i v (AddOutput v)

  Typecheck :: Branch
            -> SourceName
            -> Source
            -> Command i v (TypecheckingResult v)

  -- Load definitions from codebase:
  -- option 1:
      -- LoadTerm :: Reference -> Command i v (Maybe (Term v Ann))
      -- LoadTypeOfTerm :: Reference -> Command i v (Maybe (Type v Ann))
      -- LoadDataDeclaration :: Reference -> Command i v (Maybe (DataDeclaration' v Ann))
      -- LoadEffectDeclaration :: Reference -> Command i v (Maybe (EffectDeclaration' v Ann))
  -- option 2:
      -- LoadTerm :: Reference -> Command i v (Maybe (Term v Ann))
      -- LoadTypeOfTerm :: Reference -> Command i v (Maybe (Type v Ann))
      -- LoadTypeDecl :: Reference -> Command i v (Maybe (TypeLookup.Decl v Ann))
  -- option 3:
      -- TypeLookup :: [Reference] -> Command i v (TypeLookup.TypeLookup)

  ListBranches :: Command i v [BranchName]

  -- Loads a branch by name from the codebase, returning `Nothing` if not found.
  LoadBranch :: BranchName -> Command i v (Maybe Branch)

  -- Returns `Nothing` if a branch by that name already exists.
  NewBranch :: BranchName -> Command i v Bool

  -- Create a new branch which is a copy of the given branch, and assign the
  -- forked branch the given name. Returns `False` if the forked branch name
  -- already exists.
  ForkBranch :: Branch -> BranchName -> Command i v Bool

  -- Merges the branch with the existing branch with the given name. Returns
  -- `False` if no branch with that name exists, `True` otherwise.
  MergeBranch :: BranchName -> Branch -> Command i v Bool

  -- Return the subset of the branch tip which is in a conflicted state.
  -- A conflict is:
  -- * A name with more than one referent.
  -- *
  GetConflicts :: Branch -> Command i v Branch0

  -- RemainingWork :: Branch -> Command i v [RemainingWork]

  -- idea here is to find "close matches" of stuff in the input file, so
  -- can suggest use of preexisting definitions
  -- Search :: UF.TypecheckedUnisonFile' v Ann -> Command v (UF.TypecheckedUnisonFile' v Ann?)

notifyUser :: Var v => Output v -> IO ()
notifyUser o = case o of
  DisplayConflicts branch -> do
    let terms = R.dom $ Branch.termNamespace branch
        patterns = R.dom $ Branch.patternNamespace branch
        types = R.dom $ Branch.typeNamespace branch
    when (not $ null terms) $ do
      putStrLn "🙅 The following terms have conflicts: "
      traverse_ (\x -> putStrLn ("  " ++ unpack x)) terms
    when (not $ null patterns) $ do
      putStrLn "🙅 The following patterns have conflicts: "
      traverse_ (\x -> putStrLn ("  " ++ unpack x)) patterns
    when (not $ null types) $ do
      putStrLn "🙅 The following types have conflicts: "
      traverse_ (\x -> putStrLn ("  " ++ unpack x)) types
    -- TODO: Present conflicting TermEdits and TypeEdits
    -- if we ever allow users to edit hashes directly.
  _ -> putStrLn $ show o

addToBranch :: Var v => Branch -> UF.TypecheckedUnisonFile v Ann -> AddOutput v
addToBranch branch unisonFile
  = let
      branchUpdate = Branch.fromTypecheckedFile unisonFile
      collisions   = Branch.collisions branchUpdate branch
      duplicates   = Branch.duplicates branchUpdate branch
      dupeRefs     = Branch.ours
        $ Branch.diff' (Branch.refCollisions branchUpdate branch) duplicates
      successes = Branch.ours
        $ Branch.diff' branchUpdate (collisions <> duplicates <> dupeRefs)
      mkOutput x =
        uncurry AddOutputComponent $ Branch.intersectWithFile x unisonFile
    in
      Added unisonFile
            (mkOutput successes)
            (mkOutput duplicates)
            (mkOutput dupeRefs)
            (mkOutput collisions)

typecheck
  :: (Monad m, Var v)
  => Codebase m v Ann
  -> Names
  -> SourceName
  -> Text
  -> m (TypecheckingResult v)
typecheck codebase names sourceName src =
  Result.getResult $ parseAndSynthesizeFile
    (((<> B.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
    names
    (unpack sourceName)
    src

builtinBranch :: Branch
builtinBranch = Branch.append (Branch.fromNames B.names) mempty

newBranch :: Monad m => Codebase m v a -> BranchName -> m Bool
newBranch codebase branchName = forkBranch codebase builtinBranch branchName

forkBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
forkBranch codebase branch branchName = do
  ifM (Codebase.branchExists codebase branchName)
      (pure False)
      ((branch ==) <$> Codebase.mergeBranch codebase branchName branch)

mergeBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
mergeBranch codebase branch branchName = ifM
  (Codebase.branchExists codebase branchName)
  (Codebase.mergeBranch codebase branchName branch *> pure True)
  (pure False)

commandLine
  :: forall i v a
   . Var v
  => IO i
  -> Codebase IO v Ann
  -> Free (Command i v) a
  -> IO a
commandLine awaitInput codebase command = do
  Free.fold go command
 where
  go :: forall x . Command i v x -> IO x
  go = \case
    -- Wait until we get either user input or a unison file update
    Input         -> awaitInput
    Notify output -> notifyUser output
    Add branch unisonFile ->
      pure . addToBranch branch $ UF.discardTopLevelTerm unisonFile
    Typecheck branch sourceName source ->
      typecheck codebase (Branch.toNames branch) sourceName source
    ListBranches -> Codebase.branches codebase
    LoadBranch branchName -> Codebase.getBranch codebase branchName
    NewBranch branchName -> newBranch codebase branchName
    ForkBranch branch branchName -> forkBranch codebase branch branchName
    MergeBranch branchName branch -> mergeBranch codebase branch branchName
    GetConflicts branch -> pure $ Branch.conflicts' branch
