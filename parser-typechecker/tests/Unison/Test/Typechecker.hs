{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Test.Typechecker where

import           Control.Lens           ( view )
import           Control.Lens.Tuple     ( _5 )
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map               as Map
import           Data.Sequence          (Seq)
import           Data.Text              (pack, unpack)
import           Data.Text.IO           (readFile)
import           EasyTest
import           System.FilePath        (joinPath, splitPath, replaceExtension)
import           System.FilePath.Find   (always, extension, find, (==?))
import           System.Directory       ( doesFileExist )
import qualified Unison.Builtin         as Builtin
import           Unison.Codebase.Runtime  ( Runtime, evaluateWatches )
import           Unison.Parser          as Parser
import qualified Unison.Parsers         as Parsers
import qualified Unison.PrettyPrintEnv  as PPE
import qualified Unison.PrintError      as PrintError
import           Unison.Result          (pattern Result, Result)
import qualified Unison.Result          as Result
import qualified Unison.Runtime.Rt1IO   as RT
import           Unison.Symbol          (Symbol, Symbol(..))
import qualified Unison.Term            as Term
import           Unison.Term            ( amap )
import           Unison.Test.Common     (parseAndSynthesizeAsFile, parsingEnv)
import qualified Unison.UnisonFile      as UF
import           Unison.Util.Monoid     (intercalateMap)
import qualified Unison.Var as Var
import qualified Unison.Type as Type
import qualified Unison.Test.Common as Common
import qualified Unison.Names3

type Note = Result.Note Symbol Parser.Ann

type TFile = UF.TypecheckedUnisonFile Symbol Ann
type SynthResult =
  Result (Seq Note)
         (Either Unison.Names3.Names0 TFile)

type EitherResult = Either String TFile


ppEnv :: PPE.PrettyPrintEnv
ppEnv = PPE.fromNames Common.hqLength Builtin.names

expectRight' :: Either String a -> Test a
expectRight' (Left  e) = crash e
expectRight' (Right a) = ok >> pure a

good :: EitherResult -> Test ()
good = void <$> expectRight'

bad :: EitherResult -> Test ()
bad = void <$> EasyTest.expectLeft

test :: Test ()
test = do
  let rt = RT.runtime
  scope "typechecker"
    . tests
    $ [ go rt shouldPassNow   good
      , go rt shouldFailNow   bad
      , go rt shouldPassLater (pending . bad)
      , go rt shouldFailLater (pending . good)
      , scope "Term.substTypeVar" $ do
          -- check that capture avoidance works in substTypeVar
          let v s = Var.nameds s :: Symbol
              tv s = Type.var() (v s)
              v1 s = Symbol 1 (pack s)
              tm :: Term.Term Symbol
              tm = Term.ann() (Term.ann()
                                 (Term.nat() 42)
                                 (Type.introOuter() (v "a") $
                                   Type.arrow() (tv "a") (tv "x")))
                              (Type.forall() (v "a") (tv "a"))
              tm' = Term.substTypeVar (v "x") (tv "a") tm
              expected =
                Term.ann() (Term.ann()
                              (Term.nat() 42)
                              (Type.introOuter() (v1 "a") $
                                Type.arrow() (Type.var() $ v1 "a") (tv "a")))
                           (Type.forall() (v1 "a") (Type.var() $ v1 "a"))
          note $ show tm'
          note $ show expected
          expect $ tm == tm
          expect $ tm' == tm'
          expect $ tm' == expected
          ok
      ]

shouldPassPath, shouldFailPath :: String
shouldPassPath = "unison-src/tests"
shouldFailPath = "unison-src/errors"

shouldPassNow :: IO [FilePath]
shouldPassNow = find always (extension ==? ".u") shouldPassPath

shouldFailNow :: IO [FilePath]
shouldFailNow = find always (extension ==? ".u") shouldFailPath

shouldPassLater :: IO [FilePath]
shouldPassLater = find always (extension ==? ".uu") shouldPassPath

shouldFailLater :: IO [FilePath]
shouldFailLater = find always (extension ==? ".uu") shouldFailPath

go :: Runtime Symbol -> IO [FilePath] -> (EitherResult -> Test ()) -> Test ()
go rt files how = do
  files' <- liftIO files
  tests (makePassingTest rt how <$> files')

showNotes :: Foldable f => String -> PrintError.Env -> f Note -> String
showNotes source env =
  intercalateMap "\n\n" $ PrintError.renderNoteAsANSI 60 env source

decodeResult
  :: String -> SynthResult -> EitherResult--  String (UF.TypecheckedUnisonFile Symbol Ann)
decodeResult source (Result notes Nothing) =
  Left $ showNotes source ppEnv notes
decodeResult source (Result notes (Just (Left errNames))) =
  Left $ showNotes
          source
          (PPE.fromNames Common.hqLength
            (Unison.Names3.shadowing errNames Builtin.names))
          notes
decodeResult _source (Result _notes (Just (Right uf))) =
  Right uf

makePassingTest
  :: Runtime Symbol -> (EitherResult -> Test ()) -> FilePath -> Test ()
makePassingTest rt how filepath = scope shortName $ do
  let valueFile = replaceExtension filepath "ur"
  source <- io $ unpack <$> Data.Text.IO.readFile filepath
  let r = decodeResult source $ parseAndSynthesizeAsFile [] shortName source
  rFileExists <- io $ doesFileExist valueFile
  case (rFileExists, r) of
    (True, Right file) -> do
      values <- io $ unpack <$> Data.Text.IO.readFile valueFile
      let untypedFile = UF.discardTypes file
      let term        = Parsers.parseTerm values parsingEnv
      (bindings, watches) <- io $ either undefined id <$>
        evaluateWatches Builtin.codeLookup
                        mempty
                        (const $ pure Nothing)
                        rt
                        untypedFile
      case term of
        Right tm -> do
          -- compare the the watch expression from the .u with the expr in .ur
          let [watchResult] = view _5 <$> Map.elems watches
              tm' = Term.letRec' False bindings watchResult
          -- note . show $ tm'
          -- note . show $ amap (const ()) tm
          expect $ tm' == amap (const ()) tm
        Left e -> crash $ show e
    _ -> pure ()
  how r
  where shortName = joinPath . drop 1 . splitPath $ filepath
