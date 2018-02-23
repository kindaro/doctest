{-# LANGUAGE CPP #-}
module Runner (
  runModules
, Summary(..)
) where

import           Prelude hiding (putStr, putStrLn, error)

#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid hiding ((<>))
import           Control.Applicative
#endif

import           Control.Monad hiding (forM_)
import           Text.Printf (printf)
import           System.IO (hPutStrLn, hPutStr, stderr, hIsTerminalDevice)
import           Data.Foldable (forM_)

import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import           Interpreter (Interpreter)
import qualified Interpreter
import           Parse
import           Location
import           Property
import           Runner.Example
import           Report

data TestType = Specification | QuickCheckProperty

instance Show TestType where
    show Specification = "HSpec specification"
    show QuickCheckProperty = "QuickCheck property"

-- | Run all examples from a list of modules.
runModules :: Bool -> Bool -> Bool -> Interpreter -> [Module [Located DocTest]] -> IO Summary
runModules fastMode preserveIt verbose repl modules = do
  isInteractive <- hIsTerminalDevice stderr
  ReportState _ _ s <- (`execStateT` ReportState 0 isInteractive mempty {sExamples = c}) $ do
    forM_ modules $ runModule fastMode preserveIt verbose repl

    -- report final summary
    when verbose $ report $ "# Final summary:"
    gets (show . reportStateSummary) >>= report

  return s
  where
    c = (sum . map count) modules

-- | Count number of expressions in given module.
count :: Module [Located DocTest] -> Int
count (Module _ setup tests) = sum (map length tests) + maybe 0 length setup

-- | Run all examples from given module.
runModule :: Bool -> Bool -> Bool -> Interpreter -> Module [Located DocTest] -> Report ()
runModule fastMode preserveIt verbose repl (Module module_ setup examples) = do

  Summary _ _ e0 f0 <- gets reportStateSummary

  forM_ setup $
    runTestGroup preserveIt verbose repl reload

  Summary _ _ e1 f1 <- gets reportStateSummary

  -- only run tests, if setup does not produce any errors/failures
  when (e0 == e1 && f0 == f1) $
    forM_ examples $
      runTestGroup preserveIt verbose repl setup_
  where
    reload :: IO ()
    reload = do
      unless fastMode $
        -- NOTE: It is important to do the :reload first! See
        -- https://ghc.haskell.org/trac/ghc/ticket/5904, which results in a
        -- panic on GHC 7.4.1 if you do the :reload second.
        void $ Interpreter.safeEval repl ":reload"
      void $ Interpreter.safeEval repl $ ":m *" ++ module_

      when preserveIt $
        -- Evaluate a dumb expression to populate the 'it' variable NOTE: This is
        -- one reason why we cannot have safeEval = safeEvalIt: 'it' isn't set in
        -- a fresh GHCi session.
        void $ Interpreter.safeEval repl $ "()"

    setup_ :: IO ()
    setup_ = do
      reload
      forM_ setup $ \l -> forM_ l $ \(Located _ x) -> case x of
        Property _  -> return ()
        Example e _ -> void $ safeEvalWith preserveIt repl e

reportStart :: Location -> Expression -> Bool -> TestType -> Report ()
reportStart loc expression verbose testType = when verbose $ do
  when verbose $
    report (printf "### Started execution at %s.\n### %s:\n%s"
        (show loc) (show testType) expression)

reportFailure :: Location -> Expression -> Bool -> Report ()
reportFailure loc expression verbose = do
  report (printf "### Failure in %s: expression `%s'" (show loc) expression)
  updateSummary (Summary 0 1 0 1)

reportError :: Location -> Expression -> String -> Bool -> Report ()
reportError loc expression err verbose = do
  report (printf "### Error in %s: expression `%s'" (show loc) expression)
  report err
  updateSummary (Summary 0 1 1 0)

reportSuccess :: Bool -> Report ()
reportSuccess verbose = do
  when verbose $ report "### Successful!"
  updateSummary (Summary 0 1 0 0)

reportSeparator :: Bool -> Report ()
reportSeparator verbose = when verbose $ report ""

updateSummary :: Summary -> Report ()
updateSummary summary = do
  ReportState n f s <- get
  put (ReportState n f $ s `mappend` summary)

-- | Run given test group.
--
-- The interpreter state is zeroed with @:reload@ first.  This means that you
-- can reuse the same 'Interpreter' for several test groups.
runTestGroup :: Bool -> Bool -> Interpreter -> IO () -> [Located DocTest] -> Report ()
runTestGroup preserveIt verbose repl setup tests = do

  -- report intermediate summary
  when (not verbose) $ gets (show . reportStateSummary) >>= report_

  liftIO setup
  runExampleGroup preserveIt verbose repl examples

  forM_ properties $ \(loc, expression) -> do
    r <- do
      liftIO setup
      reportStart loc expression verbose QuickCheckProperty
      runProperty repl expression
    case r of
      Success -> do
        reportSuccess verbose
        reportSeparator verbose
      Error err -> do
        reportError loc expression err verbose
        reportSeparator verbose
      Failure msg -> do
        reportFailure loc expression verbose
        report msg
        reportSeparator verbose
  where
    properties = [(loc, p) | Located loc (Property p) <- tests]

    examples :: [Located Interaction]
    examples = [Located loc (e, r) | Located loc (Example e r) <- tests]

-- |
-- Execute all expressions from given example in given 'Interpreter' and verify
-- the output.
runExampleGroup :: Bool -> Bool -> Interpreter -> [Located Interaction] -> Report ()
runExampleGroup preserveIt verbose repl = go
  where
    go ((Located loc (expression, expected)) : xs) = do
      reportStart loc expression verbose Specification
      r <- fmap lines <$> liftIO (safeEvalWith preserveIt repl expression)
      case r of
        Left err -> do
          reportError loc expression err verbose
          reportSeparator verbose
        Right actual -> case mkResult expected actual of
          NotEqual err -> do
            reportFailure loc expression verbose
            mapM_ report err
            reportSeparator verbose
          Equal -> do
            reportSuccess verbose
            reportSeparator verbose
            go xs
    go [] = return ()

safeEvalWith :: Bool -> Interpreter -> String -> IO (Either String String)
safeEvalWith preserveIt
  | preserveIt = Interpreter.safeEvalIt
  | otherwise  = Interpreter.safeEval
