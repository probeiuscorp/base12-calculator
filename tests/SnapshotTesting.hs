{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SnapshotTesting (snapshot, snapshotPrint, fixClockResetEnable, col, raw, showMaybe, showMaybe', printMaybeCalcValue, fromEdges, ColData) where

import Prelude
import Calculator.Prelude
import Data.List (transpose, intercalate, dropWhileEnd)
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import qualified Clash.Prelude as C
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (pack, unpack)

fromString :: String -> LBS.ByteString
fromString = pack
showMaybe' :: forall a. C.BitPack a => (a -> String) -> Maybe a -> String
showMaybe' format = \case
  Just a -> format a
  Nothing -> '-' <$ show (C.pack placeholder)
    where
      placeholder :: a
      placeholder = C.bitCoerce (0 :: C.Unsigned (C.BitSize a))
showMaybe = showMaybe' (show . C.pack)

class SnapshotPrintable a where
  snapshotPrint :: a -> String

instance SnapshotPrintable LBS.ByteString where
  snapshotPrint = unpack
instance C.BitPack a => SnapshotPrintable a where
  snapshotPrint = show . C.pack

-- | I can't figure out why this is necessary. Implicit params are really annoying.
fixClockResetEnable :: C.KnownDomain dom => (C.HiddenClockResetEnable dom => r) -> r
fixClockResetEnable = withGenClockResetEnable

type ColData dom = (String, C.Signal dom String)
col
  :: (C.KnownDomain dom, SnapshotPrintable a)
  => String
  -> C.Signal dom a
  -> ColData dom
col title bData = (title, snapshotPrint <$> bData)
raw :: C.KnownDomain dom => String -> C.Signal dom String -> ColData dom
raw = (,)
takeSnapshot
  :: forall dom. C.KnownDomain dom
  => Int
  -> (C.HiddenClockResetEnable dom => [ColData dom])
  -> String
takeSnapshot nCycles givenColsData = unlines styledLines
  where
    colsData :: C.HiddenClockResetEnable dom => [ColData dom]
    colsData = ("clk", clkCountColumn) : givenColsData
    dataRows = C.sampleN nCycles $ traverse snd colsData
    -- TODO: this withGenClockResetEnable should not be necessary.
    -- The data (just the column titles) used here does not depend on clock, reset, or enable
    headers = fst <$> fixClockResetEnable colsData
    rows = headers : dataRows
    styledLines = dropWhileEnd (== ' ') . intercalate "   " . zipWith rightPad colsSize <$> rows
    colsSize = foldr (max . length) 0 <$> transpose rows

    ceiledLog10 = (+1) . floor . logBase (10 :: Double) .  fromIntegral
    clkCountColumn :: C.HiddenClockResetEnable dom => C.Signal dom String
    clkCountColumn = leftPad (max (length "clk") $ ceiledLog10 (nCycles - 1)) . show <$> bClkCount

    pad
      :: ((String -> String -> String)
       -> (String -> String -> String))
      -> Int -> String -> String
    pad f size str = let len = length str in f (<>) str $ replicate (size - len) ' '
    rightPad = pad id
    leftPad = pad flip

snapshot
  :: C.KnownDomain dom
  => String  -- ^ path name
  -> String  -- ^ test name
  -> Int
  -> (C.HiddenClockResetEnable dom => [ColData dom])
  -> TestTree
snapshot pathName testName nCycles bData = goldenVsString name (".snapshots" </> name) $ pure . fromString $ takeSnapshot nCycles bData
  where
    name = pathName </> testName

bClkCount :: C.Signal dom Int
bClkCount = C.fromList [0..]

type EdgesState a = ([(Int, a)], a)
fromEdges
  :: forall dom a. (C.HiddenClockResetEnable dom, C.NFDataX a)
  => a -> [(Int, a)] -> C.Signal dom a
fromEdges initial edges0 = C.mealy transfer (edges0, initial) bClkCount
  where
    transfer :: EdgesState a -> Int -> (EdgesState a, a)
    transfer steadyState@(edges, a) clk = case edges of
      ((n, a') : edges') | clk >= n -> ((edges', a'), a')
      _ -> (steadyState, a)
