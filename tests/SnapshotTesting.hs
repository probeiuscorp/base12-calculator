{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SnapshotTesting (snapshot, col, raw) where

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

class SnapshotPrintable a where
  snapshotPrint :: a -> String

instance SnapshotPrintable LBS.ByteString where
  snapshotPrint = unpack
instance C.BitPack a => SnapshotPrintable a where
  snapshotPrint = show . C.pack

type ColData dom = (String, C.Signal dom String)
col
  :: (C.KnownDomain dom, SnapshotPrintable a)
  => String
  -> (C.HiddenClockResetEnable dom => C.Signal dom a)
  -> (C.HiddenClockResetEnable dom => ColData dom)
col title bData = (title, snapshotPrint <$> bData)
raw :: C.KnownDomain dom => String
  -> (C.HiddenClockResetEnable dom => C.Signal dom String) -> (C.HiddenClockResetEnable dom => ColData dom)
raw = (,)
takeSnapshot
  :: forall dom. C.KnownDomain dom
  => Int
  -> (C.HiddenClockResetEnable dom => [ColData dom])
  -> String
takeSnapshot nCycles givenColsData = unlines styledLines
  where
    colsData :: C.HiddenClockResetEnable dom => [ColData dom]
    colsData = ("clk", bClkCount) : givenColsData
    dataRows = C.sampleN nCycles $ traverse snd colsData
    -- TODO: this withGenClockResetEnable should not be necessary.
    -- The data (just the column titles) used here does not depend on clock, reset, or enable
    headers = fst <$> withGenClockResetEnable colsData
    rows = headers : dataRows
    styledLines = dropWhileEnd (== ' ') . intercalate "   " . zipWith rightPad colsSize <$> rows
    colsSize = foldr (max . length) 0 <$> transpose rows

    nats = C.fromList ([0..] :: [Int])
    ceiledLog10 = (+1) . floor . logBase (10 :: Double) .  fromIntegral
    bClkCount :: C.HiddenClockResetEnable dom => C.Signal dom String
    bClkCount = leftPad (max (length "clk") $ ceiledLog10 (nCycles - 1)) . show <$> nats

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
