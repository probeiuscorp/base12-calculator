{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SnapshotTesting (snapshot, col) where

import Prelude
import Data.List (transpose, intercalate)
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
instance SnapshotPrintable String where
  snapshotPrint = id
instance C.BitPack a => SnapshotPrintable a where
  snapshotPrint = show . C.pack

type ColsData = [(String, Int -> [String])]
col :: C.KnownDomain dom => SnapshotPrintable a => String -> C.Signal dom a -> (String, Int -> [String])
col title bData = (title, flip C.sampleN $ snapshotPrint <$> bData)
takeSnapshot
  :: Int
  -> ColsData
  -> String
takeSnapshot nCycles givenColsData = unlines $ map (intercalate "   ") (headers : rows)
  where
    ceiledLog10 = (+1) . floor . logBase (10 :: Double) .  fromIntegral
    clkCycles = leftPad (max (length "clk") $ ceiledLog10 nCycles) . show <$> take nCycles ([0..] :: [Integer])
    colsData = ("clk", const clkCycles) : givenColsData
    colsString = ($ nCycles) . snd <$> colsData
    colsSize = foldr (max . length) 0 <$> colsString
    headers = zipWith rightPad colsSize $ fst <$> colsData
    rows = transpose colsString
    pad
      :: ((String -> String -> String)
       -> (String -> String -> String))
      -> Int -> String -> String
    pad f size str = let len = length str in f (<>) str $ replicate (size - len) ' '
    rightPad = pad id
    leftPad = pad flip

snapshot
  :: String  -- ^ path name
  -> String  -- ^ test name
  -> Int
  -> ColsData
  -> TestTree
snapshot pathName testName nCycles bData = goldenVsString name (".snapshots" </> name) $ pure . fromString $ takeSnapshot nCycles bData
  where
    name = pathName </> testName
