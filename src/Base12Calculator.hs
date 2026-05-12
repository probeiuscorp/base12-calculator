-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Base12Calculator (topEntity) where

import Clash.Prelude
import Calculator.Prelude
import OLED (oled)
import Keypad (keypad)
import Clash.Sized.Vector.ToTuple (VecToTuple(vecToTuple))
import Arithmetic (arithmetic, ArithmeticAction(..))

topEntity
  :: Clock DomMain
  -> Reset DomMain
  -> Enable DomMain
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain (BitVector 4)
  -> (Signal DomMain (Unsigned 7)
    , Signal DomMain (Unsigned 16)
    , Signal DomMain (Unsigned 4)
    , Signal DomMain (BitVector 4)
    )
topEntity = exposeClockResetEnable accum

-- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN topEntity
  (Synthesize
    { t_name = "top"
    , t_inputs =
      [ PortName "clk"
      , PortName "rst"
      , PortName "en"
      , PortName "sw"
      , PortName "btnL"
      , PortName "btnC"
      , PortName "btnR"
      , PortName "btnU"
      , PortName "btnD"
      , PortName "row"
      ]
    , t_output = PortProduct ""
      [ PortName "oled"
      , PortName "led"
      , PortName "an"
      , PortName "col"
      ]
    }) #-}

-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}

data WorkingValueAction
  = WIPDigit (Unsigned 4) | WIPPop CalcValue
  | WIPReset | WIPPush | WIPNegate | WIPReciprocal
  deriving (Eq, Ord, Show, Generic, NFDataX)
accum sw btnL btnC btnR btnU btnD bRow = (bOledData, led, pure 0, bCol)
  where
    btnPush = boolToBit <$> bPush
    btnPop = bitToBool <$> debounce btnD
    (bCol, bKeypad) = unbundle $ keypad bRow
    bDigits :: Signal DomMain (BitVector 12)
    (bDigits, bButtonsRow) = unbundle $ split . withVector reverse <$> bKeypad
    bDigit :: Signal DomMain (Maybe (Unsigned 4))
    bDigit = ifoldl (\ma i isUp -> if isUp
      then ma <|> Just (bitCoerce i)
      else ma) Nothing . unpack <$> bDigits

    bmArithmeticAction = (mWhen ArithSum . bitToBool <$> btnL)
      <||> (mWhen ArithSub . bitToBool <$> btnR)
      <||> (mWhen ArithMul . bitToBool <$> btnU)
      <||> (mWhen ArithDiv . bitToBool <$> btnC)
    bmrStackYielded = bStackResult ## \case
      StackYield value -> Just $ Right value
      StackOverUnderflow _ -> Just $ Left ()
      StackIdle -> Nothing
    (bmError, bmArithmeticPullStackAction, bmArithmeticStart) = inlineMealyB Nothing (bmArithmeticAction, bStackSize, bmrStackYielded) $
      \s (mArithmeticAction, stackSize, mrStackYielded) -> let
        idle = (s, (Nothing, Nothing, Nothing))
        in case s of
          Nothing -> case mArithmeticAction of
            Nothing -> idle
            Just act | stackSize >= 2 -> (Just (act, Nothing), (Nothing, Just StackPop, Nothing))
            _ -> (Nothing, (Just (), Nothing, Nothing))
          Just (act, Nothing) -> case mrStackYielded of
            Nothing -> idle
            Just r1 -> (Just (act, Just r1), (Nothing, Just StackPop, Nothing))
          Just (act, Just r1) -> case mrStackYielded of
            Nothing -> idle
            Just r2 -> (Nothing,) $ case (r1, r2) of
              (Right v1, Right v2) -> (Nothing, Nothing, Just (act, v1, v2))
              _ -> (Just (), Nothing, Nothing)
    bmArithmeticComputed = arithmetic bmArithmeticStart
    bmArithmeticStackAction = bmArithmeticPullStackAction <||> StackPush $$$: bmArithmeticComputed

    bBackspace, bPush, bNegate, bReciprocal :: Signal DomMain Bool
    (bBackspace, bPush, bNegate, bReciprocal) = vecToTuple $ traverse bitCoerce bButtonsRow
    bWIPAction = WIPDigit $$$: bDigit
      <||> ((,) <$> btnPop <*> bmTopOfStack ## \(isPop, mValue) -> if isPop
        then Just $ WIPPop $ mValue ?? defaultWorkingValue
        else Nothing)
      <||> (mWhen WIPPush <$> bPush)
      <||> (mWhen WIPNegate <$> bNegate)
      <||> (mWhen WIPReciprocal <$> bReciprocal)
      <||> (mWhen WIPReset <$> bBackspace)
    defaultWorkingValue = CalcValue False 0 1
    bmWIPValue = inlineMoore Nothing bWIPAction $ withNoop $ \mValue -> let value = mValue ?? defaultWorkingValue in \case
      WIPPush -> Nothing
      WIPPop fromStack -> Just fromStack
      WIPNegate -> Just $ value { valIsNegative = not $ valIsNegative value }
      WIPReciprocal -> Just $ value
        { valNumerator = valDenominator value
        , valDenominator = valNumerator value
        }
      WIPReset -> Just $ value { valNumerator = 0 }
      WIPDigit digit -> Just $ value { valNumerator = mul12 (valNumerator value) + zeroExtend digit }
        where
          mul12 n = n .<<. 3 + n .<<. 2

    ledLookup :: Vec 16 (Unsigned 16)
    ledLookup = iterateI ((.|. 1) . (.<<. 1)) 0
    led = (ledLookup !!) . (bitCoerce :: Unsigned 4 -> Index 16) <$> bStackSize
    bStackControlsAction = (,,) <$> btnPush <*> btnPop <*> bmWIPValue ## \case
      (1, False, Just value) -> Just $ StackPush value
      (0, True, _) -> Just StackPop
      _ -> Nothing
    (bmTopOfStack, bStackSize, bStackResult) = unbundle $ stack $ delay Nothing bmArithmeticStackAction <||> bStackControlsAction

    bOledData = oled $ bmWIPValue <||> bmTopOfStack

data StackAction = StackPush CalcValue | StackPop
  deriving (Eq, Ord, Show, Generic, NFDataX)
data StackResult = StackIdle | StackYield CalcValue | StackOverUnderflow Bool
  deriving (Eq, Ord, Show, Generic, NFDataX)
data StackState = StackState
  { stackTop :: Unsigned 4
  , stackItems :: Vec 16 CalcValue
  } deriving (Eq, Ord, Show, Generic, NFDataX)
stackMachine :: StackState -> Maybe StackAction -> (StackState, (Maybe CalcValue, Unsigned 4, StackResult))
stackMachine state@(StackState top items) = let
    ok (transferred@(StackState top' items'), result) = (transferred,
      ( head items' `mWhen` (top' > 0)
      , top'
      , result
      ))
  in ok . \case
    Just (StackPush value) -> if top == 15
      then (state, StackOverUnderflow True)
      else (StackState (top + 1) (replace top value items), StackIdle)
    Just StackPop -> if top == 0
      then (state, StackOverUnderflow False)
      else let i = top - 1 in (StackState i items, StackYield $ items !! i)
    Nothing -> (state, StackIdle)
stack = mealy stackMachine $ StackState 0 $ repeat calcValueZero
