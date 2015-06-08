{-# LANGUAGE RebindableSyntax #-}

module Temp where

import Language.Copilot
import qualified Copilot.Compile.C99 as C
import qualified Utils as U
import qualified Prelude as P

sensorDataTest = Just sensorData
  where
    sensorData = map addN [0, 43..]
    addN n = map (+ n) [600..607]

sensorDataArray :: Stream Word32 -> Stream Word32
sensorDataArray idx = externArray "temp_sensor_data" idx 8 sensorDataTest

ntcPSU_lut :: Stream Word32 -> Stream Int32
ntcPSU_lut idx = externArray "NTC_PSU_lut" idx 17 ntcPSU_lut_test

ntcPSU_lut_test = (Just $ repeat [162070, 136700, 115598, 98282, 84266, 73065, 64197, 57175, 51517, 46737, 42351, 37875, 32825, 26716, 19063, 9382, -2809])

-- CHIP TEMP SENSORS
chipTempInternal :: Stream Word32
chipTempInternal = sensorDataArray (constW32 0)

chipTempExternal :: Stream Word32
chipTempExternal = sensorDataArray (constW32 1)

maxChipTemp :: Stream Word32
maxChipTemp = U.max chipTempExternal chipTempInternal

--------------------------------------------------------------------------------
-- | ADC TEMP SENSORS

adcTempChanB :: Stream Word32
adcTempChanB = sensorDataArray (constW32 3)

adcRawTempChanB :: Stream Word32
adcRawTempChanB = sensorDataArray (constW32 3)

adcRawTempSplit :: Stream Word32 -> (Stream Word32, Stream Word32)
adcRawTempSplit raw = (idx, offset)
  where
    idx = raw .>>. (constW32 8)
    offset = raw .&. 0xff

adcRawTransfer :: (Stream Word32, Stream Word32) -> Stream Int32
adcRawTransfer (idx, offset) = cast y
  where
    y1 = ntcPSU_lut idx
    y0 = ntcPSU_lut (idx+1)
    --offset' = cast offset
    offset' = 3
    y = y0 + ((y1 - y0) * offset') `div` 256

adcTemps = foldr1 (||) chans
  where
    chans = map f [2, 3, 4, 5]
    f = ((U.hysteresis 700 1000) . sensorDataArray . constW32)

s :: Stream Word32
s = extern "s" (Just [10, 20..])

t :: Stream Word32
t = extern "t" (Just [73, 70..])

u :: Stream Word32
u = extern "t" (Just [300, 350..])

f :: Stream Float
f = extern "f" (Just [11.0, 15.7 ..])
--------------------------------------------------------------------------------
-- | FAN CONTROL

fanControl :: Stream Float -> Stream Bool -> Stream Float
fanControl temp powerOn = if powerOn then y else 0.0
  where
    y = U.clamp idleDuty maxDuty $ k * temp + m
    m = idleDuty - influenceTemp * k
    k = (maxDuty - idleDuty) / (maxTemp - influenceTemp)
    idleDuty      = constF 49.0
    maxDuty       = constF 100.0
    influenceTemp = constF 50.0 -- Point where fan increases from idle
    maxTemp       = constF 76.0 -- Point where fan is at max


testSpec :: Spec
testSpec = do
  -- observer "chipTempInternal" chipTempInternal
  -- observer "chipTempExternal" chipTempExternal
  -- observer "adcTempChanB" adcTempChanB
  -- observer "maxChipTemp" maxChipTemp
  -- observer "s" s
  --observer "s_hyst" shyst
  --observer "s_imp" simp
  --observer "t" t
  --observer "t_hyst" thyst
  --observer "t_imp" timp

  --observer "u" u
  --observer "u_hyst" uhyst
  --observer "m_hyst" mhyst
  --observer "adcTemps" adcTemps
  -- trigger "s_muteAllChannels" simp [arg shyst]
  observer "s" s
  observer "adcRawTransfer" (adcRawTransfer aRSplit)
  observer "0_adcRawTempSplit_fst" $ fst aRSplit
  observer "0_adcRawTempSplit_snd" $ snd aRSplit

  observer "f" f
  observer "fan" $ fanControl f true
  trigger "m_muteAllChannels" mimp [arg mhyst]
  where
    shyst = U.hysteresis 45 60 s
    thyst = U.hysteresis 45 60 t
    uhyst = U.hysteresis 700 1000 u
    mhyst = thyst || uhyst
    simp  = U.impulse shyst
    timp  = U.impulse thyst
    mimp  = U.impulse mhyst
    aRSplit = adcRawTempSplit s
    del = div u 123

tempSpec :: Spec
tempSpec = do
  trigger "muteAllChannels" imp [arg hyst]
  where chip = U.hysteresis 40 45 maxChipTemp
        adc  = U.hysteresis 700 1000 adcTempChanB
        hyst = chip || adc
        imp  = U.impulse hyst

runTestSpec = do
  interpret 20 testSpec

runTempSpec = do
  interpret 20 tempSpec

compileTempSpec = do
  reify tempSpec >>= C.compile C.Params {C.prefix=Just "temp_sensor", C.verbose=True}
