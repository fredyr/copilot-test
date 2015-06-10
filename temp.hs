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


ntcPSU_lut :: Stream Word32 -> Stream Float
ntcPSU_lut idx = externArray "NTC_PSU_lut" idx 17 ntcPSU_lut_test
ntcPSU_lut_test = (Just $ repeat [185.00, 141.86, 115.66, 95.57, 83.00, 73.66, 65.99, 59.32, 53.28, 47.60, 42.13, 36.73, 31.15, 25.07, 18.76, 10.82, -4.42])

ntcAMP_lut :: Stream Word32 -> Stream Float
ntcAMP_lut idx = externArray "NTC_PSU_lut" idx 17 ntcAMP_lut_test
ntcAMP_lut_test = (Just $ repeat [178.00, 149.00, 128.57, 112.68, 100.97, 91.56, 83.47, 76.19, 69.37, 62.74, 56.08, 49.16, 41.65, 32.97, 21.92, 4.20, -85.40])

ntcAMBIENT_lut :: Stream Word32 -> Stream Float
ntcAMBIENT_lut idx = externArray "NTC_PSU_lut" idx 17 ntcAMBIENT_lut_test
ntcAMBIENT_lut_test = (Just $ repeat [147.68, 97.16, 73.74, 60.56, 51.19, 43.77, 37.48, 31.90, 26.77, 21.91, 17.19, 12.48, 7.64, 2.51, -3.17, -9.84, -18.54])


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

adcRawTempSplit :: Stream Word32 -> (Stream Word32, Stream Float)
adcRawTempSplit raw = (idx, offset)
  where
    -- Input from the ADC is 12 bits.
    -- The top 4 bits are used as an index in a lockup table.
    -- The last 8 bits are converted to float and used for interpolation.
    rawC = U.clamp 0 4095 raw
    idx = rawC .>>. constW32 8
    offset' = unsafeCast $ rawC .&. 0xff
    offset = offset' / 256.0

adcRawTransform :: (Stream Word32 -> Stream Float) -> (Stream Word32, Stream Float) -> Stream Float
adcRawTransform lut (idx, offset) = y
  where
    y0 = lut idx
    y1 = lut (idx+1)
    y = y0 + ((y1 - y0) * offset)

adcTemps = foldr1 (||) chans
  where
    chans = map f [2, 3, 4, 5]
    f = ((U.hysteresis 700 1000) . sensorDataArray . constW32)

s :: Stream Word32
s = extern "s" (Just [15, 30..])

t :: Stream Word32
t = extern "t" (Just [73, 70..])

u :: Stream Word32
u = extern "u" (Just [0, 123..])

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
  observer "u" u


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
    

testSpec_ADCTemp :: Spec
testSpec_ADCTemp = do
  observer "0__fast" fast
  observer "1_TempSplit_fst" $ fst aRSplit_fast
  observer "2_TempSplit_snd" $ snd aRSplit_fast
  observer "3_Transform" $ adcRawTransform ntcPSU_lut aRSplit_fast

  observer "4_slow" slow
  observer "5_TempSplit_fst" $ fst aRSplit_slow
  observer "6_TempSplit_snd" $ snd aRSplit_slow
  observer "7_Transform" $ adcRawTransform ntcPSU_lut aRSplit_slow
  where
    fast = [0] ++ fast + 123
    slow = [0] ++ slow + 7
    aRSplit_fast = adcRawTempSplit fast
    aRSplit_slow = adcRawTempSplit slow

testSpec_Fan :: Spec
testSpec_Fan = do
  observer "f" f
  observer "fan" $ fanControl f true

tempSpec :: Spec
tempSpec = do
  trigger "muteAllChannels" imp [arg hyst]
  where chip = U.hysteresis 40 45 maxChipTemp
        adc  = U.hysteresis 700 1000 adcTempChanB
        hyst = chip || adc
        imp  = U.impulse hyst

runTestSpec = do
  interpret 20 testSpec

runTestSpec_Fan = do
  interpret 20 testSpec_Fan

runTestSpec_ADCTemp = do
  interpret 40 testSpec_ADCTemp

runTempSpec = do
  interpret 20 tempSpec

compileTempSpec = do
  reify tempSpec >>= C.compile C.Params {C.prefix=Just "temp_sensor", C.verbose=True}
