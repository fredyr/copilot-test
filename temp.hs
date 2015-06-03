{-# LANGUAGE RebindableSyntax #-}
module Temp where

import Language.Copilot
import qualified Copilot.Compile.C99 as C
import qualified Utils as U

sensorDataTest = Just sensorData
  where
    sensorData = map addN [0, 43..]
    addN n = map (+ n) [600..607]

sensorDataArray :: Stream Int32 -> Stream Int32
sensorDataArray idx = externArray "temp_sensor_data" idx 8 sensorDataTest

-- CHIP TEMP SENSORS
chipTempInternal :: Stream Int32
chipTempInternal = sensorDataArray (constI32 0)

chipTempExternal :: Stream Int32
chipTempExternal = sensorDataArray (constI32 1)

maxChipTemp :: Stream Int32
maxChipTemp = U.max chipTempExternal chipTempInternal

-- ADC TEMP SENSORS
adcTempChanB :: Stream Int32
adcTempChanB = sensorDataArray (constI32 3)

adcTemps = foldr1 (||) chans
  where
    chans = map f [2, 3, 4, 5]
    f = ((U.hysteresis 700 1000) . sensorDataArray . constI32)

s :: Stream Int32
s = extern "s" (Just [10, 20..])

t :: Stream Int32
t = extern "t" (Just [73, 70..])

u :: Stream Int32
u = extern "t" (Just [300, 350..])

testSpec :: Spec
testSpec = do
  -- observer "chipTempInternal" chipTempInternal
  -- observer "chipTempExternal" chipTempExternal
  -- observer "adcTempChanB" adcTempChanB
  -- observer "maxChipTemp" maxChipTemp
  observer "s" s
  observer "s_hyst" shyst
  observer "s_imp" simp
  observer "t" t
  observer "t_hyst" thyst
  observer "t_imp" timp

  observer "u" u
  observer "u_hyst" uhyst
  observer "m_hyst" mhyst
  observer "adcTemps" adcTemps
  -- trigger "s_muteAllChannels" simp [arg shyst]
  trigger "m_muteAllChannels" mimp [arg mhyst]
  where
    shyst = U.hysteresis 45 60 s
    thyst = U.hysteresis 45 60 t
    uhyst = U.hysteresis 700 1000 u
    mhyst = thyst || uhyst
    simp  = U.impulse shyst
    timp  = U.impulse thyst
    mimp  = U.impulse mhyst

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
