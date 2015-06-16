{-# LANGUAGE RebindableSyntax #-}

module Temp where

import Language.Copilot
import qualified Copilot.Compile.C99 as C
import qualified Utils as U
import qualified Prelude as P

chipDataTest = Just sensorData
  where
    sensorData = map addN [0, 43..]
    addN n = map (+ n) [600..607]

adcDataTest = Just sensorData
  where
    sensorData = map addN [0, 43..]
    addN n = map (+ n) [600..607]

chip_DataArray :: Stream Word32 -> Stream Int32
chip_DataArray idx = externArray "temp_chip_data" idx 2 adcDataTest

adc_DataArray :: Stream Word32 -> Stream Word32
adc_DataArray idx = externArray "temp_adc_data" idx 8 chipDataTest

ntcPSU_extern_lut :: Stream Word32 -> Stream Int32
ntcPSU_extern_lut inx = externFun "rawADC_transform_PSU" [ arg inx ] Nothing

ntcAMP_extern_lut :: Stream Word32 -> Stream Int32
ntcAMP_extern_lut inx = externFun "rawADC_transform_AMP" [ arg inx ] Nothing

ntcAMBIENT_extern_lut :: Stream Word32 -> Stream Int32
ntcAMBIENT_extern_lut inx = externFun "rawADC_transform_AMBIENT" [ arg inx ] Nothing

--------------------------------------------------------------------------------

-- CHIP TEMP SENSORS
chipTempInternal :: Stream Int32
chipTempInternal = chip_DataArray (constW32 0)

chipTempExternal :: Stream Int32
chipTempExternal = chip_DataArray (constW32 1)

maxChipTemp :: Stream Int32
maxChipTemp = foldr1 U.max [chipTempExternal, chipTempInternal] 

-- ADC TEMP SENSORS
adcTempAMP :: [Stream Int32]
adcTempAMP = ch
  where
    ch = map f [2, 3, 4, 5]
    f = (ntcAMP_extern_lut . U.delay . adc_DataArray . constW32)

adcTempPSU :: [Stream Int32]
adcTempPSU = ch
  where
    ch = map f [6, 7]
    f = (ntcAMP_extern_lut . U.delay . adc_DataArray . constW32)

adcTempAMBIENT :: [Stream Int32]
adcTempAMBIENT = [(ntcAMP_extern_lut . U.delay . adc_DataArray $ constW32 8)]



-- Temp protection with hysteresis

protectHyst :: Stream Int32 -> Stream Int32 -> [Stream Int32] -> Stream Bool
protectHyst lowH highH xs = foldr1 (||) ch
  where
    ch = map (U.hysteresis lowH highH) xs

adcHystAMP = protectHyst 55 60 adcTempAMP
adcHystPSU = protectHyst 66 69 adcTempPSU
adcHystAMBIENT = protectHyst 75 80 adcTempAMBIENT
chipHyst = protectHyst 40 45 [chipTempExternal, chipTempInternal]

allHyst = adcHystAMP || adcHystPSU || adcHystAMBIENT || chipHyst


----------------------------------------------

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
  observer "chipTempInternal" chipTempInternal
  observer "chipTempExternal" chipTempExternal
  --observer "adcTempChanB" adcTempChanB
  observer "maxChipTemp" maxChipTemp
  -- observer "s" s
  --observer "s_hyst" shyst
  --observer "s_imp" simp
  --observer "t" t
  --observer "t_hyst" thyst
  --observer "t_imp" timp

  --observer "u" u
  --observer "u_hyst" uhyst
  --observer "m_hyst" mhyst
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
    

testSpec_Fan :: Spec
testSpec_Fan = do
  observer "f" f
  observer "fan" $ fanControl f true

tempSpec :: Spec
tempSpec = do
  trigger "muteAllChannels" imp [arg allHyst]
  observer "AMPtempA" (adcTempAMP !! 0)
  observer "AMPtempB" (adcTempAMP !! 1)
  observer "AMPtempC" (adcTempAMP !! 2)
  observer "AMPtempD" (adcTempAMP !! 3)
  observer "PSUtempA" (adcTempPSU !! 0)
  observer "PSUtempB" (adcTempPSU !! 1)
  observer "AMBIENTtempA" (adcTempAMBIENT !! 0)
  observer "CHIPtempExt" chipTempExternal
  observer "CHIPtempInt" chipTempInternal

  where 
        imp  = U.impulse allHyst

runTestSpec = do
  interpret 20 testSpec

runTestSpec_Fan = do
  interpret 20 testSpec_Fan

runTempSpec = do
  interpret 20 tempSpec

compileTempSpec = do
  reify tempSpec >>= C.compile C.Params {C.prefix=Just "temp_sensor", C.verbose=True}
