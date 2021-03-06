{-# LANGUAGE RebindableSyntax #-}

module Temp where

import Language.Copilot
import qualified Copilot.Compile.C99 as C
import qualified Utils as U
import qualified Prelude as P

chipDataTest = Just chipData
  where
    chipData = map addN [0, 2..]
    addN n = map (+ n) [22..23]

adcDataTest = Just adcData
  where
    adcData = map addN [0, 43..]
    addN n = map (+ n) [100, 200..800]

lutData :: Stream Int32
lutData = [22] ++ lutData + 3

chip_DataArray :: Stream Word32 -> Stream Int32
chip_DataArray idx = externArray "temp_chip_data" idx 2 chipDataTest

adc_DataArray :: Stream Word32 -> Stream Word32
adc_DataArray idx = externArray "temp_adc_data" idx 8 adcDataTest

ntcPSU_extern_lut :: Stream Word32 -> Stream Int32
ntcPSU_extern_lut raw = externFun "rawADC_transform_PSU" [ arg raw ] (Just $ unsafeCast (raw) + lutData)

ntcAMP_extern_lut :: Stream Word32 -> Stream Int32
ntcAMP_extern_lut raw = externFun "rawADC_transform_AMP" [ arg raw ] (Just $ unsafeCast (raw `div` 40) + lutData)

ntcAMBIENT_extern_lut :: Stream Word32 -> Stream Int32
ntcAMBIENT_extern_lut raw = externFun "rawADC_transform_AMBIENT" [ arg raw ] (Just $ unsafeCast raw + lutData)

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
    ch = map f [0 .. 3]
    f = (ntcAMP_extern_lut . U.delay . adc_DataArray . constW32)

adcTempPSU :: [Stream Int32]
adcTempPSU = ch
  where
    ch = map f [4, 5]
    f = (ntcAMP_extern_lut . U.delay . adc_DataArray . constW32)

adcTempAMBIENT :: [Stream Int32]
adcTempAMBIENT = [(ntcAMP_extern_lut . U.delay . adc_DataArray $ constW32 6)]

allTemps = adcTempAMP P.++ adcTempPSU P.++ adcTempAMBIENT
           P.++ [chipTempExternal, chipTempInternal]

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

fan = fanControl (unsafeCast $ U.maxList allTemps) true

--------------------------------------------------------------------------------

testSpec :: Spec
testSpec = do
  observer "chipTempInternal" chipTempInternal
  observer "chipTempExternal" chipTempExternal
  observer "maxChipTemp" maxChipTemp
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

testSpec2 :: Spec
testSpec2 = do
  observer "chip_DataArray" chipTempExternal
  observer "AMPtempA" (adcTempAMP !! 0)
  observer "AMPtempB" (adcTempAMP !! 1)
  observer "AMPtempC" (adcTempAMP !! 2)
  observer "AMPtempD" (adcTempAMP !! 3)
  observer "adcHystAMP" adcHystAMP
  trigger "muteAllChannels" imp [arg adcHystAMP]
  where
    imp  = U.impulse adcHystAMP

testSpec_Fan :: Spec
testSpec_Fan = do
  observer "f" f
  observer "fan" $ fanControl f true

tempSpec :: Spec
tempSpec = do
  trigger "fanControl_setDuty" true [arg fan]
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

f1 :: Stream Int32
f1 = [45] ++ f1 - 5
f2 :: Stream Int32
f2 = [20] ++ f2 - 1
f3 :: Stream Int32
f3 = [3] ++ f3 + 5
f4 :: Stream Int32
f4 = [13] ++ f4 + 2

fList = [f1, f2, f3, f4]

testSpec_lists :: Spec
testSpec_lists = do
  observer "max1" $ foldl1 U.max fList
  observer "max2" $ U.maxList fList
  observer "f1" f1
  observer "f2" f2
  observer "f3" f3
  observer "f4" f4

compileTestSpecList = do
  reify testSpec_lists >>= C.compile C.Params {C.prefix=Just "testSpec_lists", C.verbose=True}

runTestSpec = do
  interpret 20 testSpec

runTestSpec2 = do
  interpret 20 testSpec2

runTestSpec_Fan = do
  interpret 20 testSpec_Fan

runTempSpec = do
  interpret 20 tempSpec

compileTempSpec = do
  reify tempSpec >>= C.compile C.Params {C.prefix=Just "temp_sensor", C.verbose=True}
