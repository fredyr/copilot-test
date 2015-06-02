{-# LANGUAGE RebindableSyntax #-}
module Temp where

import Language.Copilot
import qualified Copilot.Compile.C99 as C
import qualified Utils as U

chipTempInternal :: Stream Int32
chipTempInternal = externArray "temp_sensor_data" (constI32 0) 8
                   (Just $ repeat [0, 2, 4, 6,
                                   7, 5, 3, 1])

chipTempExternal :: Stream Int32
chipTempExternal = externArray "temp_sensor_data" (constI32 1) 8
                   (Just $ repeat [0, 2, 4, 6,
                                   7, 5, 3, 1])

maxChipTemp :: Stream Int32
maxChipTemp = U.max chipTempExternal chipTempInternal

s :: Stream Int32
s = extern "s" (Just [10,20..])

t :: Stream Int32
t = extern "t" (Just [73,70..])

testSpec :: Spec
testSpec = do
  observer "chipTempInternal" chipTempInternal
  observer "chipTempExternal" chipTempExternal
  observer "maxChipTemp" maxChipTemp
  observer "s" s
  observer "s_hyst" shyst
  observer "s_imp" simp
  observer "t" t
  observer "t_hyst" thyst
  observer "t_imp" timp
  trigger "s_muteAllChannels" simp [arg shyst]
  trigger "t_muteAllChannels" timp [arg thyst]
  where
    shyst = U.hysteresis 45 60 s
    thyst = U.hysteresis 45 60 t
    simp  = U.impulse shyst
    timp  = U.impulse thyst

tempSpec :: Spec
tempSpec = do
  trigger "muteAllChannels" tempImpulse [arg hyst]
  where hyst = U.hysteresis 40 45 maxChipTemp
        tempImpulse = U.impulse hyst

runTempSpec = do
  interpret 20 tempSpec

compileTempSpec = do
  reify tempSpec >>= C.compile C.Params {C.prefix=Just "temp_sensor", C.verbose=True}
