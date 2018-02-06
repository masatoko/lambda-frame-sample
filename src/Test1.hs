module Script.Test.Test3 where

import           Control.Monad       (forM_, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.IntMap         as IM
import           Linear.V2

import           Script

script :: Script ()
script = do
  t <- getTime
  -- when (t == 0) $ do
  --   putStr_ "[RB]:     Shoot"
  --   putStr_ "[X][Y]:   Transform"
  --   putStr_ "[LT]:     Left Jet"
  --   putStr_ "[RT]:     Right Jet"
  --   putStr_ "(L)Stick: Move"

  jes <- getJoystickEvents
  -- putStr_ $ show jes
  axes <- getJoyAxes
  -- putStr_ $ show axes

  -- ** Gun
  when (JoystickEvent 5 Pressed `elem` jes) $ do
    whenJustM (lookupPart "gun-l1") shootGun
    whenJustM (lookupPart "gun-l2") shootGun
    whenJustM (lookupPart "gun-r1") shootGun
    whenJustM (lookupPart "gun-r2") shootGun
  --
  -- ** Wheel
  whenJust (IM.lookup 0 axes) $ \d ->
    when (abs d > 0.2) $ setMotor $ -4 * d

  when (JoystickEvent 2 Pressed `elem` jes) $ do
    setAngleFor "jnt-l" 0
    setAngleFor "jnt-r" 0
  when (JoystickEvent 3 Pressed `elem` jes) $ do
    setAngleFor "jnt-l" $ pi/2
    setAngleFor "jnt-r" $ -pi/2

  whenJust (IM.lookup 2 axes) $ \rate ->
    whenJustM (lookupPart "jet-l") $ \jet -> do
      let rate' = (rate + 1) / 2
      if rate' < 0.2
        then setJetOperation jet Nothing
        else setJetOperation jet $ Just $ JetOpPower $ 10 * rate'
  whenJust (IM.lookup 5 axes) $ \rate ->
    whenJustM (lookupPart "jet-r") $ \jet -> do
      let rate' = (rate + 1) / 2
      if rate' < 0.2
        then setJetOperation jet Nothing
        else setJetOperation jet $ Just $ JetOpPower $ 10 * rate'

  return ()
  where
    setAngleFor name ang = whenJustM (lookupPart name) $ \joint -> setAngle joint ang
  --
  --   getJoyHats (JoyHatEvent 0 hs) = hs
  --   getJoyHats _                  = []
  --
    setMotor rate =
      mapM_ setMotorRate' names
      where
        names = ["wheel-" ++ lr:[n] | lr <- "lr", n <- "1,2,3"]
        setMotorRate' name =
          whenJustM (lookupPart name) $ \wheel ->
            setMotorRate wheel rate
