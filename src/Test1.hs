module Test1 where

import           Control.Monad       (forM_, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.IntMap         as IM
import           Linear.V2

import           Script              (ButtonState (..), JetOperation (..),
                                      JoystickEvent (..), Script)
import qualified Script              as S

script :: Script ()
script = do
  t <- S.getTime
  when (t == 0) $ do
    S.putStr_ "[RB]:      撃つ"
    S.putStr_ "[X][Y]:    変形"
    S.putStr_ "[LT]:      左ジェット"
    S.putStr_ "[RT]:      右ジェット"
    S.putStr_ "(左)ｽﾃｨｯｸ: 移動"

  jes <- S.getJoystickEvents
  -- putStr_ $ show jes
  axes <- S.getJoyAxes
  -- putStr_ $ show axes

  -- ** Gun
  when (S.JoystickEvent 5 S.Pressed `elem` jes) $ do
    S.shootGun "gun-l1"
    S.shootGun "gun-l2"
    S.shootGun "gun-r1"
    S.shootGun "gun-r2"
  --
  -- ** Wheel
  whenJust (IM.lookup 0 axes) $ \d ->
    when (abs d > 0.2) $ do
      let work name = S.addRotImpulse name (-4 * d)
      work "wheel-l1"
      work "wheel-l2"
      work "wheel-l3"
      work "wheel-r1"
      work "wheel-r2"
      work "wheel-r3"

  when (JoystickEvent 2 Pressed `elem` jes) $ do
    S.setAngle "jnt-l" 0
    S.setAngle "jnt-r" 0
  when (JoystickEvent 3 Pressed `elem` jes) $ do
    S.setAngle "jnt-l" $ pi/2
    S.setAngle "jnt-r" $ -pi/2

  whenJust (IM.lookup 2 axes) $ \rate -> do
    let name = "jet-l"
        rate' = (rate + 1) / 2
    if rate' < 0.2
      then S.setJetOperation name Nothing
      else S.setJetOperation name $ Just $ JetOpPower $ 10 * rate'
  whenJust (IM.lookup 5 axes) $ \rate -> do
    let name = "jet-r"
        rate' = (rate + 1) / 2
    if rate' < 0.2
      then S.setJetOperation name Nothing
      else S.setJetOperation name $ Just $ JetOpPower $ 10 * rate'
