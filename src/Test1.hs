module Test1 where

import           Control.Monad       (forM_, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.IntMap         as IM -- 整数専用のマップ

import           Script              (ButtonState (..), JetOperation (..),
                                      JoystickEvent (..), Script)
import qualified Script              as S -- ゲームのコマンド

-- このscript関数がフレームごとに呼ばれる。
script :: Script ()
script = do
  -- '--'で始まる行はコメント扱い
  -- ここにある関数が、上から順に実行される。
  t <- S.getTime -- フレーム時刻を得る
  when (t == 0) $ do
    -- 最初の時刻の場合ここが実行される
    -- putStr_ "メッセージ" は画面上にメッセージを表示する
    S.putStr_ "[RB]:      撃つ"
    S.putStr_ "[X][Y]:    変形"
    S.putStr_ "[LT]:      左ジェット"
    S.putStr_ "[RT]:      右ジェット"
    S.putStr_ "(左)ｽﾃｨｯｸ: 移動"

  jes <- S.getJoystickEvents -- ジョイスティックのイベントを得る。どのボタンが押されているかのイベント。
  -- putStr_ $ show jes
  axes <- S.getJoyAxes -- ジョイスティックの軸の状態（スティックの傾き・トリガーの押し具合）を得る
  -- putStr_ $ show axes

  -- ** Gun
  when (S.JoystickEvent 5 S.Pressed `elem` jes) $ do -- ボタン５が押されているとき
    S.shootGun "gun-l1" -- gun-l1 の名前のGunから弾を撃つ
    S.shootGun "gun-l2"
    S.shootGun "gun-r1"
    S.shootGun "gun-r2"
  --
  -- ** Wheel
  whenJust (IM.lookup 0 axes) $ \d -> -- 軸０の値（スティックの傾き）を探す
    when (abs d > 0.2) $ do -- 絶対値が指定値より大きい場合は（dは -1 以上 1 以下の値をとる）
      let work name = S.addRotImpulse name (-4 * d) -- name の名前のホイールを回転させる関数を定義する。正の値は時計回り。
      work "wheel-l1"
      work "wheel-l2"
      work "wheel-l3"
      work "wheel-r1"
      work "wheel-r2"
      work "wheel-r3"

  when (JoystickEvent 2 Pressed `elem` jes) $ do
    S.setAngle "jnt-l" 0 -- ジョイントの角度を０に指定
    S.setAngle "jnt-r" 0
  when (JoystickEvent 3 Pressed `elem` jes) $ do
    S.setAngle "jnt-l" $ pi/2 -- ジョイントの角度を pi / 2 に指定
    S.setAngle "jnt-r" $ -pi/2

  whenJust (IM.lookup 2 axes) $ \rate -> do
    let name = "jet-l"
        rate' = (rate + 1) / 2
    if rate' < 0.2
      then S.setJetOperation name Nothing -- ジェットを切る
      else S.setJetOperation name $ Just $ JetOpPower $ 10 * rate' -- ジェットを設定
  whenJust (IM.lookup 5 axes) $ \rate -> do
    let name = "jet-r"
        rate' = (rate + 1) / 2
    if rate' < 0.2
      then S.setJetOperation name Nothing
      else S.setJetOperation name $ Just $ JetOpPower $ 10 * rate'
