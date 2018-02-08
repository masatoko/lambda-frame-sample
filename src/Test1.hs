module Test1 where

import           Control.Monad       (forM_, when)         -- https://hackage.haskell.org/package/base/docs/Control-Monad.html
import           Control.Monad.Extra (whenJust, whenJustM) -- https://hackage.haskell.org/package/extra/docs/Control-Monad-Extra.html
import qualified Data.IntMap         as IM                 -- 整数専用のマップ https://hackage.haskell.org/package/containers/docs/Data-IntMap.html

import           Script              (ButtonState (..), JetOperation (..),
                                      JoystickEvent (..), Script)
import qualified Script              as S -- ゲームのコマンド

import qualified Input.XBox360       as X -- XBox360コントローラのボタン・軸番号が定義されたモジュール

-- 毎時刻、このscript関数が呼ばれる。
-- その時刻に、君のロボットをどう動かすかを、この関数の中に書こう。
script :: Script ()
script = do
  -- '--'で始まる行はコメント扱いとなる。
  -- ここから命令が沢山書いてある、上から順に実行されるよ。
  t <- S.getTime -- 時刻を得る。時刻は整数値で、最初は0、次の時刻では1と、1ずつ進む。
  when (t == 0) $ do -- 最初の時刻の場合
    -- when (何か) で、その 何か が真(True)の場合、ここが実行される。
    -- putStr_ "メッセージ" は画面上にメッセージを表示。
    S.putStr_ "[RB]:      撃つ"
    S.putStr_ "[X][Y]:    変形"
    S.putStr_ "[LT]:      左ジェット"
    S.putStr_ "[RT]:      右ジェット"
    S.putStr_ "(左)ｽﾃｨｯｸ: 移動"

  jes <- S.getJoystickEvents -- ジョイスティックのイベントを得る。どのボタンが押されているかのイベント。
  -- putStr_ $ show jes
  axes <- S.getJoyAxes -- ジョイスティックの軸の状態（スティックの傾き・トリガーの押し具合）を得る。
  -- putStr_ $ show axes

  -- === Gun - 機銃
  when (S.JoystickEvent X.btnR S.Pressed `elem` jes) $ do -- ボタンRが押されているとき
    S.shootGun "gun-l1" -- gun-l1 の名前のGunから弾を撃つ。
    S.shootGun "gun-l2"
    S.shootGun "gun-r1"
    S.shootGun "gun-r2"
  --
  -- === Wheel - ホイール
  whenJust (IM.lookup X.axisLStickX axes) $ \d -> -- 左スティックのX軸の値（傾き）を探す。
    when (abs d > 0.2) $ do -- 絶対値が指定値より大きい場合（dは -1 以上 1 以下の値をとる）
      let work name = S.addRotImpulse name (-4 * d) -- name の名前のホイールを回転させる関数を定義する。正の値は時計回り。
      work "wheel-l1"
      work "wheel-l2"
      work "wheel-l3"
      work "wheel-r1"
      work "wheel-r2"
      work "wheel-r3"

  -- === Joint - ジョイント
  when (JoystickEvent X.btnX Pressed `elem` jes) $ do
    S.setAngle "jnt-l" 0 -- ジョイントの角度を０に指定
    S.setAngle "jnt-r" 0
  when (JoystickEvent X.btnY Pressed `elem` jes) $ do
    S.setAngle "jnt-l" $ pi/2 -- ジョイントの角度を pi / 2 に指定
    S.setAngle "jnt-r" $ -pi/2

  -- === Jet - ジェット
  whenJust (IM.lookup X.axisLTrigger axes) $ \rate -> do
    let name = "jet-l"
        rate' = (rate + 1) / 2
    if rate' < 0.2
      then S.setJetOperation name Nothing -- ジェットを切る
      else S.setJetOperation name $ Just $ JetOpPower $ 10 * rate' -- ジェットを設定
  whenJust (IM.lookup X.axisRTrigger axes) $ \rate -> do
    let name = "jet-r"
        rate' = (rate + 1) / 2
    if rate' < 0.2
      then S.setJetOperation name Nothing
      else S.setJetOperation name $ Just $ JetOpPower $ 10 * rate'
