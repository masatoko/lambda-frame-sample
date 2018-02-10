module Test1 where

import           Control.Monad       (forM_, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import qualified Data.IntMap         as IM

import           Script              (InputMotion (..), JetOperation (..),
                                      JoystickEvent (..), Script)
import qualified Script              as S

import qualified Input.Keycode       as K
import qualified Input.XBox360       as X

-- 毎時刻、このscript関数が呼ばれる。
-- その時刻に、君のロボットをどう動かすかを、この関数の中に書こう。
script :: Script ()
script = do
  -- '--'で始まる行はコメント扱いとなる。
  -- ここから命令が沢山書いてある、上から順に実行されるよ。
  t <- S.getTime -- 時刻を得る。時刻は整数値で、最初は0、次の時刻では1と、1ずつ進む。
  when (t == 0) $ do -- 最初の時刻の場合
    -- when (何か) で、`何か` が真(True)の場合、ここが実行される。
    -- S.putStr_ "メッセージ" は画面上にメッセージを表示。
    S.putStr_ "(RB)    / [J]   : 撃つ"
    S.putStr_ "(X)(Y)  / [W][S]: 変形"
    S.putStr_ "(LT)    / [Q]   : 左ジェット"
    S.putStr_ "(RT)    / [E]   : 右ジェット"
    S.putStr_ "左ｽﾃｨｯｸ / [A][D]: 移動"
    S.putStr_ "(*) 文章を表示すると遅くなるため、非表示推奨。" -- 動作が遅い場合は一連の'putStr_'をコメントアウトしてください

  kes <- S.getKeyboardEvents -- キーボードのイベントを得る。
  let isKeyPressed key = event `elem` kes
        where
          event = S.KeyboardEvent key S.Pressed
  -- when (not (null kes)) $ S.putStr_ $ show kes

  jes <- S.getJoystickEvents -- ジョイスティックのイベントを得る。どのボタンが押されているかのイベント。
  let isBtnPressed btn = event `elem` jes
        where
          event = S.JoystickEvent btn S.Pressed
  -- when (not (null jes)) $ putStr_ $ show jes

  axes <- S.getJoyAxes -- ジョイスティックの軸の状態（スティックの傾き・トリガーの押し具合）を得る。
  -- putStr_ $ show axes

  -- === Gun - 機銃
  when (isBtnPressed X.btnR || isKeyPressed K.Key'J) $ do -- どちらかが真ならば
    S.command $ S.ShootGun "gun-l1" -- gun-l1 の名前のGunから弾を撃つ。
    S.command $ S.ShootGun "gun-l2"
    S.command $ S.ShootGun "gun-r1"
    S.command $ S.ShootGun "gun-r2"
  --
  -- === Wheel - ホイール
  holdingA <- S.getKeyboardState K.Key'A
  holdingD <- S.getKeyboardState K.Key'D
  let movex
        | holdingA  = -1
        | holdingD  = 1
        | otherwise =
            case IM.lookup X.axisLStickX axes of -- 左スティックのX軸の値（傾き）を探す。
              Nothing    -> 0
              Just axisX -> if abs axisX > 0.2 -- 絶対値が指定値より大きい場合（axisX は -1 以上 1 以下の値をとる）
                              then axisX
                              else 0
  when (movex /= 0) $ do
    let work name = S.command $ S.AddRotImpulse name (-4 * movex) -- name の名前のホイールを回転させる関数を定義する。正の値は時計回り。
    work "wheel-l1"
    work "wheel-l2"
    work "wheel-l3"
    work "wheel-r1"
    work "wheel-r2"
    work "wheel-r3"

  -- === Joint - ジョイント
  when (isBtnPressed X.btnX || isKeyPressed K.Key'W) $ do
    S.command $ S.SetAngle "jnt-l" 0 -- ジョイントの角度を０に指定
    S.command $ S.SetAngle "jnt-r" 0
  when (isBtnPressed X.btnY || isKeyPressed K.Key'S) $ do
    S.command $ S.SetAngle "jnt-l" $ pi/2 -- ジョイントの角度を pi / 2 に指定
    S.command $ S.SetAngle "jnt-r" $ -pi/2

  -- === Jet - ジェット
  -- 左ジェット
  holdingQ <- S.getKeyboardState K.Key'Q
  let jetLStrength
        | holdingQ  = 10
        | otherwise = case IM.lookup X.axisLTrigger axes of
                        Just x -> ((x + 1) / 2) * 10
                        _      -> 0
  let name = "jet-l"
  if jetLStrength < 0.2
    then S.command $ S.SetJetOperation name Nothing -- ジェットを切る
    else S.command $ S.SetJetOperation name $ Just $ JetOpPower jetLStrength -- ジェットを設定
  -- 右ジェット
  holdingE <- S.getKeyboardState K.Key'E
  let jetRStrength
        | holdingE  = 10
        | otherwise = case IM.lookup X.axisRTrigger axes of
                        Just x -> ((x + 1) / 2) * 10
                        _      -> 0
  let name = "jet-r"
  if jetRStrength < 0.2
    then S.command $ S.SetJetOperation name Nothing
    else S.command $ S.SetJetOperation name $ Just $ JetOpPower jetRStrength
