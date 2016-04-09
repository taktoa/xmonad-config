{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Keys where

import           Control.Applicative  hiding (many)
import           Control.Arrow        ((&&&))
import           Data.Attoparsec.Text
import           Data.List            (foldl1')
import           Data.Text            (Text, pack, unpack)
import qualified Data.Text            as T
import           Data.Text.ICU
import           XMonad


-- | Parse a key combination
comboParser :: Parser (KeyMask, KeySym)
comboParser = undefined

-- | Parse a 'KeyMask'
maskParser :: Parser KeyMask
maskParser =     "S" *> return shiftMask
             <|> "M" *> return mod1Mask
             <|> "C" *> return controlMask
             <|> "s" *> return mod4Mask

-- | Parse a 'KeySym'
keyParser :: Parser KeySym
keyParser =     genKeyParser alphaKeys
            <|> genKeyParser numberKeys
            <|> genKeyParser symbolKeys
            <|> genKeyParser functionKeys
            <|> genKeyParser specialKeys
            <|> genKeyParser multimediaKeys
  where
    genKeyParser = foldl1' (<|>) . map keyP
    keyP (n, k)  = char '<' *> string n *> char '>' *> return k


-- | The alphabetical keys and their 'KeySym's
alphaKeys :: [(Text, KeySym)]
alphaKeys = map (pack &&& stringToKeysym) $ map (:[]) ['a' .. 'z']

-- | The number keys and their 'KeySym's
numberKeys :: [(Text, KeySym)]
numberKeys = map (pack &&& stringToKeysym) $ map show [0 .. 9]

-- | The symbol keys and their 'KeySym's
symbolKeys :: [(Text, KeySym)]
symbolKeys = [ ("'", xK_apostrophe)
             , (",", xK_comma)
             , (".", xK_period)
             , ("/", xK_slash)
             , (";", xK_semicolon)
             , ("=", xK_equal)
             , ("[", xK_bracketleft)
             , ("]", xK_bracketright)
             , ("`", xK_grave)
             ]

-- | The function keys and their 'KeySym's
functionKeys :: [(Text, KeySym)]
functionKeys = [ output n k | (n, k) <- zip [1 .. 24] [xK_F1 ..]]
  where
    output n k = (pack $ 'F' : show n, k)

-- | The special keys and their 'KeySym's
specialKeys :: [(Text, KeySym)]
specialKeys = [ ("Space"      , xK_space)
              , ("Backslash"  , xK_backslash)
              , ("Backspace"  , xK_BackSpace)
              , ("Tab"        , xK_Tab)
              , ("Return"     , xK_Return)
              , ("Pause"      , xK_Pause)
              , ("Scroll_lock", xK_Scroll_Lock)
              , ("Sys_Req"    , xK_Sys_Req)
              , ("Print"      , xK_Print)
              , ("Escape"     , xK_Escape)
              , ("Esc"        , xK_Escape)
              , ("Delete"     , xK_Delete)
              , ("Home"       , xK_Home)
              , ("Left"       , xK_Left)
              , ("Up"         , xK_Up)
              , ("Right"      , xK_Right)
              , ("Down"       , xK_Down)
              , ("L"          , xK_Left)
              , ("U"          , xK_Up)
              , ("R"          , xK_Right)
              , ("D"          , xK_Down)
              , ("Page_Up"    , xK_Page_Up)
              , ("Page_Down"  , xK_Page_Down)
              , ("End"        , xK_End)
              , ("Insert"     , xK_Insert)
              , ("Break"      , xK_Break)
              , ("Space"      , xK_space)
              , ("KP_Space"   , xK_KP_Space)
              , ("KP_Tab"     , xK_KP_Tab)
              , ("KP_Enter"   , xK_KP_Enter)
              , ("KP_F1"      , xK_KP_F1)
              , ("KP_F2"      , xK_KP_F2)
              , ("KP_F3"      , xK_KP_F3)
              , ("KP_F4"      , xK_KP_F4)
              , ("KP_Home"    , xK_KP_Home)
              , ("KP_Left"    , xK_KP_Left)
              , ("KP_Up"      , xK_KP_Up)
              , ("KP_Right"   , xK_KP_Right)
              , ("KP_Down"    , xK_KP_Down)
              , ("KP_Prior"   , xK_KP_Prior)
              , ("KP_Page_Up" , xK_KP_Page_Up)
              , ("KP_Next"    , xK_KP_Next)
              , ("KP_Page_Down", xK_KP_Page_Down)
              , ("KP_End"     , xK_KP_End)
              , ("KP_Begin"   , xK_KP_Begin)
              , ("KP_Insert"  , xK_KP_Insert)
              , ("KP_Delete"  , xK_KP_Delete)
              , ("KP_Equal"   , xK_KP_Equal)
              , ("KP_Multiply", xK_KP_Multiply)
              , ("KP_Add"     , xK_KP_Add)
              , ("KP_Separator", xK_KP_Separator)
              , ("KP_Subtract", xK_KP_Subtract)
              , ("KP_Decimal" , xK_KP_Decimal)
              , ("KP_Divide"  , xK_KP_Divide)
              , ("KP_0"       , xK_KP_0)
              , ("KP_1"       , xK_KP_1)
              , ("KP_2"       , xK_KP_2)
              , ("KP_3"       , xK_KP_3)
              , ("KP_4"       , xK_KP_4)
              , ("KP_5"       , xK_KP_5)
              , ("KP_6"       , xK_KP_6)
              , ("KP_7"       , xK_KP_7)
              , ("KP_8"       , xK_KP_8)
              , ("KP_9"       , xK_KP_9)
              ]

-- | The multimedia keys and their 'KeySym's
multimediaKeys :: [(Text, KeySym)]
multimediaKeys = filterKeys [ "XF86ModeLock"
                            , "XF86MonBrightnessUp"
                            , "XF86MonBrightnessDown"
                            , "XF86KbdLightOnOff"
                            , "XF86KbdBrightnessUp"
                            , "XF86KbdBrightnessDown"
                            , "XF86Standby"
                            , "XF86AudioLowerVolume"
                            , "XF86AudioMute"
                            , "XF86AudioRaiseVolume"
                            , "XF86AudioPlay"
                            , "XF86AudioStop"
                            , "XF86AudioPrev"
                            , "XF86AudioNext"
                            , "XF86HomePage"
                            , "XF86Mail"
                            , "XF86Start"
                            , "XF86Search"
                            , "XF86AudioRecord"
                            , "XF86Calculator"
                            , "XF86Memo"
                            , "XF86ToDoList"
                            , "XF86Calendar"
                            , "XF86PowerDown"
                            , "XF86ContrastAdjust"
                            , "XF86RockerUp"
                            , "XF86RockerDown"
                            , "XF86RockerEnter"
                            , "XF86Back"
                            , "XF86Forward"
                            , "XF86Stop"
                            , "XF86Refresh"
                            , "XF86PowerOff"
                            , "XF86WakeUp"
                            , "XF86Eject"
                            , "XF86ScreenSaver"
                            , "XF86WWW"
                            , "XF86Sleep"
                            , "XF86Favorites"
                            , "XF86AudioPause"
                            , "XF86AudioMedia"
                            , "XF86MyComputer"
                            , "XF86VendorHome"
                            , "XF86LightBulb"
                            , "XF86Shop"
                            , "XF86History"
                            , "XF86OpenURL"
                            , "XF86AddFavorite"
                            , "XF86HotLinks"
                            , "XF86BrightnessAdjust"
                            , "XF86Finance"
                            , "XF86Community"
                            , "XF86AudioRewind"
                            , "XF86BackForward"
                            , "XF86Launch0"
                            , "XF86Launch1"
                            , "XF86Launch2"
                            , "XF86Launch3"
                            , "XF86Launch4"
                            , "XF86Launch5"
                            , "XF86Launch6"
                            , "XF86Launch7"
                            , "XF86Launch8"
                            , "XF86Launch9"
                            , "XF86LaunchA"
                            , "XF86LaunchB"
                            , "XF86LaunchC"
                            , "XF86LaunchD"
                            , "XF86LaunchE"
                            , "XF86LaunchF"
                            , "XF86ApplicationLeft"
                            , "XF86ApplicationRight"
                            , "XF86Book"
                            , "XF86CD"
                            , "XF86Calculater"
                            , "XF86Clear"
                            , "XF86Close"
                            , "XF86Copy"
                            , "XF86Cut"
                            , "XF86Display"
                            , "XF86DOS"
                            , "XF86Documents"
                            , "XF86Excel"
                            , "XF86Explorer"
                            , "XF86Game"
                            , "XF86Go"
                            , "XF86iTouch"
                            , "XF86LogOff"
                            , "XF86Market"
                            , "XF86Meeting"
                            , "XF86MenuKB"
                            , "XF86MenuPB"
                            , "XF86MySites"
                            , "XF86New"
                            , "XF86News"
                            , "XF86OfficeHome"
                            , "XF86Open"
                            , "XF86Option"
                            , "XF86Paste"
                            , "XF86Phone"
                            , "XF86Q"
                            , "XF86Reply"
                            , "XF86Reload"
                            , "XF86RotateWindows"
                            , "XF86RotationPB"
                            , "XF86RotationKB"
                            , "XF86Save"
                            , "XF86ScrollUp"
                            , "XF86ScrollDown"
                            , "XF86ScrollClick"
                            , "XF86Send"
                            , "XF86Spell"
                            , "XF86SplitScreen"
                            , "XF86Support"
                            , "XF86TaskPane"
                            , "XF86Terminal"
                            , "XF86Tools"
                            , "XF86Travel"
                            , "XF86UserPB"
                            , "XF86User1KB"
                            , "XF86User2KB"
                            , "XF86Video"
                            , "XF86WheelButton"
                            , "XF86Word"
                            , "XF86Xfer"
                            , "XF86ZoomIn"
                            , "XF86ZoomOut"
                            , "XF86Away"
                            , "XF86Messenger"
                            , "XF86WebCam"
                            , "XF86MailForward"
                            , "XF86Pictures"
                            , "XF86Music"
                            , "XF86TouchpadToggle"
                            , "XF86_Switch_VT_1"
                            , "XF86_Switch_VT_2"
                            , "XF86_Switch_VT_3"
                            , "XF86_Switch_VT_4"
                            , "XF86_Switch_VT_5"
                            , "XF86_Switch_VT_6"
                            , "XF86_Switch_VT_7"
                            , "XF86_Switch_VT_8"
                            , "XF86_Switch_VT_9"
                            , "XF86_Switch_VT_10"
                            , "XF86_Switch_VT_11"
                            , "XF86_Switch_VT_12"
                            , "XF86_Ungrab"
                            , "XF86_ClearGrab"
                            , "XF86_Next_VMode"
                            , "XF86_Prev_VMode"
                            ]
  where
    filterKeys = filter ((/= noSymbol) . snd) . map (id &&& toKeySym)
    toKeySym = stringToKeysym . unpack
