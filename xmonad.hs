{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

--import           Data.Attoparsec
--import           Data.Data
import qualified Data.Map                    as M
import           Data.Monoid
--import           Language.Haskell.TH
--import           Language.Haskell.TH          (Exp (..), Pat (..), Q)
--import qualified Language.Haskell.TH          as TH
--import           Language.Haskell.TH.Quote
--import           System.Exit
import           XMonad
import           XMonad.Actions.FloatSnap
--import qualified XMonad.Core                  as XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
--import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import qualified XMonad.Layout.Fullscreen    as F
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import qualified XMonad.StackSet             as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Monad
import           System.Exit
import           System.IO                   (hFlush, stdout)

--------------------------------------------------------------------------------
----------------------------------- Commands -----------------------------------
--------------------------------------------------------------------------------

-- | Launch XMonad
main :: IO ()
main = do
  -- fixPanel
  -- startKDE
  xmonad myConfig

startKDE :: IO ()
startKDE = void $ forkIO $ do
  putStrLn "Delay starting"
  threadDelay 2000000
  putStrLn "Delay done"
  putStr "Starting plasmashell ... "
  hFlush stdout
  spawn "plasmashell"
  putStrLn "[DONE]"

-- fixPanel :: IO ()
-- fixPanel = void $ forkIO $ do
--   putStrLn "Delay starting"
--   threadDelay 5000000
--   putStrLn "Delay done"
--   putStr "Restarting xfce4-panel ... "
--   hFlush stdout
--   spawn "xfce4-panel -r"
--   putStrLn "[DONE]"

myConfig = docks $ ewmh
            $ def { borderWidth        = 1
                  , normalBorderColor  = "gray"
                  , focusedBorderColor = "red"
                  , terminal           = "xfce4-terminal"
                  , focusFollowsMouse  = True
                  , clickJustFocuses   = True
                  , modMask            = mod4Mask
                  , keys               = myKeys
                  , mouseBindings      = myMouse
                  , workspaces         = myWorkspaces
                  , layoutHook         = myLayout
                  , logHook            = myLogHook
                  , startupHook        = myStartupHook
                  , handleEventHook    = myHandleEventHook
                  , manageHook         = myManageHook
                  }

-- | Separated from myKeymap so we can do a validity check at startup
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys cfg = mkKeymap cfg (myKeymap cfg)

-- | Key bindings
--   Quick guide: C = control
--              , M = alt
--              , S = shift
--              , M4 = super
--   A complete list of key names and other information
--   is available at the bottom of this file.
myKeymap :: XConfig Layout -> [(String, X ())]
myKeymap cfg = [ ("M4-S-<Return>",   startTerminal)
               , ("M4-S-c",          closeFocused)
               , ("M4-<Space>",      nextLayout)
               , ("M4-S-<Space>",    resetLayout)
               , ("M4-n",            refresh)
               , ("M4-<Tab>",        focusDown)
               , ("M4-S-<Tab>",      focusUp)
               , ("M4-j",            focusDown)
               , ("M4-k",            focusUp)
               , ("M4-<Return>",     swapMaster)
               , ("M4-S-j",          swapDown)
               , ("M4-S-k",          swapUp)
               , ("M4-h",            shrinkMaster)
               , ("M4-l",            expandMaster)
               , ("M4-t",            retileWindow)
               , ("M4-,",            incrementMaster)
               , ("M4-.",            decrementMaster)
               , ("M4-q",            restartXMonad)
               , ("M4-S-q",          logoutCmd)
               , ("M4-1",            viewWS 1)
               , ("M4-2",            viewWS 2)
               , ("M4-3",            viewWS 3)
               , ("M4-4",            viewWS 4)
               , ("M4-5",            viewWS 5)
               , ("M4-6",            viewWS 6)
               , ("M4-7",            viewWS 7)
               , ("M4-8",            viewWS 8)
               , ("M4-S-1",          moveToWS 1)
               , ("M4-S-2",          moveToWS 2)
               , ("M4-S-3",          moveToWS 3)
               , ("M4-S-4",          moveToWS 4)
               , ("M4-S-5",          moveToWS 5)
               , ("M4-S-6",          moveToWS 6)
               , ("M4-S-7",          moveToWS 7)
               , ("M4-S-8",          moveToWS 8)
               -- , ("M4-w",            viewMonitor 1)
               -- , ("M4-e",            viewMonitor 2)
               -- , ("M4-r",            viewMonitor 3)
               -- , ("M4-S-w",          moveToMonitor 1)
               -- , ("M4-S-e",          moveToMonitor 2)
               -- , ("M4-S-r",          moveToMonitor 3)
               , ("M1-M4-b",         toggleStruts)
               , ("M4-c",            chromiumCmd)
               , ("M4-M1-c",         chromiumCmd)
               , ("M1-M4-k",         conkerorCmd)
               , ("M1-M4-t",         teamspeakCmd)
               , ("M4-z",            rofiRunCmd)
               , ("M1-M4-z",         rofiRunCmd)
               , ("M4-w",            rofiWindowCmd)
               , ("M1-M4-w",         rofiWindowCmd)
               , ("M4-p",            rofiPassCmd)
               , ("M4-m",            mocCmd)
               , ("M1-M4-m",         mocCmd)
               , ("M1-M4-e",         emacsCmd)
               , ("M1-M4-p",         pavucontrolCmd)
               , ("<XF86AudioPlay>", mocPlayPauseCmd)
               , ("M4--",            shrinkTile)
               , ("M4-=",            expandTile)
               , ("M4-S-l",          lockScreen)
               ]
  where
    startTerminal   = spawn $ XMonad.terminal cfg
    closeFocused    = kill
    nextLayout      = sendMessage NextLayout
    resetLayout     = setLayout $ XMonad.layoutHook cfg
    focusDown       = windows W.focusDown
    focusUp         = windows W.focusUp
    focusMaster     = windows W.focusMaster
    swapMaster      = windows W.swapMaster
    swapDown        = windows W.swapDown
    swapUp          = windows W.swapUp
    shrinkMaster    = sendMessage Shrink
    expandMaster    = sendMessage Expand
    retileWindow    = withFocused $ windows . W.sink
    incrementMaster = sendMessage $ IncMasterN 1
    decrementMaster = sendMessage $ IncMasterN (-1)
    -- logoutCmd       = spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1"
    logoutCmd       = io (exitWith ExitSuccess)
    restartXMonad   =
      spawn $ unwords [ "if type xmonad; then"
                      , "xmonad --recompile && xmonad --restart;"
                      , "else"
                      , "xmessage xmonad not in PATH: \"$PATH\";"
                      , "fi"
                      ]
    doWorkspace f i = windows $ f $ XMonad.workspaces cfg !! (i - 1)
    doMonitor f i   = screenWorkspace i >>= flip whenJust (windows . f)
    viewWS          = doWorkspace W.greedyView
    moveToWS        = doWorkspace W.shift
    viewMonitor     = doMonitor W.view
    moveToMonitor   = doMonitor W.shift
    toggleStruts    = sendMessage ToggleStruts
    chromiumCmd     = spawn "chromium"
    conkerorCmd     = spawn "conkeror"
    teamspeakCmd    = spawn "ts3client"
    emacsCmd        = spawn "emacs"
    mocCmd          = spawn "konsole -e mocp"
    mocPlayPauseCmd = spawn "mocp -G"
    pavucontrolCmd  = spawn "pavucontrol"
    rofiRunCmd      = spawn "rofi -show run"
    rofiWindowCmd   = spawn "rofi -show window"
    rofiPassCmd     = spawn "rofi-pass"
    shrinkTile      = sendMessage MirrorShrink
    expandTile      = sendMessage MirrorExpand
    lockScreen      = spawn "sleep 0.5; qdbus org.freedesktop.ScreenSaver /ScreenSaver Lock; sleep 0.5"

-- | Mouse bindings
--   buttons: 1 = left, 2 = middle, 3 = right, 4 = scroll down, 5 = scroll up
--myMouse :: MonadIO m => [((KeyMask, Button), m ())]
myMouse :: t -> M.Map (KeyMask, Button) (Window -> X ())
myMouse _ =  M.fromList [ ((mod4Mask, button1), floatMove)
                        , ((mod4Mask, button3), resizeMove)
                        ]
  where
    floatMove w = do
      focus w
      mouseMoveWindow w
      snapMagicMove (Just 50) (Just 50) w
      --windows W.shiftMaster
    resizeMove w = do
      focus w
      mouseResizeWindow w
      snapMagicResize [R,D] (Just 50) (Just 50) w
      --windows W.shiftMaster

-- | My workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "1"
               , "2"
               , "3"
               , "4"
               , "5"
               , "6"
               , "7"
               , "8"
               ]

-- FIXME: https://www.reddit.com/r/xmonad/comments/3vkrc3/does_this_layout_exist_if_not_can_anyone_suggest/

-- | My window layouts
myLayout = modifyL unmodified
  where
    unmodified = tiled ||| Mirror tiled ||| Full
    -- default tiling algorithm partitions the screen into two panes
    tiled   = ResizableTall nmaster delta ratio []
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Functions to run on the layout
    modifyL = smartBorders . avoidStruts

-- | My event logging hook
myLogHook :: X ()
myLogHook = return ()

-- | My event handling hook
myHandleEventHook :: Event -> X All
myHandleEventHook = fullscreenEventHook

-- | My startup hook
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  return ()
  checkKeymap def (myKeymap undefined)
  return ()

-- | The 'ManageHook' for my XMonad configuration
myManageHook :: ManageHook
myManageHook = composeAll [ dialogMH
                          , strutsMH
                          , fullscreenMH
                          , specialMH ]
  where
    dialogMH     = isDialog --> doCenterFloat   -- Float dialog boxes
    strutsMH     = manageDocks                  -- Avoid struts (e.g.: a panel)
    fullscreenMH = isFullscreen --> doFullFloat -- Fixes fullscreen windows
    specialMH    = composeAll $ map (uncurry (-->)) specialWindows

-- | This is a list of programs where XMonad's default behavior is not ideal.
specialWindows :: [(Query Bool, ManageHook)]
specialWindows =
  [ (qClassN "7zFM",                                          doCenterFloat)
  , (qClassN "Arandr",                                        doCenterFloat)
  , (qClassN "Avahi-discover",                                doCenterFloat)
  , (qClassN "bssh",                                          doCenterFloat)
  , (qClassN "bvnc",                                          doCenterFloat)
  , (qClassN "File-roller",                                   doCenterFloat)
  , (qClassN "Gigolo",                                        doCenterFloat)
  , (qClassN "Ghb",                                           doCenterFloat)
  , (qClassN "melt",                                          doCenterFloat)
  , (qClassN ".nm-connection-editor-wrapped",                 doCenterFloat)
  , (qClassN "net-sf-openrocket-startup-Startup",             doCenterFloat)
  , (qClassN "net-technicpack-launcher-LauncherMain",         doCenterFloat)
  , (qClassN "sun-awt-X11-XFramePeer",                        doCenterFloat)
  , (qClassN "com-intellij-rt-execution-application-AppMain", doCenterFloat)
  -- , (qClassN "Ristretto",                                     doCenterFloat)
  , (qClassN "Unetbootin",                                    doCenterFloat)
  , (qClassN "Bustle",                                        doCenterFloat)
  , (qClassN "Xfce4-about",                                   doCenterFloat)
  , (qClassN "Xfce4-accessibility-settings",                  doCenterFloat)
  , (qClassN "Xfce4-appearance-settings",                     doCenterFloat)
  , (qClassN "Xfce4-display-settings",                        doCenterFloat)
  , (qClassN "Xfce4-keyboard-settings",                       doCenterFloat)
  , (qClassN "Xfce4-mime-settings",                           doCenterFloat)
  , (qClassN "Xfce4-mouse-settings",                          doCenterFloat)
  , (qClassN "Xfce4-notifyd-config",                          doCenterFloat)
  , (qClassN "Xfce4-session-settings",                        doCenterFloat)
  , (qClassN "Xfce4-taskmanager",                             doCenterFloat)
  , (qClassN "Xfce4-settings-manager",                        doCenterFloat)
  , (qClassN "Zenity",                                        doCenterFloat)
  , (qClassN "Wrapper-1.0",                                   doCenterFloat)
  , (qClassN "Gnuplot",                                       doCenterFloat)
  , (qClassN "MPlayer",                                       doFloat)
  , (qClassN "Gimp",                                          doFloat)
  , (qTitle  "Panel",                                         doCenterFloat)
  , (qTitle  "Add New Items",                                 doCenterFloat)
  , (qAppN   "IcedTea-Web Control Panel",                     doFloat)
  , (qAppN   "Java Control Panel",                            doFloat)
  , (qAppN   "Policy Tool",                                   doFloat)
  , (qClassN "Xfce4-panel",                                   doCenterFloat)
  , (qClassN "QtSpimbot" <&&> qTitle "Map",                   doFloat)
  -- , (qClassN "Plasma-desktop",                                doFloat)
  , (qClassN "plasmashell",                                   doFloat)
  ]
  where
    qTitle  s = title     =? s
    qAppN   s = appName   =? s
    qClassN s = className =? s


--------------------------------------------------------------------------------
----------------------------------- Utility ------------------------------------
--------------------------------------------------------------------------------

-- type Quasi a = (Data a, Typeable a)
--
-- -- | Quasiquoter for keyboard combinations
-- keysQ :: QuasiQuoter
-- keysQ = QuasiQuoter { quoteExp = qqKeysE, quotePat = qqKeysP }
--
-- qqKeysE :: String -> Q Exp
-- qqKeysE s = getPosition >>= keyParse s >>= dataToExpQ (const Nothing)
--
-- qqKeysP :: String -> Q Pat
-- qqKeysP s = getPosition >>= keyParse s >>= dataToPatQ defaultQP
--
-- defaultQP :: Quasi a => a -> Maybe (Q Pat)
-- defaultQP = const Nothing
--
-- getPosition = fmap transPos location where
--   transPos loc = (loc_filename loc,
--                   fst (loc_start loc),
--                   snd (loc_start loc))
--
-- keyParse :: Quasi a => String -> (String, Int, Int) -> Q [(String, a)]
-- keyParse str pos = undefined
--
-- data KeyMask = KMShift
--              | KMAlt
--              | KMCtrl
--              | KMSuper
--              deriving (Eq, Ord, Enum, Read, Show, Data)
--
-- keyComboParser :: a
-- keyComboParser = undefined


--------------------------------------------------------------------------------
-------------------------------- Documentation ---------------------------------
--------------------------------------------------------------------------------


-- Key masks

-- Find the values of M1 through M4 with xmodmap -pm
--   shift    Shift_L (0x32), Shift_R (0x3e)
--   lock
--   control  Control_L (0x25), Control_L (0x42), Control_R (0x69)
--   mod1     Alt_L (0x40), Alt_R (0x6c), Meta_L (0xcd)
--   mod2     Num_Lock (0x4d)
--   mod3
--   mod4     Super_L (0x85), Super_R (0x86), Super_L (0xce), Hyper_L (0xcf)
--   mod5     ISO_Level3_Shift (0x5c), Mode_switch (0xcb)

-- Key symbols

-- <Backspace>
-- <Tab>
-- <Return>
-- <Pause>
-- <Scroll_lock>
-- <Sys_Req>
-- <Print>
-- <Escape>, <Esc>
-- <Delete>
-- <Home>
-- <Left>, <L>
-- <Up>, <U>
-- <Right>, <R>
-- <Down>, <D>
-- <Page_Up>
-- <Page_Down>
-- <End>
-- <Insert>
-- <Break>
-- <Space>
-- <F1>-<F24>
-- <KP_Space>
-- <KP_Tab>
-- <KP_Enter>
-- <KP_F1>
-- <KP_F2>
-- <KP_F3>
-- <KP_F4>
-- <KP_Home>
-- <KP_Left>
-- <KP_Up>
-- <KP_Right>
-- <KP_Down>
-- <KP_Prior>
-- <KP_Page_Up>
-- <KP_Next>
-- <KP_Page_Down>
-- <KP_End>
-- <KP_Begin>
-- <KP_Insert>
-- <KP_Delete>
-- <KP_Equal>
-- <KP_Multiply>
-- <KP_Add>
-- <KP_Separator>
-- <KP_Subtract>
-- <KP_Decimal>
-- <KP_Divide>
-- <KP_0>-<KP_9>
-- <XF86ModeLock>
-- <XF86MonBrightnessUp>
-- <XF86MonBrightnessDown>
-- <XF86KbdLightOnOff>
-- <XF86KbdBrightnessUp>
-- <XF86KbdBrightnessDown>
-- <XF86Standby>
-- <XF86AudioLowerVolume>
-- <XF86AudioMute>
-- <XF86AudioRaiseVolume>
-- <XF86AudioPlay>
-- <XF86AudioStop>
-- <XF86AudioPrev>
-- <XF86AudioNext>
-- <XF86HomePage>
-- <XF86Mail>
-- <XF86Start>
-- <XF86Search>
-- <XF86AudioRecord>
-- <XF86Calculator>
-- <XF86Memo>
-- <XF86ToDoList>
-- <XF86Calendar>
-- <XF86PowerDown>
-- <XF86ContrastAdjust>
-- <XF86RockerUp>
-- <XF86RockerDown>
-- <XF86RockerEnter>
-- <XF86Back>
-- <XF86Forward>
-- <XF86Stop>
-- <XF86Refresh>
-- <XF86PowerOff>
-- <XF86WakeUp>
-- <XF86Eject>
-- <XF86ScreenSaver>
-- <XF86WWW>
-- <XF86Sleep>
-- <XF86Favorites>
-- <XF86AudioPause>
-- <XF86AudioMedia>
-- <XF86MyComputer>
-- <XF86VendorHome>
-- <XF86LightBulb>
-- <XF86Shop>
-- <XF86History>
-- <XF86OpenURL>
-- <XF86AddFavorite>
-- <XF86HotLinks>
-- <XF86BrightnessAdjust>
-- <XF86Finance>
-- <XF86Community>
-- <XF86AudioRewind>
-- <XF86XF86BackForward>
-- <XF86Launch0>-<XF86Launch9>, <XF86LaunchA>-<XF86LaunchF>
-- <XF86ApplicationLeft>
-- <XF86ApplicationRight>
-- <XF86Book>
-- <XF86CD>
-- <XF86Calculater>
-- <XF86Clear>
-- <XF86Close>
-- <XF86Copy>
-- <XF86Cut>
-- <XF86Display>
-- <XF86DOS>
-- <XF86Documents>
-- <XF86Excel>
-- <XF86Explorer>
-- <XF86Game>
-- <XF86Go>
-- <XF86iTouch>
-- <XF86LogOff>
-- <XF86Market>
-- <XF86Meeting>
-- <XF86MenuKB>
-- <XF86MenuPB>
-- <XF86MySites>
-- <XF86New>
-- <XF86News>
-- <XF86OfficeHome>
-- <XF86Open>
-- <XF86Option>
-- <XF86Paste>
-- <XF86Phone>
-- <XF86Q>
-- <XF86Reply>
-- <XF86Reload>
-- <XF86RotateWindows>
-- <XF86RotationPB>
-- <XF86RotationKB>
-- <XF86Save>
-- <XF86ScrollUp>
-- <XF86ScrollDown>
-- <XF86ScrollClick>
-- <XF86Send>
-- <XF86Spell>
-- <XF86SplitScreen>
-- <XF86Support>
-- <XF86TaskPane>
-- <XF86Terminal>
-- <XF86Tools>
-- <XF86Travel>
-- <XF86UserPB>
-- <XF86User1KB>
-- <XF86User2KB>
-- <XF86Video>
-- <XF86WheelButton>
-- <XF86Word>
-- <XF86Xfer>
-- <XF86ZoomIn>
-- <XF86ZoomOut>
-- <XF86Away>
-- <XF86Messenger>
-- <XF86WebCam>
-- <XF86MailForward>
-- <XF86Pictures>
-- <XF86Music>
-- <XF86TouchpadToggle>
-- <XF86_Switch_VT_1>-<XF86_Switch_VT_12>
-- <XF86_Ungrab>
-- <XF86_ClearGrab>
-- <XF86_Next_VMode>
-- <XF86_Prev_VMode>

-- Mouse symbols

-- button1 = left mouse button
-- button2 = middle mouse button
-- button3 = right mouse button
-- button4 = scroll down
-- button5 = scroll up
