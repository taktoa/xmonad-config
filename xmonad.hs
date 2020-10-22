{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

import qualified Data.Map                    as M
import           Data.Monoid
import           XMonad
import           XMonad.Actions.FloatSnap
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import qualified XMonad.StackSet             as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Monad
import           System.IO                   (hFlush, stdout)

main :: IO ()
main = xmonad myConfig

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
               , ("M1-M4-b",         toggleStruts)
               , ("M4-z",            rofiRunCmd)
               , ("M1-M4-z",         rofiRunCmd)
               , ("M4-w",            rofiWindowCmd)
               , ("M1-M4-w",         rofiWindowCmd)
               , ("M4--",            shrinkTile)
               , ("M4-=",            expandTile)
               ]
  where
    startTerminal   = spawn $ XMonad.terminal cfg
    closeFocused    = kill
    nextLayout      = sendMessage NextLayout
    resetLayout     = setLayout $ XMonad.layoutHook cfg
    focusDown       = windows W.focusDown
    focusUp         = windows W.focusUp
    swapMaster      = windows W.swapMaster
    swapDown        = windows W.swapDown
    swapUp          = windows W.swapUp
    shrinkMaster    = sendMessage Shrink
    expandMaster    = sendMessage Expand
    retileWindow    = withFocused $ windows . W.sink
    incrementMaster = sendMessage $ IncMasterN 1
    decrementMaster = sendMessage $ IncMasterN (-1)
    restartXMonad   =
      spawn $ unwords [ "if type xmonad; then"
                      , "xmonad --recompile && xmonad --restart;"
                      , "else"
                      , "xmessage xmonad not in PATH: \"$PATH\";"
                      , "fi"
                      ]
    doWorkspace f i = windows $ f $ XMonad.workspaces cfg !! (i - 1)
    viewWS          = doWorkspace W.greedyView
    moveToWS        = doWorkspace W.shift
    toggleStruts    = sendMessage ToggleStruts
    rofiRunCmd      = spawn "rofi -show run"
    rofiWindowCmd   = spawn "rofi -show window"
    shrinkTile      = sendMessage MirrorShrink
    expandTile      = sendMessage MirrorExpand

-- | Mouse bindings
--   buttons: 1 = left, 2 = middle, 3 = right, 4 = scroll down, 5 = scroll up
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

-- | My window layouts
myLayout = modifyL unmodified
  where
    unmodified = tiled ||| Full -- Mirror tiled ||| Full
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
  void $ setDefaultCursor xC_left_ptr
  void $ checkKeymap def (myKeymap undefined)

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
--
-- To find the "class" or "name", use the @xprop@ command and click on the
-- problematic window.
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
  , (qClassN "sun-awt-X11-XFramePeer",                        doCenterFloat)
  , (qClassN "com-intellij-rt-execution-application-AppMain", doCenterFloat)
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
  , (qClassN "Xfce4-notifyd",                                 doIgnore)
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
  , (qTitle  "meet.google.com is sharing your screen.",       doHideIgnore)
  , (qClassN "Xfce4-panel",                                   doCenterFloat)
  , (qClassN "QtSpimbot" <&&> qTitle "Map",                   doFloat)
  , (qClassN "Pinentry",                                      doCenterFloat)
  ]
  where
    qTitle  s = title     =? s
    qAppN   s = appName   =? s
    qClassN s = className =? s
