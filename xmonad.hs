import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare

import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.TopicSpace
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.Commands

import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.Mosaic
import XMonad.Layout.IM
import XMonad.Layout.Fullscreen

import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Prompt.Workspace

import System.IO
import System.Exit

import Data.Ratio ((%))
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified XMonad.StackSet as W


------------------------------------------------
--Basic Config
------------------------------------------------

myTerminal = "alacritty"
myShell = "fish"
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth = 3
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#0099ff"

myIconDir = "/home/tsemczyszyn/icons"
myFont = "Envy Code R"

myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myVisibleBGColor = "#424242"
myHiddenBGColor = "#3d3d3d"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#c2c2c2"
myFocusedBGColor = "#424242"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
myPatternColor = "#1f1f1f"

myXPConfig = greenXPConfig { font = "xft:Envy Code R:pixelsize=15:autohint=true" }

------------------------------------------------
--Key Bindings
------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [-- ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
     ((modm .|. shiftMask, xK_Return), spawnShell)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch password manager
    , ((modm .|. shiftMask, xK_p     ), passPrompt myXPConfig)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Lock screen
    , ((modm .|. shiftMask  , xK_z  ), spawn "slock")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm, xK_g), goToSelected def)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall conky dzen2 && killall trayer && xmonad --recompile; xmonad --restart")
    -- Screenshots
    , ((0, xK_Print), spawn "scrot")
    , ((modm              , xK_n     ), spawnShell) -- %! Launch terminal
    , ((modm              , xK_a     ), currentTopicAction myTopicConfig)
    , ((modm              , xK_g     ), promptedGoto)
    , ((modm .|. shiftMask, xK_g     ), promptedShift)
    ]
    -- ++
    -- [ ((modm, k), switchNthLastFocused myTopicConfig i)
    -- | (i, k) <- zip [1..] workspaceKeys
    -- ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------
--Additional Functions
------------------------------------------------
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawnHere $ myTerminal ++ " --working-directory " ++ dir 

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

------------------------------------------------
--Topic Spaces
------------------------------------------------

myTopics =
    [ "main"
    , "games"
    , "im"
    , "sandbox"
    , "music"
    , "virtual"
    , "office"
    , "meetings"
    , "code"
    , "void"
    ]

myTopicConfig = def 
    { topicDirs = M.fromList $
        [ ("main", "~/")
        , ("games", "~/")
        , ("im", "~/")
        , ("sandbox", "~/")
        , ("music", "~/")
        , ("virtual", "~/")
        , ("office", "~/Documents")
        , ("meetings", "~/")
        , ("code", "~/sources")
        , ("void", "~/")
        ]
    , defaultTopicAction = const $ spawnShell >*> 1 
    , defaultTopic = "dashboard"
    , topicActions = M.fromList $
        [ ("main", spawnHere "firefox")
        , ("games", spawnHere "steam")
        , ("music", spawnHere "spotify")
        , ("office", spawnHere "libreoffice")
        , ("meetings", spawnHere "zoom")
        ]
    }

------------------------------------------------
--Mouse Bindings
------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------
--Logging
------------------------------------------------

myLogHook h = dynamicLogWithPP $ def { ppOutput = hPutStrLn h}

------------------------------------------------
--Status Bar Stuff
------------------------------------------------
--
myDzenBar = "dzen2 -dock -e 'button2=;' -ta 'l' -x '0' -w '1000' -y '0' -h '15' -fn '" ++ myFont ++ ":size=10' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "'" 
myRightBar = "conky | dzen2 -dock -e 'button2=;' -ta r -x '1000' -w '920' -y '0' -h '15' -fn '" ++ myFont ++ ":size=10' -bg '" ++ myNormalBGColor ++ "'"

------------------------------------------------
--Dzen PP
------------------------------------------------

myDzenPP h = def
    { ppCurrent = wrap ("^fg(" ++ myFocusedFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()") "^fg()^bg()^p()"  
    , ppVisible = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg(" ++ myDzenBGColor ++ ")^p()") "^fg()^bg()^p()"
    , ppUrgent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myDzenBGColor ++ ")^p()") "^fg()^bg()^p()"
    , ppSep = " "
    , ppWsSep = " "
    , ppOutput= hPutStrLn h
    }

myDzenPP_ h = def 
    { ppCurrent = wrap ("^ib(1)^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(100x12)^p(-100)^fg(" ++ myFocusedFGColor ++ ")") ("^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(100x12)^p(-100)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId 
    , ppVisible = wrap ("^ib(1)^fg(" ++ myVisibleBGColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(100x12)^p(-100)^fg(" ++ myNormalFGColor ++ ")") ("^fg(" ++ myVisibleBGColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myDzenBGColor ++ ")^r(100x12)^p(-100)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId 
    , ppHidden = wrap ("^ib(1)^fg(" ++ myDzenBGColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(100x12)^p(-100)^fg(" ++ myDzenFGColor ++ ")") ("^fg(" ++ myDzenBGColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(100x12)^p(-100)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId 
    , ppUrgent = wrap ("^ib(1)^fg(" ++ myDzenBGColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(100x12)^p(-100)^fg(" ++ myUrgentFGColor ++ ")") ("^fg(" ++ myDzenBGColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(100x12)^p(-100)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId 
    , ppSep = " >> "
    , ppWsSep = " "
    , ppSort = mkWsSort $ getXineramaWsCompare 
    , ppOutput= hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = [myTopics]
------------------------------------------------
--Layout
------------------------------------------------

myLayout = 
    smartBorders $ 
    avoidStruts $ 
    onWorkspace "virtual" (Grid ||| all) $
    onWorkspace "im" (gridIM (1%7) (Title "Hangouts")) $
    onWorkspace "games" (fullscreenFull Full) $
    all
        where
        all = layoutHook def

------------------------------------------------
--Manage Hooks
------------------------------------------------

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "XBoard"         --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "XTerm"          --> doFloat
    , className =? "Steam"          --> doFullFloat
    , className =? "Skype"          --> doF (W.shift "im")
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore]
    --, stringProperty "WM_WINDOW_ROLE" =? "pop-up"          --> doF (W.shift "im")]

------------------------------------------------
--Ugency Hook
------------------------------------------------
--
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "650", "-y", "1064", "-h", "16", "-w", "1020", "-ta", "r", "-p", "10", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }

------------------------------------------------
--MISC
------------------------------------------------

myWeatherBar = "tail -f /home/tsemczyszyn/.cache/metar/metar | dzen2 -e 'button2=;' -ta 'l' -x '0' -w '650' -y '1064' -h '16' -fn '" ++ myFont ++ "' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "'" 
trayer = "trayer --SetPartialStrut true --alpha 0 --tint 0x000000 --transparent true --height 18"

------------------------------------------------
--Main
------------------------------------------------

main = do
    dzenBar <- spawnPipe myDzenBar
    rightBar <- spawn myRightBar
    weather <- spawn myWeatherBar
    trayer <- spawn trayer
    checkTopicConfig myTopics myTopicConfig
    xmonad $ myUrgencyHook $ docks $ ewmh def
        { manageHook = manageDocks <+> myManageHook <+> manageHook def
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP $ myDzenPP_ dzenBar 
        , keys = myKeys
        , mouseBindings  = myMouseBindings
        , workspaces = myTopics
        , modMask = myModMask
        , normalBorderColor = myNormalBorderColor
        , borderWidth = myBorderWidth
        , focusedBorderColor = myFocusedBorderColor
        }
