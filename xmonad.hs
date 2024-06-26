import XMonad

import XMonad.Util.EZConfig (additionalKeysP, remapKeysP, removeKeysP)
import XMonad.Hooks.EwmhDesktops

-- XMobar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers

-- Workspaces
import XMonad.Actions.CycleWS

-- Borders and useless gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

-- scratchpads
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask = mod4Mask
		, borderWidth = 4
		, terminal = "kitty"
		, normalBorderColor = "#A89984"
		, focusedBorderColor = "#458588"
		, layoutHook = spacingWithEdge 5 . smartBorders $ layoutHook def
		, manageHook = namedScratchpadManageHook scratchpads
		, workspaces = myWorkspaces
    }
		`additionalKeysP`
		[("M-<R>", nextWS)
		-- ,("M-w", spawn "qutebrowser")
		,("M-<L>", prevWS)
		,("M-o", spawn "rofi -show drun")
		,("M-S-o", spawn "rofi -show run")
		,("M-S-y", namedScratchpadAction scratchpads "pdfs")
		,("M-y", namedScratchpadAction scratchpads "obsidian")
		,("M-S-u", namedScratchpadAction scratchpads "journal_paper")
		,("M-w", namedScratchpadAction scratchpads "qutebrowser")]
		`remapKeysP`
		[("M-r", "M-q") -- change the restart to meta-r
		,("M-q", "M-S-c") -- change quit to mera-q
		,("M-<Return>", "M-S-<Return>") -- flip the spawn terminal and the focus
		,("M-S-<Return>", "M-<Return>")
		,("M-e", "M-<Space>")
		,("M-S-e", "M-S-<Space>")
		,("M-S-t", "M-t")]
		`removeKeysP`
		["M-t"]


-- bar config
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = green " • "
    , ppCurrent         = blue . wrap " " ""
    , ppHidden          = yellow . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    -- , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppOrder           = \[ws, l, _, win] -> [ws, l, win]
    , ppExtras          = [logClassnames blue yellow]
    }
  where
    blue, lowWhite, green, red, white, yellow :: String -> String
    green  = xmobarColor "#98971A" ""
    blue     = xmobarColor "#458588" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#D79921" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

-- scratchpad config
scratchpads :: [NamedScratchpad]
scratchpads = [
    NS "pdfs" "kitty -T pdfs -e yazi" (title =? "pdfs") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
		, NS "obsidian" "kitty -T obsidian -d ~/Documents/Vaults/Research -e nvim Tasks.md" (title =? "obsidian") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
		, NS "journal_paper" "kitty -T journal_paper -d ~/Documents/ -e yazi" (title =? "journal_paper") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
		, NS "qutebrowser" "qutebrowser" (className=? "qutebrowser") (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6))
  ]

-- Workspaces
myWorkspaces :: [String]
myWorkspaces = ["\9679 ","\9680 ","\9681 ","\9682 ","\9683 ","\9684 ","\9685 ","\9686 ","\9687 "]
