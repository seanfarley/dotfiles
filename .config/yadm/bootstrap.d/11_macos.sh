#!/bin/sh

# Looking for more settings? See here,
# https://macos-defaults.com
# ~/.macos — https://mths.be/macos

system_type=$(uname -s)
is_sudo=$(sh -c "sudo -vn 2>&1 && echo password" | grep -c password)

if [ "$system_type" = "Darwin" ] && [ $is_sudo = 1 ]; then

    echo "=============================================================================="
    echo "Setup MacOS ..."
    echo "=============================================================================="

    # Close any open System Preferences panes, to prevent them from overriding
    # settings we’re about to change
    osascript -e 'tell application "System Preferences" to quit'

    # Ask for the administrator password upfront
    sudo -v

    # Keep-alive: update existing `sudo` time stamp until `.macos` has finished
    while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

    ###############################################################################
    # General UI/UX                                                               #
    ###############################################################################

    # Set computer name (as done via System Preferences → Sharing)
    lname="laptop"
    if [ "$(sysctl -n machdep.cpu.model 2>/dev/null)" -eq 70 ]; then
        lname="laptop2014"
    elif [ "$USER" == "sean.farley" ]; then
        lname="glados"
    fi
    sudo scutil --set ComputerName "$lname"
    sudo scutil --set HostName "$lname"
    sudo scutil --set LocalHostName "$lname"
    sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "$lname"

    # Disable the sound effects on boot
    sudo nvram SystemAudioVolume=" "

    defaults write NSGlobalDomain NSQuitAlwaysKeepsWindows -bool true

    # show battery percentage
    defaults write com.apple.menuextra.battery ShowPercent -bool true

    # Disable ext change warning
    defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

    # Automatically quit printer app once the print jobs complete
    defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

    # Disable the “Are you sure you want to open this application?” dialog
    defaults write com.apple.LaunchServices LSQuarantine -bool false

    # Check for software updates daily, not just once per week
    defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

    # # Disable guest account
    # sudo defaults write /Library/Preferences/com.apple.loginwindow SHOWOTHERUSERS_MANAGED -bool FALSE
    # sudo defaults write /Library/Preferences/com.apple.loginwindow GuestEnabled -bool NO
    # sudo defaults write /Library/Preferences/com.apple.AppleFileServer guestAccess -bool NO
    # sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server AllowGuestAccess -bool NO
    # sudo defaults write /Library/Preferences/com.apple.FindMyMac FMMEnabled -bool NO

    # Auto hide the menu bar
    defaults write NSGlobalDomain _HIHideMenuBar -bool true

    # Set the datetime to "Tue Dec 20 2:52 PM"
    defaults write com.apple.menuextra.clock DateFormat "EEE MMM d  h:mm a"

    # Disable automatic capitalization as it’s annoying when typing code
    defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

    # Disable automatic period substitution as it’s annoying when typing code
    defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false

    # Disable auto-correct
    defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

    ###############################################################################
    # Trackpad, mouse, keyboard, Bluetooth accessories, and input                 #
    ###############################################################################

    # Always display full control strip (ignoring App Controls)
    defaults write com.apple.touchbar.agent PresentationModeGlobal fullControlStrip

    # Trackpad: enable tap to click for this user and for the login screen
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
    defaults write com.apple.AppleMultitouchTrackpad Clicking -int 1
    defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
    defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

    # Trackpad: map two-finger tap to right-click
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
    defaults write com.apple.AppleMultitouchTrackpad TrackpadRightClick -bool true

    # Enable secondary click globally
    defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true

    # Disable three finger swipe for full-screen app
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerHorizSwipeGesture -int 0

    # Enable three finger drag
    defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true

    # Enable three finger tap (look up)
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerTapGesture -int 2

    # Zoom in or out
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadPinch -bool true

    # Swipe between pages with two fingers
    defaults write NSGlobalDomain AppleEnableSwipeNavigateWithScrolls -bool true

    # Smart zoom, double-tap with two fingers
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadTwoFingerDoubleTapGesture -bool true

    # Rotate
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRotate -bool true

    # Enable other multi-finger gestures
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerVertSwipeGesture -int 0
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadFourFingerVertSwipeGesture -int 2
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadFourFingerPinchGesture -int 2
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadFourFingerHorizSwipeGesture -int 2
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadFiveFingerPinchGesture -int 2

    defaults write com.apple.dock showMissionControlGestureEnabled -bool true
    defaults write com.apple.dock showAppExposeGestureEnabled -bool true
    defaults write com.apple.dock showDesktopGestureEnabled -bool true
    defaults write com.apple.dock showLaunchpadGestureEnabled -bool true

    # Enable full keyboard access for all controls
    # (e.g. enable Tab in modal dialogs)
    defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

    # Disable press-and-hold for keys in favor of key repeat
    defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

    # Set a blazingly fast keyboard repeat rate
    defaults write NSGlobalDomain KeyRepeat -int 1
    defaults write NSGlobalDomain InitialKeyRepeat -int 20

    ###############################################################################
    # Finder                                                                      #
    ###############################################################################

    # Finder: allow quitting via ⌘ + Q; doing so will also hide desktop icons
    defaults write com.apple.finder QuitMenuItem -bool true

    # Finder: disable window animations and Get Info animations
    defaults write com.apple.finder DisableAllAnimations -bool true

    # Set Desktop as the default location for new Finder windows
    # For other paths, use `PfLo` and `file:///full/path/here/`
    defaults write com.apple.finder NewWindowTarget -string "PfHm"
    defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"

    # Show icons for hard drives, servers, and removable media on the desktop
    defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool false
    defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false
    defaults write com.apple.finder ShowMountedServersOnDesktop -bool false
    defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool false

    # Finder: show all filename extensions
    defaults write NSGlobalDomain AppleShowAllExtensions -bool true

    # Finder: show status bar
    defaults write com.apple.finder ShowStatusBar -bool true

    # Finder: show path bar
    defaults write com.apple.finder ShowPathbar -bool true

    # Finder: allow text selection in Quick Look
    defaults write com.apple.finder QLEnableTextSelection -bool true

    # When performing a search, search the current folder by default
    defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

    # Disable the warning when changing a file extension
    defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

    # Enable spring loading for directories
    defaults write NSGlobalDomain com.apple.springing.enabled -bool true

    # Remove the spring loading delay for directories
    defaults write NSGlobalDomain com.apple.springing.delay -float 0

    # Avoid creating .DS_Store files on network or USB volumes
    defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
    defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

    # Disable disk image verification
    defaults write com.apple.frameworks.diskimages skip-verify -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
    defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true

    # Automatically open a new Finder window when a volume is mounted
    defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
    defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
    defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

    # Show item info near icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

    # Show item info to the right of the icons on the desktop
    /usr/libexec/PlistBuddy -c "Set DesktopViewSettings:IconViewSettings:labelOnBottom false" ~/Library/Preferences/com.apple.finder.plist

    # Enable snap-to-grid for icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

    # Increase grid spacing for icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:gridSpacing 100" ~/Library/Preferences/com.apple.finder.plist

    # Increase the size of icons on the desktop and in other icon views
    /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist

    # Use column view in all Finder windows by default
    # Four-letter codes for the other view modes: `icnv`, `Nlsv`, `Flwv`
    defaults write com.apple.finder FXPreferredViewStyle -string "clmv"

    # Disable the warning before emptying the Trash
    defaults write com.apple.finder WarnOnEmptyTrash -bool false

    # Don't empty Trash securely by default
    defaults write com.apple.finder EmptyTrashSecurely -bool false

    ###############################################################################
    # Dock, Dashboard, and hot corners                                            #
    ###############################################################################

    # Minimize windows into their application’s icon
    defaults write com.apple.dock minimize-to-application -bool true

    # Enable spring loading for all Dock items
    defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true

    # Show indicator lights for open applications in the Dock
    defaults write com.apple.dock show-process-indicators -bool true

    # Wipe all (default) app icons from the Dock
    # This is only really useful when setting up a new Mac, or if you don’t use
    # the Dock to launch apps.
    defaults write com.apple.dock persistent-apps -array

    # Don’t animate opening applications from the Dock
    defaults write com.apple.dock launchanim -bool false

    # Speed up Mission Control animations
    defaults write com.apple.dock expose-animation-duration -float 0.1

    # Don’t automatically rearrange Spaces based on most recent use
    defaults write com.apple.dock mru-spaces -bool false

    # Remove the auto-hiding Dock delay
    defaults write com.apple.dock autohide-delay -float 0
    # Remove the animation when hiding/showing the Dock
    defaults write com.apple.dock autohide-time-modifier -float 0

    # Automatically hide and show the Dock
    defaults write com.apple.dock autohide -bool true

    # Make Dock icons of hidden applications translucent
    defaults write com.apple.dock showhidden -bool true

    # Don’t show recent applications in Dock
    defaults write com.apple.dock show-recents -bool false

    # Disable the Launchpad gesture (pinch with thumb and three fingers)
    defaults write com.apple.dock showLaunchpadGestureEnabled -int 0

    # Hot corners
    # Possible values:
    #  0: no-op
    #  2: Mission Control
    #  3: Show application windows
    #  4: Desktop
    #  5: Start screen saver
    #  6: Disable screen saver
    #  7: Dashboard
    # 10: Put display to sleep
    # 11: Launchpad
    # 12: Notification Center
    # 13: Lock Screen
    # Top left screen corner → no-op
    defaults write com.apple.dock wvous-tl-corner -int 0
    defaults write com.apple.dock wvous-tl-modifier -int 0
    # Top right screen corner → Lock screen
    defaults write com.apple.dock wvous-tr-corner -int 10
    defaults write com.apple.dock wvous-tr-modifier -int 0
    # Bottom left screen corner → no-op
    defaults write com.apple.dock wvous-bl-corner -int 0
    defaults write com.apple.dock wvous-bl-modifier -int 0
    # Bottom right screen corner → no-op
    defaults write com.apple.dock wvous-br-corner -int 0
    defaults write com.apple.dock wvous-br-modifier -int 0

    # disable mission control ctrl-left/right keys
    /usr/libexec/PlistBuddy -c "Set :AppleSymbolicHotKeys:79:enabled false" ~/Library/Preferences/com.apple.symbolichotkeys.plist
    /usr/libexec/PlistBuddy -c "Set :AppleSymbolicHotKeys:80:enabled false" ~/Library/Preferences/com.apple.symbolichotkeys.plist
    /usr/libexec/PlistBuddy -c "Set :AppleSymbolicHotKeys:81:enabled false" ~/Library/Preferences/com.apple.symbolichotkeys.plist
    /usr/libexec/PlistBuddy -c "Set :AppleSymbolicHotKeys:82:enabled false" ~/Library/Preferences/com.apple.symbolichotkeys.plist
    # defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 82 "{enabled = 0; value = { parameters = (65535, 124, 393216); type = 'standard'; }; }"
    # defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 81 "{enabled = 0; value = { parameters = (65535, 124, 262144); type = 'standard'; }; }"
    # defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 80 "{enabled = 0; value = { parameters = (65535, 123, 393216); type = 'standard'; }; }"
    # defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 79 "{enabled = 0; value = { parameters = (65535, 123, 262144); type = 'standard'; }; }"

    # disable ctrl-up
    defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 32 "{enabled = 0; value = { parameters = (65535, 124, 393216); type = 'standard'; }; }"
    defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 34 "{enabled = 0; value = { parameters = (65535, 124, 393216); type = 'standard'; }; }"


    # disable ctrl-down
    defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 33 "{enabled = 0; value = { parameters = (65535, 124, 393216); type = 'standard'; }; }"
    defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 35 "{enabled = 0; value = { parameters = (65535, 124, 393216); type = 'standard'; }; }"


    ###############################################################################
    # Safari & WebKit                                                             #
    ###############################################################################

    # Enable the Develop menu and the Web Inspector in Safari
    defaults write com.apple.Safari IncludeDevelopMenu -bool true
    defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
    # defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

    # Add a context menu item for showing the Web Inspector in web views
    defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

    # Disable "safe" web ads
    defaults write com.apple.Safari WebKitPreferences.privateClickMeasurementEnabled -bool false

    # Update extensions automatically
    # defaults write com.apple.Safari InstallExtensionUpdatesAutomatically -bool true

    # switch to duckduckgo search enginge
    defaults write NSGlobalDomain NSPreferredWebServices -dict \
        NSPreferredWebServices '{NSDefaultDisplayName=DuckDuckGo;NSProviderIdentifier="com.duckduckgo";}'
    defaults write com.apple.Safari SearchProviderShortName DuckDuckGo

    ###############################################################################
    # Time Machine                                                                #
    ###############################################################################

    # Prevent Time Machine from prompting to use new hard drives as backup volume
    defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

    ###############################################################################
    # Homebrew                                                                    #
    ###############################################################################

    # python stuff
    pip3 install --user -U \
        cython \
        flake8 \
        jedi \
        nose \
        pytest \
        "python-lsp-server[all]" \
        python-lsp-black \
        python-lsp-ruff \
        sphinx

    # beets stuff
    pip3 install --user -U beets pyacoustid

    # also, also make sure hammerspoon is open
    if ! pgrep -q Hammerspoon; then
        open -a Hammerspoon
    fi

    # hammerspoon settings
    defaults write org.hammerspoon.Hammerspoon MJShowDockIconKey -bool False
    defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"

    ###############################################################################
    # Customize Finder Sidebar                                                    #
    ###############################################################################

    # remove everything but AirDrop
    mysides list | grep -v AirDrop | awk '{print $1}' | xargs -I{} mysides remove {}

    mysides add sean "file://$HOME"
    mysides add Downloads "file://$HOME/Downloads"
    mysides add Nextcloud "file://$HOME/Nextcloud"
    mysides add projects "file://$HOME/projects"

    ###############################################################################
    # Kill affected applications                                                  #
    ###############################################################################

    for app in "Activity Monitor" \
        "cfprefsd" \
        "Dock" \
        "Finder" \
        "SystemUIServer"; do
        killall "${app}" &> /dev/null
    done
    echo "Done. Note that some of these changes require a logout/restart to take effect."

fi
