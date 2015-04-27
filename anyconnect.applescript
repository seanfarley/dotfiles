-- 1. Export to .app to run from spotlight (I place mine in /Applications/Cisco)
-- 2. Substitute "/usr/bin/security ..." with the equivalent command to get your password
-- 3. Open Security & Privacy System Preferences, go to Privacy, Accessibility
-- 4. Enable this app and System UI Server (should be prompted by the system if you forgot)
-- 5. Run from spotlight
-- 6. Enjoy being connected
-- 7. Run script again to close connection (or just quit anyconnect from the menu bar)


-- AnyConnect now refered to as targetApp
set targetApp to "Cisco AnyConnect Secure Mobility Client"


-- Determine if AnyConnect is currently running
tell application "System Events"
	set processExists to exists process targetApp
end tell

set thePassword to do shell script "/usr/bin/security find-internet-password -w -s atlassian.onelogin.com"

-- Close connection if running; else start connection and fill in password
if processExists is true then
	tell application targetApp
		quit
	end tell
else
	tell application targetApp
		activate
	end tell
	
	tell application "System Events"
		-- Wait for first window to open. Do nothing.
		repeat until (window 1 of process targetApp exists)
			delay 1
		end repeat
		
		-- Wait for second window to open. Enter password.
		repeat until (window 2 of process targetApp exists)
			delay 2
		end repeat
		tell process targetApp
			keystroke (thePassword as string)
			keystroke tab
			keystroke ("push" as string)
			keystroke return
		end tell
		
	end tell
end if
