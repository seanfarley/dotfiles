;;; ahk-mode.el --- Major mode for editing AutoHotKey (AutoHotkey_L) scripts for Emacs
;; -*- Mode: Emacs-Lisp -*-

;; last updated: 2013-09-30 00:26:03

;; Author       : blechmusik
;; Keywords     : AutoHotKey, AutoHotkey_L
;; License      : NYSL (http://www.kmonos.net/nysl/NYSL_withfaq.TXT)

;;; Install:
;; put this file somewhere in `load-path'
;; and load it by putting the following command
;; in your .emacs (or .emacs.d/init.el) file:
;; (require 'ahk-mode)


(eval-when-compile (require 'font-lock))
(eval-when-compile (require 'cl))



(defvar ahk-path-exe-optional nil)
(defvar ahk-registry "HKEY_CLASSES_ROOT\\AutoHotkeyScript\\Shell\\Open\\Command") 
(defvar ahk-path-exe-installed
  (let ((reg-data (shell-command-to-string (format "reg query \"%s\"" ahk-registry))))
    ;; from
    ;; "C:\\Program Files (x86)\\AutoHotkey\\AutoHotkey.exe"
    ;; to
    ;; "C:/Program Files (x86)/AutoHotkey/AutoHotkey.exe"
    (replace-regexp-in-string "\\\\" "/" (cadr (split-string reg-data "\\\"")))))
(defvar ahk-path-exe-installed-p
  (file-exists-p ahk-path-exe-installed))


(defvar ahk-mode-keyword
  '(
    ))

(defvar ahk-mode-builtin
  '(
    ))

(defvar ahk-mode-type
  '(
    "integer"
    "float"
    "number"
    "digit"
    "xdigit"
    "alpha"
    "upper"
    "lower"
    "alnum"
    "space"
    "time"
    "Ptr"
    "AStr"
    "WStr"
    ))

(defvar ahk-mode-constant
  '(
    "true"
    "false"
    ))

(defvar ahk-mode-function-name
  '(
    "Break"
    "Catch"
    "For"
    "Gosub"
    "Goto"
    "If"
    "IfExist"
    "IfInString"
    "IfMsgBox"
    "IfNotExist"
    "IfNotInString"
    "IfWinActive"
    "IfWinExist"
    "IfWinNotActive"
    "IfWinNotExist"
    "Return"
    "Throw"
    "Try"
    "Until"
    "While"
    "and"
    "for"
    "if"
    "loop"
    "not"
    "or"
    "return"
    "while"
    "else"
    "Continue"
    "Class"
    "global"
    "static"
    "local"
    ;; builtin
    "Add"
    "AutoTrim"
    "BlockInput"
    "Button"
    "Cancel"
    "Checkbox"
    "Click"
    "ClipWait"
    "Color"
    "ComboBox"
    "Control"
    "ControlClick"
    "ControlFocus"
    "ControlGet"
    "ControlGetFocus"
    "ControlGetPos"
    "ControlGetText"
    "ControlMove"
    "ControlSend"
    "ControlSetText"
    "CoordMode"
    "Critical"
    "DateTime"
    "Default"
    "Destroy"
    "DetectHiddenText"
    "DetectHiddenWindows"
    "Drive"
    "DriveGet"
    "DriveSpaceFree"
    "DropDownList"
    "Edit"
    "EnvAdd"
    "EnvDiv"
    "EnvGet"
    "EnvMult"
    "EnvSet"
    "EnvSub"
    "EnvUpdate"
    "Exit"
    "ExitApp"
    "FileAppend"
    "FileCopy"
    "FileCopyDir"
    "FileCreateDir"
    "FileCreateShortcut"
    "FileDelete"
    "FileEncoding"
    "FileGetAttrib"
    "FileGetShortcut"
    "FileGetSize"
    "FileGetTime"
    "FileGetVersion"
    "FileInstall"
    "FileMove"
    "FileMoveDir"
    "FileRead"
    "FileReadLine"
    "FileRecycle"
    "FileRecycleEmpty"
    "FileRemoveDir"
    "FileSelectFile"
    "FileSelectFolder"
    "FileSetAttrib"
    "FileSetTime"
    "Flash"
    "Font"
    "FormatTime"
    "GUI"
    "GetKeyState"
    "GroupActivate"
    "GroupAdd"
    "GroupBox"
    "GroupClose"
    "GroupDeactivate"
    "Gui"
    "GuiClose"
    "GuiContextMenu"
    "GuiControl"
    "GuiControlGet"
    "GuiDropFiles"
    "GuiEscape"
    "GuiSize"
    "Hide"
    "Hotkey"
    "ImageSearch"
    "IniDelete"
    "IniRead"
    "IniWrite"
    "Input"
    "InputBox"
    "KeyHistory"
    "KeyWait"
    "ListBox"
    "ListHotkeys"
    "ListLines"
    "ListVars"
    "ListView"
    "Loop"
    "Margin"
    "Maximize"
    "Menu"
    "Minimize"
    "MonthCal"
    "MouseClick"
    "MouseClickDrag"
    "MouseGetPos"
    "MouseMove"
    "MsgBox"
    "OnClipboardChange"
    "OnExit"
    "OutputDebug"
    "PARSE"
    "Pause"
    "Picture"
    "PixelGetColor"
    "PixelSearch"
    "PostMessage"
    "Process"
    "Progress"
    "READ"
    "Radio"
    "Random"
    "RegDelete"
    "RegRead"
    "RegWrite"
    "Reload"
    "Restore"
    "Run"
    "RunAs"
    "RunWait"
    "Send"
    "SendEvent"
    "SendInput"
    "SendMessage"
    "SendMode"
    "SendPlay"
    "SendRaw"
    "SetBatchLines"
    "SetCapsLockState"
    "SetControlDelay"
    "SetDefaultMouseSpeed"
    "SetEnv"
    "SetFormat"
    "SetKeyDelay"
    "SetMouseDelay"
    "SetNumLockState"
    "SetScrollLockState"
    "SetStoreCapslockMode"
    "SetTimer"
    "SetTitleMatchMode"
    "SetWinDelay"
    "SetWorkingDir"
    "Show"
    "Shutdown"
    "Sleep"
    "Slider"
    "Sort"
    "SoundBeep"
    "SoundGet"
    "SoundGetWaveVolume"
    "SoundPlay"
    "SoundSet"
    "SoundSetWaveVolume"
    "SplashImage"
    "SplashTextOff"
    "SplashTextOn"
    "SplitPath"
    "StatusBar"
    "StatusBarGetText"
    "StatusBarWait"
    "StringCaseSense"
    "StringGetPos"
    "StringLeft"
    "StringLen"
    "StringLower"
    "StringMid"
    "StringReplace"
    "StringRight"
    "StringSplit"
    "StringTrimLeft"
    "StringTrimRight"
    "StringUpper"
    "Submit"
    "Suspend"
    "SysGet"
    "Tab"
    "Tab2"
    "Text"
    "Thread"
    "ToolTip"
    "Transform"
    "TrayTip"
    "TreeView"
    "Trim"
    "URLDownloadToFile"
    "UpDown"
    "WinActivate"
    "WinActivateBottom"
    "WinClose"
    "WinGet"
    "WinGetActiveStats"
    "WinGetActiveTitle"
    "WinGetClass"
    "WinGetPos"
    "WinGetText"
    "WinGetTitle"
    "WinHide"
    "WinKill"
    "WinMaximize"
    "WinMenuSelectItem"
    "WinMinimize"
    "WinMinimizeAll"
    "WinMinimizeAllUndo"
    "WinMove"
    "WinRestore"
    "WinSet"
    "WinSetTitle"
    "WinShow"
    "WinWait"
    "WinWaitActive"
    "WinWaitClose"
    "WinWaitNotActive"
    "lower"
    ;; 
    "DllCall"
    "IsFunc"
    "IsLabel"
    "NumGet"
    "NumPut"
    "OnMessage"
    "RegisterCallback"
    "OnMessage"
    "ACos"
    "ASin"
    "ATan"
    "Abs"
    "Asc"
    "Ceil"
    "Chr"
    "ComObjActive"
    "ComObjConnect"
    "ComObjCreate"
    "ComObjError"
    "ComObjGet"
    "Cos"
    "DllCall"
    "Exp"
    "FileExist"
    "FileOpen"
    "Floor"
    "GetKeyState"
    "InStr"
    "IsObject"
    "LTrim"
    "Ln"
    "Log"
    "Mod"
    "NumGet"
    "NumPut"
    "Object"
    "OnMessage"
    "RTrim"
    "RegExMatch"
    "RegExReplace"
    "RegisterCallback"
    "Round"
    "Sin"
    "Sqrt"
    "StrGet"
    "StrLen"
    "StrPut"
    "Tan"
    "VarSetCapacity"
    "WinActive"
    "WinExist"
    "ComObjActive"
    "ComObjEnwrap/Unwrap"
    "ComObjParameter"
    "ComObjType         "
    "ComObjArray"
    "ComObjConnect"
    "ComObjCreate"
    "ComObjError"
    "ComObjFlags"
    "ComObjGet"
    "ComObjQuery"
    "ComObjType"
    "ComObjValue"
    "Exception"
    "FileOpen"
    "Func"
    "GetKeyName"
    "GetKeyVK"
    "GetKeySC"
    "InStr"
    "IsByRef"
    "IsObject"
    "StrPut"
    "StrGet"
    "Trim"
    "LTrim"
    "RTrim"
    ))

(defvar ahk-mode-variable-name
  '(
    "A_AhkPath"
    "A_AhkVersion"
    "A_AppData"
    "A_AppDataCommon"
    "A_AutoTrim"
    "A_BatchLines"
    "A_CaretX"
    "A_CaretY"
    "A_ComputerName"
    "A_ControlDelay"
    "A_Cursor"
    "A_DD"
    "A_DDD"
    "A_DDDD"
    "A_DefaultMouseSpeed"
    "A_Desktop"
    "A_DesktopCommon"
    "A_DetectHiddenText"
    "A_DetectHiddenWindows"
    "A_EndChar"
    "A_EventInfo"
    "A_ExitReason"
    "A_FileEncoding"
    "A_FileEncoding"
    "A_FormatFloat"
    "A_FormatInteger"
    "A_Gui"
    "A_GuiControl"
    "A_GuiControlEvent"
    "A_GuiEvent"
    "A_GuiHeight"
    "A_GuiWidth"
    "A_GuiX"
    "A_GuiY"
    "A_Hour"
    "A_IPAddress1"
    "A_IPAddress2"
    "A_IPAddress3"
    "A_IPAddress4"
    "A_IconFile"
    "A_IconHidden"
    "A_IconNumber"
    "A_IconTip"
    "A_Index"
    "A_IsAdmin"
    "A_IsCompiled"
    "A_IsCritical"
    "A_IsPaused"
    "A_IsSuspended"
    "A_IsUnicode"
    "A_IsUnicode"
    "A_KeyDelay"
    "A_Language"
    "A_LastError"
    "A_LineFile"
    "A_LineNumber"
    "A_LoopField"
    "A_LoopField"
    "A_LoopFileAttrib"
    "A_LoopFileDir"
    "A_LoopFileExt"
    "A_LoopFileFullPath"
    "A_LoopFileLongPath"
    "A_LoopFileName"
    "A_LoopFileShortName"
    "A_LoopFileShortPath"
    "A_LoopFileSize"
    "A_LoopFileSizeKB"
    "A_LoopFileSizeMB"
    "A_LoopFileTimeAccessed"
    "A_LoopFileTimeCreated"
    "A_LoopFileTimeModified"
    "A_LoopReadLine"
    "A_LoopRegKey"
    "A_LoopRegName"
    "A_LoopRegSubKey"
    "A_LoopRegTimeModified"
    "A_LoopRegType"
    "A_MDay"
    "A_MM"
    "A_MMM"
    "A_MMMM"
    "A_MSec"
    "A_Min"
    "A_Mon"
    "A_MouseDelay"
    "A_MyDocuments"
    "A_Now"
    "A_NowUTC"
    "A_OSType"
    "A_OSVersion"
    "A_OSVersion"
    "A_PriorHotkey"
    "A_PriorKey"
    "A_ProgramFiles"
    "A_Programs"
    "A_ProgramsCommon"
    "A_PtrSize"
    "A_PtrSize"
    "A_ScreenHeight"
    "A_ScreenWidth"
    "A_ScriptDir"
    "A_ScriptFullPath"
    "A_ScriptHwnd"
    "A_ScriptName"
    "A_Sec"
    "A_StartMenu"
    "A_StartMenuCommon"
    "A_Startup"
    "A_StartupCommon"
    "A_StringCaseSense"
    "A_Temp"
    "A_ThisFunc"
    "A_ThisHotkey"
    "A_ThisLabel"
    "A_ThisMenu"
    "A_ThisMenuItem"
    "A_ThisMenuItemPos"
    "A_TickCount"
    "A_TimeIdle"
    "A_TimeIdlePhysical"
    "A_TimeSincePriorHotkey"
    "A_TimeSinceThisHotkey"
    "A_TitleMatchMode"
    "A_TitleMatchModeSpeed"
    "A_UserName"
    "A_WDay"
    "A_WinDelay"
    "A_WinDir"
    "A_WorkingDir"
    "A_YDay"
    "A_YDay"
    "A_YWeek"
    "A_YYYY"
    "A_Year"
    "Clipboard"
    "ClipboardAll"
    "ComSpec"
    "ErrorLevel"
    ))

(defvar ahk-mode-warning
  '(
    "#ClipboardTimeout"
    "#CommentFlag"
    "#ErrorStdOut"
    "#EscapeChar"
    "#HotkeyInterval"
    "#HotkeyModifierTimeout"
    "#Hotstring"
    "#IfWinActive"
    "#IfWinExist"
    "#If"
    "#IncludeAgain"
    "#Include"
    "#InstallKeybdHook"
    "#InstallMouseHook"
    "#KeyHistory"
    "#MaxHotkeysPerInterval"
    "#MaxMem"
    "#MaxThreads"
    "#MaxThreadsBuffer"
    "#MaxThreadsPerHotkey"
    "#MenuMaskKey"
    "#NoEnv"
    "#NoTrayIcon"
    "#Persistent"
    "#SingleInstance"
    "#Timeout"
    "#UseHook"
    "#Warn"
    "#WinActivateForce"
    ))

(defun add-backslash-b (list)
  (interactive)
  (mapcar (lambda (x) (concat "\\b" x "\\b")) list))

(defun ahk-mode-words-from-list-to-string (list)
  (concat "\\("
          (mapconcat  'identity  list "\\|")
          "\\)"))

(defun get-string-of-current-line ()
  (interactive)
  (buffer-substring (point-at-bol) (point-at-eol)))

(defun get-string-of-backward-line ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (get-string-of-current-line)))

(defun get-string-of-forward-line ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (get-string-of-current-line)))


(defun get-spaces-from-beginning-of-string (str)
  "インデント処理を独自に実装するため、現在行の冒頭に空白がどれほどあるのか調べる。
返り値は空白の文字列"
  (interactive)
  (if (string-match "^[\s\t]+" str)
      (match-string 0 str)
    ""))


(defun ahk-newline-and-indent-and-electric-brace ()
  "insert newline, indent and electric blace."
  (interactive)
  (lexical-let ((spaces (get-spaces-from-beginning-of-string (get-string-of-current-line))))
    (newline)
    (newline)
    ;; to make string of spaces, you can use this: (make-string count ?\ )
    (insert spaces) (insert "}")
    (forward-line -1)
    (insert spaces) (indent-according-to-mode)))

(defun ahk-newline-and-indent-and-indent  ()
  "insert newline and my indent"
  (interactive)
  (lexical-let ((spaces (get-spaces-from-beginning-of-string (get-string-of-current-line))))
    (newline)
    (insert spaces) (indent-according-to-mode)))

(defun ahk-only-newline-and-indent  ()
  "insert newline and my indent"
  (interactive)
  (lexical-let ((spaces (get-spaces-from-beginning-of-string (get-string-of-current-line))))
    (newline)
    (insert spaces)))


(defun ahk-electric-brace-and-comment-p (str)
  (string-match "{.*?;.*?$" str))

(defun ahk-colon-and-comment-p (str)
  (string-match ":.*?;.*?$" str))


(defun ahk-newline-and-indent-dwim ()
  "newline-and-indent-dwim for ahk"
  (interactive)
  (let ((this-line (get-string-of-current-line))
        (indent-line-function (lambda () (insert "    "))))
    (cond
     ((or (equal "{" (format "%c" (char-before)))
          (ahk-electric-brace-and-comment-p this-line))
      (ahk-newline-and-indent-and-electric-brace))
     ((or (equal ":" (format "%c" (char-before)))
          (ahk-colon-and-comment-p this-line))
      (ahk-newline-and-indent-and-indent))
     (t
      (ahk-only-newline-and-indent)))))
    

(defun ahk-comment-dwim (arg)
  ;; comment-dwim for ahk
  (interactive "*P")
  (let ((indent-line-function (lambda () (insert ""))))
    (comment-dwim arg)))

(defun ahk-tab-to-tab-stop-dwim ()
  (interactive)
  (ahk-tab-to-tab-stop-or-backtab 'tab-to-tab-stop))

(defun ahk-backtab-dwim ()
  (interactive)
  (ahk-tab-to-tab-stop-or-backtab 'backtab))

(defun ahk-tab-to-tab-stop-or-backtab (func)
  (if mark-active
      (save-excursion
        (save-restriction
          (let ((lines (count-lines (region-beginning) (region-end))))
            (goto-char (region-beginning))
            (loop repeat lines
                  do (progn
                       (funcall func)
                       (forward-line))))
          (setq deactivate-mark nil)))
    (funcall func)))
  

(defun ahk-backtab ()
  "delete indentation"
  (interactive)
  (save-excursion
    (save-restriction
      (when (string= "\t" (char-to-string (following-char)))
        (delete-char 1)))))

(defun ahk-backward-spaces-p ()
  (save-excursion
    (save-restriction
      (and (not (bolp))
           (string= " " (char-to-string (char-before)))))
    ))

(defun ahk-delete-backward-chars-dwim ()
  (interactive)
  (save-excursion
    (save-restriction
      (if (ahk-backward-spaces-p)
          (while (ahk-backward-spaces-p)
            (backward-delete-char 1)
            )
        (delete-backward-char 1)
        ))))


(defvar ahk-mode-map nil)
(unless ahk-mode-map
  (setq ahk-mode-map (make-sparse-keymap)))


;; 以下の define-generic-mode 内の set-syntax-table のコメント参照
;; (defvar ahk-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     ;; "_"
;;     (modify-syntax-entry ?_ "w" table)
;;     ;; change escape character from "\" to "`"
;;     (modify-syntax-entry ?` "\\" table)
;;     (modify-syntax-entry ?\\ "w" table)
;;     table))




(define-generic-mode ahk-mode
  ;; comment out
  '(";"
    ("/*" . "*/"))
  ;; keyword
  ahk-mode-keyword
  ;; nil
  
  ;; color
  `(
    (,(ahk-mode-words-from-list-to-string
       '("\"\""
         "\{\{\}" "\{\}\}" "\{\\^\}" "\{\\!\}" "\{\\+\}" "\{\"\}"))
     (1 font-lock-warning-face keep))

    ;; coloring "%var%" such as "%A_ThisHotkey%
    ("\\(%\\)\\(.+?\\)\\(%\\)"
     (1 font-lock-warning-face)
     (2 font-lock-builtin-face)
     (3 font-lock-warning-face))

    ;; key name: F12::
    ;; ("^\\([^\\n\\r]+?\\)\\(::\\)$"
    ("^\\(.+?\\)\\(::\\)$"
     (1 font-lock-warning-face)
     (2 font-lock-type-face))

    ;; ex.) #include test.ahk and #singleinstance force (there is not less than 2 arguments)
    (,(concat (ahk-mode-words-from-list-to-string ahk-mode-warning) "\\s+?\\(.+?\\)$")
     (1 font-lock-warning-face)
     (2 font-lock-string-face))

    ;; ex.) #singleinstance (there is no argument)
    (,(ahk-mode-words-from-list-to-string ahk-mode-warning)
     (1 font-lock-warning-face keep))
    (,(ahk-mode-words-from-list-to-string (add-backslash-b ahk-mode-type))
     (1 font-lock-type-face))
    (,(ahk-mode-words-from-list-to-string (add-backslash-b ahk-mode-constant))
     (1 font-lock-constant-face))    
    (,(ahk-mode-words-from-list-to-string (add-backslash-b ahk-mode-builtin))
     (1 font-lock-function-name-face))
    (,(ahk-mode-words-from-list-to-string (add-backslash-b ahk-mode-function-name))
     (1 font-lock-function-name-face))
    (,(ahk-mode-words-from-list-to-string (add-backslash-b ahk-mode-variable-name))
     (1 font-lock-keyword-face))

    (,(ahk-mode-words-from-list-to-string 
       '(","
         "\\\\"
         "\\^=" "\\!=" "&&" "&="
         "\\*\\*" "\\*=" "\\*" "//" "//=" "/=" "/"
         "\\+\\+" "\\+=" "\\+" "--" "-=" "-"
         "\\.=" ":=" ":"
         "<<" "<<=" "<=" "<" "==" "=" ">=" ">>" ">>=" ">"
         "\\?"
         ))
     (1 font-lock-warning-face))

    (,(ahk-mode-words-from-list-to-string 
       '("`n" "`r" "`b" "`t" "`v" "`a" "`f"
         "`," "`%" "``" "`;")
       )
     (1 font-lock-doc-face t))

    ;; tooltip, % A_TickCount
    ("\\(%\\)\\s+?\\(.+?\\)$"
     (1 font-lock-warning-face)
     (2 font-lock-keyword-face keep))
    )
  ;; filetype
  '("\\.ahk\\'")
  ;; initializer
  (list
   (function
    (lambda ()
      (add-hook 'ahk-mode-hook
                (lambda ()
                  ;; case-incensitive
                  (set (make-local-variable 'font-lock-defaults)
                       '(generic-font-lock-keywords nil t))

                  (use-local-map ahk-mode-map)
                  
                  ;; comment of the beginning of line `;;'
                  (set (make-local-variable 'comment-add) 1)

                  ;; not using tab
                  (setq indent-tabs-mode nil)
                  ;; tab stop
                  (set (make-local-variable 'tab-stop-list)
                       (loop for i from 0 upto 120 by 4 collect i))
                  ;; tab width
                  (set (make-local-variable 'tab-width) 4)

                  ;; set-syntax-table を使うと define-generic-mode の第2引数の設定が無視されてしまう
                  ;; (set-syntax-table ahk-mode-syntax-table)

                  ;; そのため modify-syntax-entry をここで利用する
                  ;; modify-syntax-entry
                  (modify-syntax-entry ?_ "w")
                  (modify-syntax-entry ?# "w")
                  ;; change escape character from "\" to "`"
                  (modify-syntax-entry ?` "\\")
                  (modify-syntax-entry ?\\ "w")

                  ;; font-lock-keywordsの 正規表現探索で大文字小文字を区別しない
                  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
                  )
                nil 'local)
      )
    ))

  "Major mode for AutoHotkey (AutoHotkey_L)")



(defun run-this-ahk-script ()
  (interactive)
  (lexical-let*
      ((file (shell-quote-argument (buffer-file-name)))
       (optional-ahk-exe (and (stringp ahk-path-exe-optional)
                              (file-exists-p ahk-path-exe-optional)))
       (ahk-exe-path (shell-quote-argument (if optional-ahk-exe
                                               ahk-path-exe-optional
                                             ahk-path-exe-installed))))
    (if (and (stringp ahk-path-exe-optional)
             (not optional-ahk-exe))
        (error "Error: optional-ahk-exe is not found.")
      (save-window-excursion
        (async-shell-command (format "%s %s" ahk-exe-path file))))))



(provide 'ahk-mode)

;;; ahk-mode.el ends here
