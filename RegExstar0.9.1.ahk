#NoEnv
#include C:\Users\Robert\Documents\AutoHotkey\Lib\ConsoleLog.ahk
#SingleInstance, force
; Title = RegExstar
; Author = WizardZedd
; Version = 0.9.2
; Released = 2/10/2016

; ======================================= RegExstar ===================================================
;	Performs quick RegEx Operations.
;	Store your Commonly used RegEx's in one place.
;	Create and test new RegEx's.
;
; ======================================= Main Features ===============================================
;	RegexReplace Mode:
;		Based on RegExReplace Command. (See https://autohotkey.com/docs/commands/RegExReplace.htm)
;		Perform a replacement.
; 		Click on count to count the RegEx matches without replacements.
;		Undo Option.
;	RegexMatch Mode:
;		Based on RegExMatch Command. (See https://autohotkey.com/docs/commands/RegExMatch.htm)
;		Captures Subpatterns, which can be seen by clicking the "Pattern Details" Button or in the View Menu.
;		Highlighting of Current Match
;		Next Match button, to quickly find the next match (^f).
;	Match List:
;		See all the matches in the Match ListBox in either mode.
;		Double click on a match to highlight it in the haystack.
;
; ======================================= Other Features ==============================================
;	Storage of your favorite RegEx Needles.
;	Pick up where you left off ini file.
;	Examples of Regular Expressions.
;	Copy Code - Copies the command to the clipboard.
; 	Conversion between text and var regex. (` to \, "" to ") 
;	Toolbox of Regular Expressions
; =====================================================================================================

; ======================================= GUI =========================================================
; To open Gui, 
; 	Double Click on Tray OR
; 	control-shift-r copies selected text if available, otherwise uses whatever is on the clipboard 
; 	as haystack and opens Gui.
;
; ======================================= Gui Hotkeys =================================================
; 	^c: Copies RegEx code to clipboard if nothing is selected
; 	^o: Open A RegEx File
; 	^s: Save A RegEx - 	These files currently only store the chosen name and the needle of the RegEx 
; 	^f: Next Match in Match mode
; 	f1: Opens RegEx-Quick Reference Docs
;	-And of course many of the alt codes
; =====================================================================================================

#NoEnv
SetWorkingDir, %a_scriptdir%
OnExit, WriteIni
RegStart:=1
RegEndPos:=0
menu, tray, add, 
menu, tray, add, Open RegExstar, OpenRegExstar
menu, tray, default, Open RegExstar
; File Menu
menu, FileMenu, add, &Open Regex, Open
menu, FileMenu, add, &Save Regex, Save
menu, FileMenu, add
menu, FileMenu, add, &Help, Help
menu, FileMenu, add, &Quit, WriteIni

; Edit Menu
menu, EditMenu, add, Cut, EditMenuLabel
menu, EditMenu, add, Copy, EditMenuLabel 
menu, EditMenu, add, Paste, EditMenuLabel
menu, EditMenu, add, 
menu, EditMenu, add, Copy Code, CopyRegex
menu, EditMenu, add, Undo, Undo

; Examples Menu
menu, ExampleMenu, add, Word Count, WordCount
menu, ExampleMenu, add, No Symbols, NoSymbols
menu, ExampleMenu, add, Single-Spaced, SingleSpace
menu, ExampleMenu, add, Match Dates, DatesOnly

; View Menu
menu, ViewMenu, add, ToolBox, ViewMenuLabel
menu, ViewMenu, add, Sub-patterns, PatternDetails
menu, ViewMenu, add,
menu, ViewMenu, add, &Examples, :ExampleMenu
; Menu
menu, myMenu, add, &File, :FileMenu
menu, myMenu, add, &Edit, :EditMenu
menu, myMenu, add, &View, :ViewMenu

gosub, ReadIni ; Grab saved settings

; ==================== MAIN - GUI ===========================================
gui, main: +hwndMainHWND
gui, main: menu, myMenu
gui, main: add, text, y5 section, &Needle:
gui, main: font, s13
gui, main: add, edit, yp xp+45 w525 vRegNeedle hwndRegNeedleHWND
guicontrol, main:, RegNeedle, %RegNeedle%
gui, main: font, s8
gui, main: add, button, ys xs+570 vGoGo gGo default, &Go
gui, main: add, button, yp xp+90 gUpdateLV, Update &Matches
if(RegMode = "Match:")
	gui, main: add, ddl, xs yp+40 w65 vRegMode gSwitchRegMode, Replace:|Match:||
else
	gui, main: add, ddl, xs yp+40 w65 vRegMode gSwitchRegMode, Replace:||Match:
val = 
gui, main: add, edit, yp xp+65 w150 vRegReplace, %RegReplace%
gui, main: add, edit, yp xp w150 vRegMatch, %RegMatch%
gui, main: add, text, yp xp+155 vRegLimitText, &Limit:
gui, main: add, edit, yp xp+30 w30 vRegLimit number
gui, main: add, text, yp xp+35, &Start Position:
gui, main: add, edit, yp xp+70 w30 vRegStart gRegStartValidate hwndRSHwnd, %RegStart%
gui, main: add, Button, hidden yp xp+35 vPatternDetail gPatternDetails, &Pattern Details
;~ gui, main: add, checkbox, yp xp+5 checked vReplace, Re&place
gui, main: add, text, yp xp+85 vCountText gGo, %a_space%    &Count:
gui, main: add, edit, yp xp+55 w30 vRegReplaceCount readonly
gui, main: add, button, yp xp+35 vUndoButton gUndo, &Undo
gui, main: add, button, yp xp vNextReg gNextRegMatch hidden, &Next
gui, main: add, edit, hwndHaystackHnd xs w600 h400 vRegHaystack, %RegHaystack%
gui, main: add, listview, vMatchLV gLVClick xp+605 yp-30 hp+30 w150, #|Pos|Match
LV_ModifyCol(1, "Integer")
LV_ModifyCol(2, "Integer")
gosub, SwitchRegMode ; make sure the right controls are up
; ===================== Pattern GUI ===============================
gui, pat: +ownermain +hwndPatHWND +resize
gui, pat: add, listview, vPatternLV w200 h200, #|Pos|Len|Name|Value
LV_ModifyCol(1, "Integer")
LV_ModifyCol(2, "Integer")
LV_ModifyCol(3, "Integer")

; ===================== ToolBox GUI ================================
gui, tb: +parentmain +ToolWindow -Caption ;+hwndTBHWND ;+AlwaysOnTop 
;~ gui, tb: add, groupbox, Section w500 h500
gui, tb: add, tab2, vOptionsTab h200 w155 -wrap left, Common||Options|Assertions
gui, tb: tab, 2		; Tab 2 - Options
width:=100
w:=width/4
gui, tb: add, groupbox, section w120 h125
gui, tb: add, button, 	xs+10 yp+10	w%w%	gAddOption, i
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, m
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, s
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, x
gui, tb: add, button,  	xs+10 yp+25	wp	gAddOption, A
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, D
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, J
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, U
gui, tb: add, button,  	xs+10 yp+25	wp	gAddOption, X
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, P
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, S
gui, tb: add, button,  	xp+%w% yp	wp	gAddOption, C
; Options that are unavailable, (when inside a variable they break and the edit control only does `n newlines anyway)
;~ gui, tb: add, button,  	xs+10 yp+25	wp	gAddOption	disabled, ``n
;~ gui, tb: add, button,  	xp+%w% yp	wp	gAddOption	disabled, ``r
;~ gui, tb: add, button,  	xp+%w% yp	wp	gAddOption	disabled, ``a

gui, tb: tab, 1			; Tab 1 - Common
gui, tb: add, groupbox, section w120 h185
gui, tb: add, button,  ys+10 xs+10 	w%w% gAddFromTB, . 
gui, tb: add, button,  yp xp+%w% 	wp	gAddFromTB, * 
gui, tb: add, button,  yp xp+%w% 	wp	gAddFromTB, ? 
gui, tb: add, button,  yp xp+%w% 	wp	gAddFromTB, + 
gui, tb: add, button,  xs+10 yp+25 	w%width% gAddFromTB, {min,max}
w:=width/2
gui, tb: add, button,  xs+10 yp+25 	w%w%	gAddFromTB, [abc]
gui, tb: add, button,  yp xp+%w% 	wp	gAddFromTB, [^abc]
gui, tb: add, button,  xs+10 yp+25 	w25	gAddFromTB, \d
gui, tb: add, button,  yp xp+25 	wp	gAddFromTB, \s
gui, tb: add, button,  yp xp+25 	wp	gAddFromTB, \w
gui, tb: add, button,  yp xp+25 	wp 	gAddFromTB, \b
gui, tb: add, button,  xs+10 yp+25 	w25	gAddFromTB, ^
gui, tb: add, button,  yp xp+25 	wp	gAddFromTB, $
gui, tb: add, button,  yp xp+25 	wp	gAddFromTB, |
gui, tb: add, button,  yp xp+25 		gAddFromTB, (...)
gui, tb: add, button,  xs+10 yp+25 	w%w%	gAddFromTB, (?:...)
gui, tb: add, button,  yp xp+%w% 	w%w%	gAddFromTB, (?<...>)
gui, tb: add, button,  xs+10 yp+25 	w%width%	gAddFromTB	Center, \Q...\E

gui, tb: tab, 3		; TAB 3 - Assertions
gui, tb: add, groupbox, section w120 h70
gui, tb: add, button,  xs+10 ys+10	w%w% 	gAddFromTB, (?=..)
gui, tb: add, button,  yp xp+%w% 	wp		gAddFromTB, (?!..)
gui, tb: add, button,  xs+10 yp+25 	w40 	gAddFromTB, (?<=..)
gui, tb: add, button,  yp xp+40 	wp	 	gAddFromTB, (?<!..)
gui, tb: add, button,  yp xp+40 	w20 	gAddFromTB, \K
OnMessage(0x200, "WM_MOUSEMOVE")

OpenRegExstar: ; Tray menu to open GUI
gui, main: show,, RegExstar
return

; ======================================================================
; MENU Labels and Context Hotkeys
; ======================================================================
; EXAMPLES
WordCount:
guiControl, main:  choose, RegMode, 1
gosub, SwitchRegMode
guiControl, main:  , RegNeedle, \w+
NoReplace:=true
goto, Go
return
NoSymbols:
guiControl, main:  choose, RegMode, 1
gosub, SwitchRegMode
guiControl, main:  , RegNeedle, [^a-zA-Z0-9\s]
goto, Go
SingleSpace:
guiControl, main:  choose, RegMode, 1
gosub, SwitchRegMode
guiControl, main: , RegNeedle, (``n)``n+
guicontrol, main: , RegReplace, $1
goto, Go
DatesOnly:
guiControl, main:  choose, RegMode, 2
gosub, SwitchRegMode
guiControl, main: , RegNeedle, O)\b((?<Month31>[13578]|1[02])\/(?<Day31>[12]?\d|3[01])|(?<Month30>[469]|11)\/(?<Day30>[12]?\d|30)|(?<Feb>2)\/(?<Day29>[12]?\d))\/(?<Year>\d{1,4})\b
goto, Go
;;;

; View Menu Label
ViewMenuLabel:
	if(a_thismenuitem = "ToolBox") {
		showToolBox()
	}
return

; Edit Menu Label
EditMenuLabel:
guiControl, main: focus, RegHaystack
if(a_thismenuitem = "Paste")
	send ^v
else {
	send ^a
if(a_thismenuitem = "Cut")
	send, ^x
else if(a_thismenuitem = "Copy")
	send, ^c
}
return

; RegExstar Context Hotkeys
#IfWinActive, RegExstar
; Open
^o::
Open:
; Reg Ex Folder
IfNotExist, MyRegularExpressions
	FileCreateDir, MyRegularExpressions
FileSelectFile, tRegEx,, %a_scriptdir%\MyRegularExpressions, Select a RegEx File, Text Documents (*.txt)
if (errorlevel || tRegEx = "")
	return
fileread, myFile, %tRegEx%
if(regexmatch(myFile, "Needle=\S*", RegNeedle)) {
	StringReplace, RegNeedle, RegNeedle, Needle=
	RegNeedle:=formatRegex(RegNeedle, false)
	guicontrol, main:, RegNeedle, %RegNeedle%
} else {
	MsgBox, 16, RegExstar, File is in incorrect format. Unable to open!
}
tRegEx = 
myFile = 
return
; Save
^s::
Save:
; Reg Ex Folder
IfNotExist, MyRegularExpressions
	FileCreateDir, MyRegularExpressions
FileSelectFile, tRegEx, S, %a_scriptdir%\MyRegularExpressions, Save Your RegEx`, Note: Only The Needle is saved, Text Documents (*.txt)
if (errorlevel || tRegEx = "")
	return
gui, main: submit, nohide
SplitPath, tRegEx, name,, ext
if(ext = "")
	tRegEx .= ".txt"
else if(ext != "")
	StringReplace, tRegEx, tRegEx, %ext%, "txt"
IfExist, %tRegEx%
	FileDelete, %tRegEx%
res:="Name= " name "`nNeedle=" formatRegex(RegNeedle)
FileAppend, %res%, %tRegEx%
name=
ext=
tRegEx =
res = 
return

$^c::	; COPY
Clipboard =
send, ^c
ClipWait, 1
if(clipboard!="")
	return
CopyRegex:
gui, main: submit, nohide
if(RegMode = "Replace:") {
	clipboard:= "RegExReplace( , """ formatRegex(RegNeedle) """, """ formatRegex(RegReplace) """, Count, " RegLimit "," RegStart ")"
} else
	clipboard:= "RegExMatch( ,""" formatRegex(RegNeedle) """, outVar," RegStart ")"
return
f1::
Help:
run, https://autohotkey.com/docs/misc/RegEx-QuickRef.htm
return
    ; UNDO
Undo: 
if(oldHay = "")
	return
GuiControlGet, tempHay,, RegHaystack
RegHaystack:=oldHay
guiControl, main: , RegHaystack, %oldHay%
oldHay:=tempHay
tempHay=
return
	; NEXT Match
^f::
GuiControlGet, Mode, main:, Mode
if(Mode != "Match:")
	return
NextRegMatch:
guicontrol, main:, RegStart, % RegEndPos ? RegEndPos : RegEndPos+1
gosub, Go
return
#IfWinActive
; ==================================================================

RegStartValidate:		; Validate start position
GuiControlGet, newRegStart, , RegStart
if newRegStart is not integer  
{
	guiControl, main: , RegStart, %RegStart%	
	EM_SHOWBALLOONTIP(RSHwnd, "Unacceptable Character", "You can only type an integer here.", 3) ; 3 is an error
}
else
	RegStart:=newRegStart
return

; ============= Ini File and exitapp ====================	
^+!r:: gosub, WriteIni ; ExitApp
ReadIni:
IfExist, RegexInit.ini
{
	IniRead, RegHaystack, RegexInit.ini, Saved, RegHaystack
	RegHaystack:=RegExReplace(RegHaystack, "\Q*MLF*\E", "`n")
	IniRead, RegNeedle, RegexInit.ini, Saved, RegNeedle
	IniRead, RegMode, RegexInit.ini, Saved, RegRegMode
	IniRead, RegReplace, RegexInit.ini, Saved, RegReplace
	IniRead, RegStart, RegexInit.ini, Saved, RegStart
	RegNeedle:=formatRegex(RegNeedle, false)
}
return
WriteIni:
gui, submit, nohide
IfNotExist, RegexInit.ini
	FileAppend, [Saved]`n, RegexInit.ini
	RegHaystack:=RegExReplace(RegHaystack, "`n", "*MLF*")
IniWrite, %RegHaystack%, RegexInit.ini, Saved, RegHaystack
IniWrite, % formatRegex(RegNeedle) , RegexInit.ini, Saved, RegNeedle
	IniWrite, %RegMode%, RegexInit.ini, Saved, RegRegMode
	IniWrite, %RegReplace%, RegexInit.ini, Saved, RegReplace
	Iniwrite, %RegStart%, RegexInit.ini, Saved, RegStart
ExitApp
;===========================================================

; ============================================
; 	Tool Box
; ============================================
; ========== Add Option ================
AddOption:
	toggleOption(RegExReplace(a_guicontrol, "\s?-.*"))
return
; ========== Add Common Expressions From Tool Box ==========
AddFromTB:
	; todo - get cursor position, figure out text to add, set cursor position
	ConsoleLog("Received click on button " A_guicontrol)
	addToRegEx(a_guicontrol)
return

; ============================================
; REGEXMatch Pattern Details - Pattern GUI
; ============================================
PatternDetails:
	; Header
	ConsoleLog("Loading Pattern Details Mode: " outPutRegMode)
	gui, pat: default
	LV_Delete()
	If (outPutRegMode = "P") {
		LV_Add( , "ALL", RegReplaceCount, strlen(Output), "ALL", RegExReplace(Output, "`n", "``n"))
		Loop, % sub0
			LV_Add(, a_index, OutputPos%A_index%, OutputLen%A_index%, OutputName%a_index%) 
	} else if (outPutRegMode = "O") {
			LV_Add(, "All", Output.Pos(0), Output.Len(0), Output.Name(0), RegExReplace(Output.Value(0), "`n", "``n"))
		loop, % Output.Count()
		  LV_Add(, a_index, Output.Pos(a_index), Output.Len(a_index), Output.Name(a_index), RegExReplace(Output.Value(a_index), "`n", "``n"))			
	}
	else {
		LV_Add(,"All",,, "ALL",  RegExReplace(Output, "`n", "``n"))
		loop, % sub0
			LV_Add(,A_Index,,, OutputName%a_index%, RegExReplace(Output%A_Index%, "`n", "``n"))
	}
	guiControl, pat:, PatternEdit, %PatternEdit%
	gui, pat: show,, Sub-Patterns
  PatternEdit=
return
patGuiSize:
ConsoleLog("Sizing pattern gui")
w:=A_GuiWidth-15
h:=A_GuiHeight-15
guicontrol, pat: move, PatternLV, w%w% h%h%
return


; ===================================================
; ===================== Main GUI ====================
; ===================================================
; Control - Shift - R
^+r:: 	; Opens RegExReplace Gui, copying selected text if available, otherwise using whatever is on the clipboard.
prevClip:=Clipboard
Clipboard = 
send, ^c
ClipWait, 1
if(clipboard = "") {
	clipboard:=prevClip
}
guiControl, main: , RegHaystack, %Clipboard%
Clipboard:=prevClip
prevClip=
gui, main: show ,, RegExstar
return

SwitchRegMode:
guicontrolget, RegMode
IfInString, RegMode, Match 
{
	guiControl, main:  hide, RegLimit
	guiControl, main:  hide, RegLimitText
	;~ guiControl, main:  hide, Replace
	guiControl, main:  hide, RegReplace
	guiControl, main:  hide, UndoButton
	guiControl, main:  show, PatternDetail
	guiControl, main:  show, RegMatch
	guicontrol, main: show, NextReg
	guiControl, main:  , CountText, &Found%a_space%at:
} 
else {
	guiControl, main:  show, RegLimit
	guiControl, main:  show, RegLimitText
	;~ guiControl, main:  show, Replace
	guiControl, main:  show, RegReplace
	guiControl, main:  show, UndoButton
	guiControl, main:  hide, PatternDetail
	guiControl, main:  hide, RegMatch
	guiControl, main: , RegMatch,
	guiControl, main:  hide, NextReg
	guiControl, main: , CountText, %a_space%%a_space%%a_space%%a_space%%a_space%&Count:
}
return

; GO - Does the Regex
Go:
gui, main: submit, nohide
RegNeedle:=formatRegex(RegNeedle, false)
if(RegHaystack = "")
	guicontrol, main:, RegHaystack, % RegHaystack:=loadReference()
if(RegMode = "Replace:") {
	if(RegLimit < 1)
		RegLimit:=-1
	if(instr(a_guicontrol, "Count") || NoReplace) {   ; Count was pushed or don't replace
		RegExReplace(RegHaystack, RegNeedle, RegReplace, RegReplaceCount, RegLimit, RegStart)
		NoReplace:=false
	} else {
		RegHaystack:=RegExReplace(tempHay:=RegHaystack, "`n" . getPerformanceNeedle(), RegReplace, RegReplaceCount, RegLimit, RegStart)
		if errorlevel
			MsgBox, 16, RegEx Error, %Errorlevel%

		if(tempHay != RegHaystack)
			oldHay:=tempHay
		tempHay = 
	}
	guiControl, main:  , RegReplaceCount, %RegReplaceCount%
	guiControl, main:  , RegHaystack, %RegHaystack%
}
else {
	outPutRegMode:=getMatchMode(RegNeedle)
	if(outPutRegMode!="O") {
		; Clean up
		while(Output%a_index% != "" || OutputLen%a_index% != "") {
			Output%a_index% =
			OutputLen%a_index% =
			OutputPos%a_index% =
			OutputName%a_index% = 
		}
          pos = 1                                    ;get named subpattern
          sub = 0
        	Loop{
          		If (pos := RegExMatch(RegNeedle,"(?<!\\)\((?:\?P?<(\w+)>|'(\w+)')", Name , pos))
				{
					sub++                              ;subpattern Name index
					Name0 := Name1 = "" ? Name2 : Name1
					sub%sub% := Name0                  ;subpattern Name array
					pos += StrLen(Name)                
                }
          		Else Break                     
            }
	}
	;~ options:=getNeedleOptions(RegNeedle)
	;~ if(options = "")
		;~ RegNeedle:= ")" RegNeedle 
	RegReplaceCount:=RegExMatch(RegHaystack, "`n" . getPerformanceNeedle(), Output, RegStart) ; RegReplaceCount is actually position
	if errorlevel
		MsgBox, 16, RegEx Error, %Errorlevel%
	if(outPutRegMode != "O") {
	i = 0
	; Merge arrays for convenience
		if(outPutRegMode != "P")
			while(i < sub, sub0:=a_index-1) {
				if(!Output%a_index%) {
					i++
					t:="Output" (OutputName%a_index%:=sub%i%)   ; Name
					Output%a_index%:=%t%	; Move value
				}
			}
		else 
			while(i < sub, sub0:=a_index-1) {
				if(!OutputLen%a_index%) {
					i++
					t:= "OutputLen" (OutputName%a_index%:=sub%i%)   ; Name
					OutputLen%a_index%:=%t%   ; Move Length
					t:= "OutputPos" sub%i%
					OutputPos%a_index% := %t%	; Move Position
				}
			}
	RegEndPos:=strlen(Output)+RegReplaceCount
	guiControl, main:  , RegMatch, % RegExReplace(Output, "`n", "``n", inlineFeedCount)
	selectHaystack(RegReplaceCount, Output)
	}
	else {
		RegEndPos:=strlen(Output[0])+RegReplaceCount
		guiControl, main:  , RegMatch, % RegExReplace(Output[0], "`n", "``n", inlineFeedCount)	
		selectHaystack(RegReplaceCount, Output[0])
	}
	guiControl, main:  , RegReplaceCount, %RegReplaceCount%
	IfWinExist, ahk_id %PatHWND%
		gosub, PatternDetails
}
LV_Updater()
return

; Updates the LV results
UpdateLV:
	gui, submit, nohide
	LV_Updater()
return

;  ============= Main List View =================================
LV_Updater() {
	global RegNeedle, RegHaystack, MainLV
	gui, main: default
	LV_Delete()
	startPos:=0
	outputMode:=getMatchMode(RegNeedle)
	performNeedle:=getPerformanceNeedle()
	while(startPos:=RegExMatch(RegHaystack, "`n" . performNeedle, output, startPos+1)) {
			if(outputMode = "O")
				output:=output[0]
			LV_Add(, a_index, startPos, output)
			startPos+=StrLen(output)
	}
return

; Finds the selected LV item in Haystack and highlights
LVClick:
	gui, main: default
	row:=LV_GetNext()
	LV_GetText(pos, row, 2)
	LV_GetText(match, row, 3)
	SelectHaystack(pos, match)
return

}
; ===============================================================
; Get Match Mode O - object, P - position, 1-Default
getMatchMode(needle) {
	if RegExMatch(needle, "^[\w`]*O[\w`]*\)") { ; Object RegMode
		return "O"
	}		
	If RegExMatch(needle, "^[\w`]*P[\w`]*\)")  
		return "P"
	return 1
}

; Returns Integer if n is integer, otherwise returns "Error"
validateInt(n) {
RegExMatch(n, "^-?\d+$", n)
	return (n = "") ? "Error" : n
}

; ===================================================================================================================
;	formatRegex - Purpose: Transform the regex (x) into ahk Text mode or to RegEx var mode. 
;		This allows users to type in `n as the needle RegEx without having problems.
; toText RegMode (toText:=true):
;		All new lines \n are converted to `n. Only linefeeds are supported, due to the nature of edit controls
; 		All quotes in a row are converted to 2 double quotes.
; 		To avoid this behavior, use a \ before a quote to preserve them exactly. The formatter won't touch them.
;		(ie. To replace the following: ""HelloWorld"" - Specify \"\"HelloWorld\"\" as the needle.)
; Variable RegMode (toText:=false):
; 		All ` are converted to \
;		Any double double-quotes are converted to a single double-quote. (Fun to say I know ;))
; 	Returns the formatted Regex (x)
; ====================================================================================================================
formatRegex(x, toText:=true) {
	if !x
		return
	StringReplace, x, x, % options:=getNeedleOptions(x)
	if (toText) {    ; Convert to a regex in AHK text form
		x:=options . RegExReplace(RegExReplace(RegExReplace(x, "\n", "``n"), "\t", "``t"), "(?<!\\)""+", """""") 
	} else {		; Convert to a regex variable form
		x:=options . RegExReplace(RegExReplace(x, "``", "\"), "(?<!\\)""+", """")
	}
	consoleLog("Formatted Regex " x (toText ? "`nText Form" : "`nRegEx Form ") "`nOptions: " options)
	;~ MsgBox, % x
	return x
}

; Grab RegEx-QuickReference
loadReference() {
	try{
	ie:=ComObjCreate("InternetExplorer.Application")
	ie.Navigate("https://autohotkey.com/docs/misc/RegEx-QuickRef.htm")
	while(ie.busy || ie.document.readyState !="complete")
		continue
		content:=ie.document.getElementByID("main-content").innerText
	} catch {
	}
	ie.Quit
	return content
}

; Thanks just me - https://autohotkey.com/boards/viewtopic.php?f=5&t=13743
; ======================================================================================================================
; Hides any balloon tip associated with an edit control.
; ======================================================================================================================
EM_HIDEBALLOONTIP(HWND) {
   ; EM_HIDEBALLOONTIP = 0x1504 -> msdn.microsoft.com/en-us/library/bb761604(v=vs.85).aspx
   Return DllCall("User32.dll\SendMessage", "Ptr", HWND, "UInt", 0x1504, "Ptr", 0, "Ptr", 0, "Ptr")
}
; ======================================================================================================================
; Displays a balloon tip associated with an edit control.
; Title  -  the title of the balloon tip.
; Text   -  the balloon tip text.
; Icon   -  the icon to associate with the balloon tip, one of the keys defined in Icons.
; ======================================================================================================================
EM_SHOWBALLOONTIP(HWND, Title, Text, Icon := 0) {
   ; EM_SHOWBALLOONTIP = 0x1503 -> http://msdn.microsoft.com/en-us/library/bb761668(v=vs.85).aspx
   Static Icons := {0: 0, 1: 1, 2: 2, 3: 3, NONE: 0, INFO: 1, WARNING: 2, ERROR: 3}
   NumPut(VarSetCapacity(EBT, 4 * A_PtrSize, 0), EBT, 0, "UInt")
   If !(A_IsUnicode) {
      VarSetCapacity(WTitle, StrLen(Title) * 4, 0)
      VarSetCapacity(WText, StrLen(Text) * 4, 0)
      StrPut(Title, &WTitle, "UTF-16")
      StrPut(Text, &WText, "UTF-16")
   }
   If !Icons.HasKey(Icon)
      Icon := 0
   NumPut(A_IsUnicode ? &Title : &WTitle, EBT, A_PtrSize, "Ptr")
   NumPut(A_IsUnicode ? &Text : &WText, EBT, A_PtrSize * 2, "Ptr")
   NumPut(Icons[Icon], EBT, A_PtrSize * 3, "UInt")
   Return DllCall("User32.dll\SendMessage", "Ptr", HWND, "UInt", 0x1503, "Ptr", 0, "Ptr", &EBT, "Ptr")
}

; Highlight the Haystack Match
SelectHaystack(start, match) {
	local lfCount, withinlfCount, cline
	if(match = "")
		return
	RegExReplace(substr(RegHaystack, 1, start), "`n",, lfCount)
	RegExReplace(match, "`n",, withinlfCount)
	ConsoleLog("Selecting Haystack Match: " match " LFcount: " lfCount)
	guicontrol, main: focus, RegHaystack
	SendMessage, 0xB1, % start+lfCount-1, % start+strlen(match)-1+lfCount+withinlfCount,, ahk_id %HaystackHnd% ; EM_SETSEL
	 SendMessage, 0xB7, 0, 0,, ahk_id %HaystackHnd% ; EM_SCROLLCARET
}

; ================================================================================================
; 	TOOLBOX Stuff
; ================================================================================================
; Toggles an option to the RegEx Needle
ToggleOption(option) {
	global RegNeedle
	guicontrolget, RegNeedle, main:, RegNeedle
	if((pos:=instr(RegNeedle, "(")) > (optParenth:=instr(RegNeedle, ")")) || !pos && optParenth) {
		options:= substr(RegNeedle, 1, optParenth)
		needle:=substr(RegNeedle, optParenth+1)
	}
	else
		needle:=RegNeedle
		;~ RegNeedle:= option ")" RegNeedle
	Msgbox % " options : " options "`n" 
	;~ if(option = "(*ANYCRLF)") {
			;~ (substr(needle, 1, 9) = "(ANYCRLF)") ? (needle:=substr(needle, 10)) : (needle:= option . needle)
	;~ }
	;~ else {
		if (options = "")
			options:= option . ")"
		else if (instr(options, option)) {  ; inside options already?
			StringReplace, options, options, %option%
			if(options = ")")
				options:=
		} else
			options:= option . options
	;~ }
	RegNeedle:=options . needle
	GuiControl, main:, RegNeedle, %RegNeedle%
	return
}

; ==========================================
; 	Show Tool Box
; 	Param show
;		-1 = Toggle
;		0 = Hide
;		1 = Show
; ==========================================
showToolBox(show=-1) ; Toggle
{
	global
	static isShown
	if(!isShown && show != 0){
		menu, ViewMenu, check, ToolBox
		gui, tb: show, x600 y240, ToolBox
		guicontrol, main: move, MatchLV, h200
		isShown:=true
	} else if (isShown && show < 1){
		menu, ViewMenu, UnCheck, ToolBox
		gui, tb: hide
		guicontrol, main: move, MatchLV, h430
		isShown:=false
	}
}

;  ToolTips
; https://www.autohotkey.com/docs/commands/Gui.htm#Examples
WM_MOUSEMOVE()
{
    static CurrControl, PrevControl
	if(a_gui != "tb")
		return
    CurrControl := A_GuiControl
    If (CurrControl <> PrevControl and not InStr(CurrControl, " "))
    {
        ToolTip  ; Turn off any previous tooltip.
        SetTimer, DisplayToolTip, -300
        PrevControl := CurrControl
    }
    return

    DisplayToolTip:
		; TAB 1 - COMMON
		ConsoleLog("Displaying Tooltip for " CurrControl)
		if(CurrControl = ".") {
		ToolTip % CurrControl " By default, a dot matches any single`n character which is not part of a newline (``r``n) sequence, but`n this can be changed by using the DotAll (s), linefeed (``n),`n carriage return (``r), ``a or (*ANYCRLF) options. For example,`n ab. matches abc and abz and ab_."
		} else if(CurrControl = "*") {
		ToolTip % CurrControl " An asterisk matches zero or more of`n the preceding character, class, or subpattern."
		} else if(CurrControl = "?") {
		ToolTip % CurrControl " A question mark matches zero or one of `nthe preceding character, class, or subpattern. Think of this as `n""the preceding item is optional""."
		} else if(CurrControl = "+") {
		ToolTip % CurrControl " A plus sign matches one or more of the `npreceding character, class, or subpattern."
		} else if(CurrControl = "{min,max}") {
		ToolTip	 % CurrControl " Matches between min and max`n occurrences of the preceding character, class, or subpattern. `nAlso, {3} means exactly 3 occurrences, and {3,} means 3 or more `noccurrences."
		} else if(CurrControl = "[abc]") {
		ToolTip % CurrControl " Classes of characters: The square`n brackets enclose a list or range of characters (or both)."
		} else if(CurrControl = "[^abc]") {
		ToolTip % CurrControl " Matches any single character that is not in the class."
		} else if(CurrControl = "\d") {
		ToolTip % CurrControl " Matches any single digit (equivalent`n to the class [0-9]). Conversely, capital \D means ""any`n non-digit"". This and the other two below can also be used inside`n a class; for example, [\d.-] means ""any single digit, period, `nor minus sign""."
		} else if(CurrControl = "\s") {
		ToolTip % CurrControl " Matches any single whitespace `ncharacter, mainly space, tab, and newline (``r and ``n).`n Conversely, capital \S means ""any non-whitespace character""."
		} else if(CurrControl = "\w") {
		ToolTip % CurrControl " Matches any single ""word"" character,`n namely alphanumeric or underscore. This is equivalent to`n [a-zA-Z0-9_]. Conversely, capital \W means ""any non-word`n character""."
		} else if(CurrControl = "^") {
		ToolTip % CurrControl " may appear at the beginning of a pattern to require`n the match to occur at the very beginning of a line."
		} else if(CurrControl = "$") {
		ToolTip % CurrControl " may appear at the end of a pattern `nto require the match to occur at the very end of a line."
		} else if(CurrControl = "\b") {
		ToolTip % CurrControl " means ""word boundary"", which is`n like an anchor because it doesn't consume any characters. It`n requires the current character's status as a word character (\w)`n to be the opposite of the previous character's."
		} else if(CurrControl = "|") {
		ToolTip % CurrControl " The vertical bar separates two or more alternatives.`n A match occurs if any of the alternatives is satisfied."
		} else if(CurrControl = "(...)") {
		ToolTip % CurrControl " Items enclosed in parentheses are most commonly used to:`nDetermine the order of evaluation.`nApply *, ?, +, or {min,max} to a series of characters rather than just one. `nCapture a subpattern such as the dot-star in abc(.*)xyz. `nChange options on-the-fly."
		} else if(CurrControl = "(?:...)") {
		ToolTip % CurrControl " Non-capturing group"
		} else if(CurrControl = "(?<...>)") {
		ToolTip % CurrControl " Named Sub-Pattern" 
		} else if(CurrControl = "\Q...\E") {
		ToolTip % "Escaping can be avoided by using \Q...\E.`nFor example: \QLiteral Text\E."
		}
		; TAB 3 - Assertions
		else if(CurrControl = "(?=..)") {
		ToolTip % CurrControl " is called a positive look-ahead because it requires that the specified pattern exist.`nThe groups (?=...), (?!...), (?<=...), and (?<!...) are called assertions because they demand a condition to be met but don't consume any characters."
		} else if(CurrControl = "(?!..)") {
		ToolTip % CurrControl " is a negative look-ahead `nbecause it requires that the specified pattern not exist."
		} else if(CurrControl = "(?<=..)") {
		ToolTip % CurrControl " is a positive look-behind`n because it looks to the the left of the current position.`nLook-behinds are more limited than look-aheads because they do`n not support quantifiers of varying size such as *, ?, and +."
		} else if(CurrControl = "(?<!..)") {
		ToolTip % CurrControl " is a negative look-behind that`n looks to the left of the current position."
		} else if(CurrControl = "\K") {
		ToolTip % CurrControl " The escape sequence \K is similar to a`n look-behind assertion because it causes any previously-matched `ncharacters to be omitted from the final matched string."
		} 
		; TAB 2 - Options
		else if(CurrControl = "i") {
			ToolTip % CurrControl " Case-insensitive matching"
		}else if(CurrControl = "m") {
			ToolTip % CurrControl " Multiline. Views Haystack as a collection of individual lines `n(if it contains newlines) rather than as a single continuous line."
		}else if(CurrControl = "s") {
			ToolTip % CurrControl " DotAll. This causes a period (.) to match all characters including`n newlines (normally, it does not match newlines)."
		}else if(CurrControl = "x") {
			ToolTip % CurrControl " Ignores whitespace characters in the pattern except when escaped `nor inside a character class."
		}else if(CurrControl = "A") {
			ToolTip % CurrControl " Forces the pattern to be anchored; that is, it can match only at `nthe start of Haystack."
		}else if(CurrControl = "D") {
			ToolTip % CurrControl " Forces dollar-sign ($) to match at the very end of Haystack, even `nif Haystack's last item is a newline."
		}else if(CurrControl = "J") {
			ToolTip % CurrControl " Allows duplicate named subpatterns. This can be `nuseful for patterns in which only one of a collection of `nidentically-named subpatterns can match."
		}else if(CurrControl = "U") {
			ToolTip % CurrControl " Ungreedy. Makes the quantifiers *+?{} consume only those `ncharacters absolutely necessary to form a match, leaving the `nremaining ones available for the next part of the pattern."
		}else if(CurrControl = "X") {
			ToolTip % CurrControl " PCRE_EXTRA. Enables PCRE features that are incompatible with Perl.`nCurrently, the only such feature is that any backslash `nin a pattern that is followed by a letter that has no special `nmeaning causes the match to fail and ErrorLevel to be set accordingly." 
		}else if(CurrControl = "P") {
			ToolTip % CurrControl " Position mode. This causes RegExMatch() to yield the `nposition and length of the match and its subpatterns rather than `ntheir matching substrings."
		}else if(CurrControl = "S") {
			ToolTip % CurrControl " Studies the pattern to try improve its performance. This is useful`nwhen a particular pattern (especially a complex one) will`nbe executed many times."
		}else if(CurrControl = "C") {
			ToolTip % CurrControl " Enables the auto-callout mode."
		}
		;~ else if(CurrControl = "``n") {
			;~ ToolTip % CurrControl " Switches from the default newline character (``r``n) to a solitary linefeed (``n)`n(Not Supported by RegExstar)"
		;~ }else if(CurrControl = "``r") {
			;~ ToolTip % CurrControl " Switches from the default newline character (``r``n) to a `nsolitary carriage return (``r).`n(Not Supported by RegExstar)"
		;~ }else if(CurrControl = "``a") {
			;~ ToolTip % CurrControl " In v1.0.46.06+, ``a recognizes any type of newline`n(Not Supported by RegExstar)"
		;~ }
    SetTimer, RemoveToolTip, -10000
    return

    RemoveToolTip:
    ToolTip
    return
}

; Add an item to the RegEx - (from ToolBox)
addToRegEx(item) {
	global RegNeedle, RegNeedleHWND
	cursorOffset:=0
	if(item = "{min,max}") {
		append:=RegExReplace(item, "[a-z]+")
		cursorOffset:=-2
	}
	else if(instr(item, "[")) {
		append:=RegExReplace(item, "[a-z]+")
		cursorOffset:=-1
	} else if(instr(item, "(")) { 
		StringReplace, append, item, .,, All
		if(instr(append, ">"))
			cursorOffset:=-2
		else
			cursorOffset:=-1
	} else if(item = "^") {
		anchor:=1
	} else if(item = "$") {
		anchor:=2
	} else if(item = "\Q...\E") {
		StringReplace, append, item, .,, All
		cursorOffset:=-2
	}
	if(append = "")
		append:=item ; default
	
	GuiControlGet, RegNeedle, main:, RegNeedle
	if(anchor = 1)  ; ^
	{
		if(opt:=getNeedleOptions(RegNeedle)) {
			if(instr(needle:=substr(RegNeedle, strlen(opt)+1), append) != 1)
				RegNeedle:= opt . append . needle
			else
				return
		}
		else if(instr(RegNeedle, append) != 1) 
			RegNeedle:= append . RegNeedle
		else 
			return
	}
	else if(anchor = 2 && instr(RegNeedle, append,, strlen(RegNeedle)-1))
		return
	 else
		RegNeedle := RegNeedle . append
	consoleLog("Options: " opt " Anchor: " anchor)
	guicontrol, main:, RegNeedle, %RegNeedle%
	guicontrol, main: focus, RegNeedle
	SendMessage, 0xB1, % pos:=(cursorOffset <= 0) ? strlen(RegNeedle)+cursorOffset : cursorOffset, pos,, ahk_id %RegNeedleHWND% ; EM_SETSEL	
	consoleLog("Set RegNeedle " RegNeedle " Appended: " append " CursorOffset was " cursorOffset)
}
; ==========================================================================================

; Needle options
getNeedleOptions(needle) {
	if((pos:=instr(needle, "(")) > (optParenth:=instr(needle, ")")) || !pos && optParenth) 
		return substr(needle, 1, optParenth)
}

; Usable needle - formats for use with "`n" option since edit controls have `n LineFeeds
getPerformanceNeedle() {
	local options, pos
	if(pos:=instr(options:=getNeedleOptions(RegNeedle), "``")) {
		MsgBox, 16, Invalid Option, % "Option " substr(options, pos, 2) " is not supported."
		exit
	}
	if(options = "")
		return  ")" RegNeedle
	return RegNeedle
}