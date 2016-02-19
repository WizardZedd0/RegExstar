# RegExstar
An AHK Tool for Regular Expressions.
Title  RegExstar
Author  WizardZedd
Version  0.9.3
Released  2/10/2016

  RegExstar 
	Performs quick RegEx Operations.
	Store your Commonly used RegEx's in one place.
	Create and test new RegEx's.

# Main Features 
	RegexReplace Mode:
		Based on RegExReplace Command. (See https://autohotkey.com/docs/commands/RegExReplace.htm)
		Perform a replacement.
 		Click on count to count the RegEx matches without replacements.
		Undo Option.
	RegexMatch Mode:
		Based on RegExMatch Command. (See https://autohotkey.com/docs/commands/RegExMatch.htm)
		Captures Subpatterns, which can be seen by clicking the "Pattern Details" Button or in the View Menu.
		Highlighting of Current Match
		Next Match button, to quickly find the next match (^f).
	Match List:
		See all the matches in the Match ListBox in either mode.
		Double click on a match to highlight it in the haystack.
   	Debugger:
	       	Step through your RegEx and see exactly what the pattern is doing.		

# Other Features 
	Storage of your favorite RegEx Needles.
	Pick up where you left off ini file.
	Examples of Regular Expressions.
	Copy Code - Copies the command to the clipboard.
 	Conversion between text and var regex. (` to \, "" to ") 
	Toolbox of Regular Expressions

# Pitfalls
	Due to the nature of edit controls all newlines are interpreted as `n or \n. 
	Accordingly, RegExstar always uses the `n option in order to support the multiline option, 
	and does not provide support for other newline options such as `a, `r, etc. 
	However, when actually using the code one must be aware of what type of newline they are dealing with. 
	The multiline option or the \n may perform differently inside of other applications than they would inside of RegExstar.

# GUI 
 To open Gui, 
 	Double Click on Tray OR
 	control-shift-r copies selected text if available, otherwise uses whatever is on the clipboard 
 	as haystack and opens Gui.

#  Gui Hotkeys 
 	^c: Copies RegEx code to clipboard if nothing is selected
 	^o: Open A RegEx File
 	^s: Save A RegEx - 	These files currently only store the chosen name and the needle of the RegEx 
 	^f: Next Match in Match mode
 	f1: Opens RegEx-Quick Reference Docs
 	f5: Open Debugger
	-And of course many of the alt codes
 
