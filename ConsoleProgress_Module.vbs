' Progress Monitor - Console Edition
' Author: Rob Pascoe
' Date: 05092018
' script adapted from work by Concept211
' at url:
'https://stackoverflow.com/questions/18216027/vbscript-script-progress-notification

			' calls the function below to force the interpreter to be CScript. This allows the console to be easily presented to a user.
			' (without interrupting with MsgBoxes or it seeming like the script has broken!)
			
ForceConsole()

dim str_progtitle
dim str_section1
dim str_completemessage

'=========================================================================================================
' Tools available 
'=========================================================================================================

'---------------------------------------------------------------------------------------------------------
' WScript.Echo
'---------------------------------------------------------------------------------------------------------
' Simply use WScript.Echo to write Section start/complete markers and to provide supporting information (drawing in variables, if necessary!)

'EXAMPLE: 
str_progtitle = "Process Completion:"
WScript.Echo str_progtitle 		' can be freely customised.

'   note, unlike in deprecated browser-based solutions, the console method does not support real-time update of anything other than the CURRENT line. 
'  we cannot go back and update a heading/section name above the progress bar. 
' starting a new progress bar when moving between sections is advised. 

'---------------------------------------------------------------------------------------------------------
' Progress(i, imax)
'---------------------------------------------------------------------------------------------------------
' Progress writes a progress bar to the CScript console. 
' Use this whenever a batch process of known size is iterated across. 
' Prefacing with a sensible title using WScript.Echo will help a user understand exactly what is 'progressing'.
' A title field has not been built into the function as CScript cannot easily overwrite itself.
' There are workarounds for this, but it is reccommended that a user start a new progress bar if a section changes...

		
' EXAMPLE 1 - demonstration of multiple progress bars
'Wscript.Echo str_section1
'For i = 1 To 100
'   Call progress(i, 100)
 
'				WScript.Sleep(30)
'Next

'WScript.Echo chr(10) & chr(13)

'str_section2 = "Section 2... "
'Wscript.Echo str_section2
'For i = 1 To 100
'   Call progress(i, 100)
 
'				WScript.Sleep(30)
'Next
'WScript.Echo chr(10) & chr(13)

'---------------------------------------------------------------------------------------------------------

' EXAMPLE 2 -  clear demonstration of exactly how progress() function works (with and without line break)
 '  Call progress(50, 100)
 '  				WScript.Sleep(1000)
 '     Call progress(50, 50)
 '  				WScript.Sleep(1000)	  
				
				
'WScript.Echo chr(10) & chr(13)
				
'	     Call progress(5, 25)
'   				WScript.Sleep(1000)		 
'		    Call progress(50, 200)
'   				WScript.Sleep(1000)

'----------------------------------------------------------------------------------------------------------				
				
				
				' close down console
'---------------------------------------------------------------------------------------------------------
' Closing Statements 
'---------------------------------------------------------------------------------------------------------				
' The console is not an ideal venue for crucial notifications for a user, as they may pass them by
' However, it feels more natural when the console informs the user when the code has run.
' Again... supporting information could be added here via variables. 



				WScript.Echo chr(10) & chr(13) 
				WScript.Echo "Processing Complete!"
 				WScript.Sleep(4000)
				
				


' End of Main Script 
' -----------------------------------------------------------------------------------

'=========================================================================================================
' Functions and Subs
'=========================================================================================================
Function printi(txt)
    WScript.StdOut.Write txt
End Function    

Function printr(txt)
    back(Len(txt))
    printi txt
End Function

Function back(n)
    Dim i
    For i = 1 To n
        printi chr(08)
    Next
End Function   

Function percent(x, y, d)
    percent = FormatNumber((x / y) * 100, d) & "%"
End Function

Function progress(x, y)
    Dim intLen, strPer, intPer, intProg, intCont
    intLen  = 22
    strPer  = percent(x, y, 1)
    intPer  = FormatNumber(Replace(strPer, "%", ""), 0)
    intProg = intLen * (intPer / 100)
    intCont = intLen - intProg
    printr String(intProg, ChrW(9608)) & String(intCont, ChrW(9618)) & " " & strPer
End Function

Function ForceConsole()
    Set oWSH = CreateObject("WScript.Shell")
    vbsInterpreter = "cscript.exe"

    If InStr(LCase(WScript.FullName), vbsInterpreter) = 0 Then
        oWSH.Run vbsInterpreter & " //NoLogo " & Chr(34) & WScript.ScriptFullName & Chr(34)
        WScript.Quit
    End If
End Function