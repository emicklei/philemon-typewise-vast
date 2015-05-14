"Philemon SmalltalkMT exporter has done all the work
Version PhilemonTerminalViewTest EM 5-0507"
PROJECTNAME PhilemonTerminalViewTest .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonTerminalClient .
POOLS TerminalConstants TerminalCharacterConstants .

PROFILE
BEGIN
END

CLASS TerminalTextEditor
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	open
END
INSTANCEMETHODS
BEGIN
	helpMenu
	viewMenu
	aboutText
	aboutDialogWindow
	text
	showAboutDialog
	terminal
	buildWindow
	open
	terminal:
	fileMenu
	findMenu
	editMenu
	menuApearance
	menuBar
END

CLASS TerminalTests
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	testCheckBox1
	testWordWrap6
	testScreen2x2
	testListFocus
	testInputFormPanel
	testScreenPanelButton
	testScreenInsertAfterTextReplace
	testScreenPanelButton3
	testScreenUnity
	testListViewSingleLine
	screen2x2
	testInput
	testFullScreenSpeed2
	testEditDeleteChar3
	testDamageInOutside4
	testShowScreenWithInputBeforeOpen
	testTextLinesFromTab
	testGetSetCharacter
	testEditInsertChar
	testCharactersInBounds
	setUp
	testGetSet2
	testEmptyDropDown
	testTextFromLines2
	testFullScreenSpeed
	testWordWrap3
	testCharacterSet
	testScreenScrollingText
	testEditDeleteChar
	testEditDeleteChar2
	testScreenMenuBar
	testTextFromLines
	testDamageInOutside1
	testBigWindow
	testScreenFunctionKeys
	testRadioButton1
	source1
	testSize
	testLabel1
	testEditInsertChar3
	testEditDeleteCR
	testList
	testScreenTabTest
	testEditInsertChar10
	testList2
	testFullScreenSpeedProfile
	screen2
	testEditInsertChar11
	testEditBackspace
	testScreenNonFittingText
	testDropDownOverlap
	testWordWrap8
	screen1
	testEditDeleteChar1
	testWordWrap7
	tearDown
	testScreenTableListNoRows
	testFocusJumpOnTab
	testDamageInOutside2
	testTextFromLinesCR
	keySelectors
	testScreenDropDown
	testWordWrap5
	testDamageInOutside3
	testShowDialogNoDelay
	testListScrolling
	testScreen2x2_input
	testScreen
	processInputFormData:
	testGetSet
	testScreenPanelButton2
	testShowScreenAfterOpen
	testEditBackspace3
	testEditBackspace2
	testTextOffset
	testGraphics
	screenListFull
	testWordWrap2
	testScreenTableList
	screenFull
	testMenuBar
	testWordWrap4
	inputFormPanel
	testButton
	testShowScreenSwap
	close:
	testTextLinesFrom
	dialog1
	testScreenTableListEditable
	test256Colors
	testMenu
	shoot:
	doTestEdit:
	dialogIn:text:
	testEditInsertChar4
	testInputSetString
	testEditInsertChar2
	methods1
	testWordWrap1
	testScreenMultiLineTest
	testScreenBlackWhite
	testTextTwice
	testRadioGroup
	testScreenPanelButton4
	testScreenBlackWhiteByMessage
	addToMenuBar:
	testScreenHuge
	testOneItemDropDown
	testFileList3
END

CLASS WidgetDirtyTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	testBounds
	testCheckBox
	testVisible
	testAppearance
	testImage
	testList
	testInput
END

CLASS TerminalGraphicalObject
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	inspectActions
END

CLASS IndexedMessageSend
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	printOn:
END

CLASS TerminalWindow
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	inspectActions
END

! !
Compiler poolAdd: 'TerminalConstants' value: (StringDictionary new 
 at: 'TMAppearanceGet' put: 2;
 at: 'TMURLGet' put: 16384;
 at: 'TMTitleSet' put: 65537;
 at: 'TMClearAllDirty' put: 80;
 at: 'TMRemove' put: 70;
 at: 'TMAlignmentGet' put: 4096;
 at: 'TMCountSet' put: 257;
 at: 'TMEnabledGet' put: 16;
 at: 'TMOpenURL' put: 78;
 at: 'TMSetFocus' put: 82;
 at: 'TMItemsGet' put: 64;
 at: 'TMShowDialog' put: 81;
 at: 'TMSelectionSet' put: 33;
 at: 'TMInstallMessageOnEvent' put: 75;
 at: 'TMTickAlignmentGet' put: 2097152;
 at: 'TMBoundsSet' put: 9;
 at: 'TMVisibleSet' put: 5;
 at: 'TMShow' put: 69;
 at: 'TMItemGet' put: 262144;
 at: 'TMSelectionGet' put: 32;
 at: 'TECounted' put: 8;
 at: 'TMItemsSet' put: 65;
 at: 'TMRequest' put: 79;
 at: 'TMOpenHelp' put: 77;
 at: 'TMDelaySet' put: 513;
 at: 'TMURLSet' put: 16385;
 at: 'LF' put: $
;
 at: 'TECounterEnd' put: 6;
 at: 'TAB' put: $	;
 at: 'CR' put: $;
 at: 'TMScaleToFitSet' put: 32769;
 at: 'TMStringSet' put: 2049;
 at: 'TMMessageForShowingScreenAccessedBy' put: 67;
 at: 'TMIntervalSet' put: 524289;
 at: 'TMAppearanceSet' put: 3;
 at: 'TMRepeatGet' put: 1024;
 at: 'TEClicked' put: 4;
 at: 'TMDelayGet' put: 512;
 at: 'TMSubmit' put: 66;
 at: 'TMYourself' put: 76;
 at: 'TMBoundsGet' put: 8;
 at: 'TMEditableGet' put: 8192;
 at: 'TMSelectedItemGet' put: 128;
 at: 'SPACE' put: $ ;
 at: 'TMSelectionIndexSet' put: 1048577;
 at: 'TMMessageForShowingScreenAccessedByWith' put: 68;
 at: 'TELosingFocus' put: 2;
 at: 'TMSelectedItemSet' put: 129;
 at: 'TMVisibleGet' put: 4;
 at: 'TMEditableSet' put: 8193;
 at: 'TMSelectionIndexGet' put: 1048576;
 at: 'TMTickAlignmentSet' put: 2097153;
 at: 'TMStringGet' put: 2048;
 at: 'TMRepeatSet' put: 1025;
 at: 'TMTitleGet' put: 65536;
 at: 'TMEnabledSet' put: 17;
 at: 'TESelectionIsValid' put: 3;
 at: 'TESelectedItem' put: 1;
 at: 'TMItemSet' put: 262145;
 at: 'TEGettingFocus' put: 0;
 at: 'TMCountGet' put: 256;
 at: 'TMScaleToFitGet' put: 32768;
 at: 'TMClose' put: 74;
 at: 'TMHandleError' put: 73;
 at: 'TMMessageForAddingCenteredWidgetAccessedBy' put: 72;
 at: 'TMMessageForAddingWidgetAccessedBy' put: 71;
 at: 'TMAlignmentSet' put: 4097;
yourself).Compiler poolAdd: 'TerminalCharacterConstants' value: (StringDictionary new 
 at: 'TCHasFocus' put: 1048576;
 at: 'TCTopLeft' put: 1024;
 at: 'TCSelect' put: 4;
 at: 'TCTop' put: 32;
 at: 'TCLeft' put: 16;
 at: 'TCBottomRight' put: 8192;
 at: 'TCChoiceMarker' put: 2097152;
 at: 'TCBottom' put: 128;
 at: 'TCCheckTick' put: 262144;
 at: 'TCRadioCircle' put: 131072;
 at: 'TCCheckBox' put: 524288;
 at: 'TCUnderline' put: 32768;
 at: 'TCRadioTick' put: 65536;
 at: 'TCGrid' put: 2;
 at: 'TCHorizontal' put: 512;
 at: 'TCBottomLeft' put: 4096;
 at: 'TCInsertMode' put: 16384;
 at: 'TCTopRight' put: 2048;
 at: 'TCRight' put: 64;
 at: 'SpaceString' put: ' ';
 at: 'TCHighlight' put: 1;
 at: 'TCVertical' put: 256;
 at: 'TCReturn' put: 8;
yourself).
!

Object subclass: #TerminalTextEditor
    instanceVariableNames: 'terminal '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TestCase subclass: #TerminalTests
    instanceVariableNames: 'grid '
    classVariableNames: 'Close '
    poolDictionaries: 'TerminalConstants TerminalCharacterConstants '
	comment: '' 
	category: '' !
TestCase subclass: #WidgetDirtyTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: 'TerminalConstants '
	comment: '' 
	category: '' !	

!TerminalTextEditor class publicMethods !

open
	self new open! !

!TerminalTextEditor publicMethods !

aboutDialogWindow

	| window |
	window := TerminalWidget windowClass in: (5@10 corner: 11@36).
	window add: self aboutText.
	^window!

aboutText

	| text  app|
	text := TerminalWidget textClass in: (5@10 corner: 11@36).
	text name: 'text'.
	(text appearance) 
		background: Color gray
		;foreground: Color black.
	text string: '
	TerminalTextEditor
	
	version 2002-08-23
	written by E.Micklei
	(c) PhilemonWorks'.
	
	^text
	!

buildWindow

	| window |
	window := TerminalWidget windowClass in: (1@1 corner: 24@64).
	window 
		add: self menuBar
		;add: self text.
	^window!

editMenu

	| menu |
	menu := TerminalWidget menuClass new.
	menu appearance: self menuApearance.
	menu 
		add:  ' Cut    Ctrl+X ' key: $x message: nil
		;add: ' Copy   Ctrl+C ' key: $c message: nil
		;add: ' Paste  Ctrl+V ' key:$v message: nil
		;add: ' Delete    DEL ' key:$d message: nil.
	^menu!

fileMenu

	| menu |
	menu := TerminalWidget menuClass new.
	menu appearance: self menuApearance.
	menu labelWidth: 14.
	menu 
		add: ' New' key: $n message: nil
		;add: ' Open...' key: $o message: nil
		;add: ' Save' key:$s message: nil
		;add: ' Save As...' key:$s message: nil
		;add: ' Close' key:$c message: nil
		;addLine
		;add: ' Print...' key:$p message: nil
		;addLine
		;add: ' Exit' key:$x message: (#close asMessageTo:terminal).
	^menu!

findMenu

	| menu |
	menu := TerminalWidget menuClass new.
	menu appearance: self menuApearance.
	menu 
		add:  ' Find...' key: $z message: nil
		;add: ' Find Repeat  F3 ' key: $f message: nil
		;add: ' Replace...' key:$V message: nil.
	^menu!

helpMenu

	| menu |
	menu := TerminalWidget menuClass new.
	menu appearance: self menuApearance.
	menu 
		add:  ' Key Control ' key: $s message: nil
		;add: ' About... ' key: $f message: (#showAboutDialog asMessageTo: self).
	^menu!

menuApearance
	^(TWidgetAppearance default copy)
		foreground: Color black;
		background: Color white;
		selectionForeground: Color gray;
		selectionBackground: Color black!

menuBar

	| menuBar |
	menuBar := TerminalWidget menuBarClass in: (1@1 corner: 1@64).
	menuBar appearance: self menuApearance.
	menuBar
		add: ' File ' key: $f menu: self fileMenu
		;add: ' Edit ' key: $e menu: self editMenu
		;add: ' Search ' key: $s menu: self findMenu
		;add: ' View ' key: $v menu: self viewMenu
		;add: ' Help ' key: $h menu: self helpMenu.		
	^menuBar!

open
	"DoitTool open:'TerminalTextEditor new open' " 
	
	terminal := Terminal show: self buildWindow!

showAboutDialog

	| aboutDialog |
	aboutDialog := self aboutText.
	terminal show: self aboutDialogWindow.!

terminal
	^terminal!

terminal: anObject
	terminal := anObject!

text

	| text |
	text := TerminalWidget textClass in:(2@1 corner: 24@64).
	(text appearance)
	 	background: Color navy.
	^text
	!

viewMenu

	| menu |
	menu := TerminalWidget menuClass new.
	menu appearance: self menuApearance.
	menu 
		add:  ' Split window  Ctrl+F6 ' key: $s message: nil
		;add: ' Change size   Ctrl+F8 ' key: $f message: nil
		;add: ' Close window  Ctrl+F4 ' key:$V message: nil
		;addLine
		;add: ' 1 Unnamed File  Alt+1 ' key:$1 message: nil.
	^menu! !
	

!TerminalTests publicMethods !

addToMenuBar: menuBar

	|   menu1 menu2 menu3|
	(menuBar appearance)
	 	foreground: Color black
		; background: Color lightGray
		; selectionBackground: Color black
		; selectionForeground: Color lightGray.

	menu1 := TerminalWidget menuClass new.
	menu1 name: 'menu1'.
	menu1 add: ' New ' key: $N message: (#value asMessageTo: [1.1 echo]).	
	menu1 add: ' Open... ' key: $O message: (#value asMessageTo: [1.2 echo]).		
	menu1 add: ' Save ' key: $S message: (#value asMessageTo: [1.2 echo]).		
	menu1 addLine.
	menu1 add: ' Exit ' key: $X message: (#value asMessageTo: [menuBar form close]).				
	menu1 appearance: menuBar appearance.
	menuBar add: ' File ' key: $F menu: menu1.

	menu2 := TerminalWidget menuClass new.
	menu2 name: 'menu2'.
	menu2 add: ' Cut ' key: $X message: (#value asMessageTo: [2.1 echo]).	
	menu2 add: ' Copy ' key: $C message: (#value asMessageTo: [2.2 echo]).		
	menu2 add: ' Paste ' key: $V message: (#value asMessageTo: [2.3 echo]).			
	menu2 appearance: menuBar appearance.
	menuBar add: ' Edit ' key: $E menu: menu2.
	
	menu3 := TerminalWidget menuClass new.
	menu3 name: 'menu3'.	
	menu3 add: ' About ' key: $A message: (#value asMessageTo: [
		| dia |
		dia := self dialogIn: (3@3 extent: 2@17) text: '
  Go Smalltalk !!' .
		menuBar topWidget add: dia.
		dia show.
		(Delay forSeconds:1) wait.
		menuBar topWidget remove: dia ]).		
	menu3 appearance: menuBar appearance.
	menuBar add: ' Help ' key: $H menu: menu3.	

	^menuBar!

close: aTerminal
	"Close := true"
	
	"Close := false"
	
	aTerminal title: self class name , '>>' , self selector.
	Close ~~ false ifTrue: [aTerminal close]!

dialog1
	^self dialogIn: (4 @ 4 extent: 0 @ 31)
		text: '< How can this be Smalltalk ?  >'!

dialogIn: aRectangle text: aText 
	| input  dialog |
	input := TerminalWidget textClass in: aRectangle.
	(input appearance)
		foreground: Color yellow;
		background: Color black.
	input string: aText.
	dialog := TerminalWidget windowClass in: input bounds.
	dialog add: input.
	^dialog!

doTestEdit: oneArgBlock 
	| t s txt |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 80).
	txt := TerminalWidget textClass in: (1 @ 1 corner: 3 @ 11).
	txt appearance background: Color blue.
	s add: txt.
	t := Terminal show: s.
	oneArgBlock value: txt.
	self close: t!

inputFormPanel
	| form label field button dropdown |
	form := TerminalWidget panelIn: (3 @ 3 extent: 20 @ 30).
	form background: Color blue.
	form name: 'inputFormPanel'.
	label := TerminalWidget labelIn: (4 @ 4 extent: 0 @ 8).
	label background: Color navy.
	label name: 'label1'.
	label string: 'Integer'.
	form add: label.
	field := TerminalWidget inputIn: (4 @ 13 extent: 0 @ 11).
	field
		background: Color white;
		foreground: Color black.
	field name: 'field1'.
	field string: '42'.
	form add: field.
	label := TerminalWidget labelIn: (5 @ 4 extent: 0 @ 8).
	label background: Color navy.
	label name: 'label2'.
	label string: 'String'.
	form add: label.
	field := TerminalWidget inputIn: (5 @ 13 extent: 0 @ 11).
	field
		background: Color white;
		foreground: Color black.
	field name: 'field2'.
	field string: 'Hello'.
	form add: field.
	label := TerminalWidget labelIn: (6 @ 4 extent: 0 @ 8).
	label background: Color navy.
	label name: 'label3'.
	label string: 'Float'.
	form add: label.
	field := TerminalWidget inputIn: (6 @ 13 extent: 0 @ 11).
	field
		background: Color white;
		foreground: Color black.
	field name: 'field3'.
	field string: '3.14159'.
	form add: field.
	label := TerminalWidget labelIn: (7 @ 4 extent: 0 @ 8).
	label background: Color navy.
	label name: 'label4'.
	label string: 'Date'.
	form add: label.
	field := TerminalWidget inputIn: (7 @ 13 extent: 0 @ 11).
	field
		background: Color white;
		foreground: Color black.
	field name: 'field4'.
	field string: '20-11-1967'.
	form add: field.
	label := TerminalWidget labelIn: (8 @ 4 extent: 0 @ 8).
	label background: Color navy.
	label name: 'label5'.
	label string: 'Choose'.
	form add: label.
	dropdown := TerminalWidget dropDownListIn: (8 @ 13 extent: 0 @ 11).
	dropdown
		background: Color white;
		foreground: Color black.
	dropdown maximumVisibleRows: 4.
	dropdown name: 'dropdown'.
	dropdown 
		items: #('monday' 'tuesday' 'wednesday' 'thursday' 'friday' 'saturday' 'sunday').
	form add: dropdown.
	button := TerminalWidget buttonIn: (10 @ 5 extent: 0 @ 5).
	button
		background: Color black;
		foreground: Color white.
	button name: 'ok'.
	button label: 'OK'.
	button 
		when: TWidgetEvent clicked
		send: #value
		to: [self processInputFormData: form inputNamesAndValues].
	form add: button.
	button := TerminalWidget buttonIn: (10 @ 14 extent: 0 @ 7).
	button
		background: Color black;
		foreground: Color white.
	button name: 'cancel'.
	button label: 'CANCEL'.
	button 
		when: TWidgetEvent clicked
		send: #inspect
		to: self.
	form add: button.
	^form!

keySelectors

	^TerminalBasicController info selectorsCategorizedIn: #( 'control-keys') all: true!

methods1
	| methods |
	methods := TerminalList in:(2@61 corner: 20@120).
	methods name: 'methods'.
	methods appearance background: Color navy.
	methods items: #().
	^methods!

processInputFormData: nameValueCollectionString
	nameValueCollectionString inspect!

screen1
	| s i |
	s := TerminalWidget windowClass in: (1 @ 1 corner: 40 @ 80).
	s add: (i := TerminalWidget inputIn: (2 @ 2 corner: 2 @ 32)).
	i name: 'input1'.
	i appearance background: Color brown.
	i string: '[Input for 32 normal characters]'.
	^s!

screen2
	| s i |
	s := TerminalWidget windowClass in: (1 @ 1 corner: 20 @ 40).
	s add: (i := TerminalWidget inputIn: (2 @ 2 extent: 4 @ 32)).
	i name: 'input2'.
	i appearance background: Color orange.
	i 
		string: '[ Input for 128  characters   ]
[ Input for  128  characters  ]
[ Input for  128  characters  ]
[ Input for  128  characters]'.
	^s!

screen2x2
	| s i i1 i2 i3 i4 |
	s := TerminalWidget windowClass in: (1 @ 1 corner: 40 @ 80).
	i1 := TerminalWidget textIn: (2 @ 2 corner: 10 @ 10).
	i3 := TerminalWidget textIn: (12 @ 2 corner: 20 @ 10).
	i2 := TerminalWidget textIn: (2 @ 12 corner: 10 @ 20).
	i4 := TerminalWidget textIn: (12 @ 12 corner: 20 @ 20).
	s add: i1.
	s add: i2.
	s add: i3.
	s add: i4.
	i1 appearance background: Color gray.
	i2 appearance background: Color green.
	i3 appearance background: Color orange.
	i4 appearance background: Color blue.
	i1 name: 'text1'.
	i2 name: 'text2'.
	i3 name: 'text3'.
	i4 name: 'text4'.
	^s!

screenFull
	| s i |
	s := TerminalWidget windowClass in: (1 @ 1 corner: 40 @ 80).
	s add: (i := TerminalWidget textIn: (1 @ 1 corner: 40 @ 80)).
	i name: 'text'.
	i appearance background: Color brown.
	^s!

screenListFull
	| s i |
	s := TerminalWidget windowClass in: (1 @ 1 corner: 40 @ 80).
	s add: (i := TerminalList in: (1 @ 1 corner: 40 @ 80)).
	i name: 'list'.
	i appearance background: Color brown.
	^s!

setUp
	"CwFontPrompter new prompt"
	
	grid := TerminalCharacterGrid rows: 20 columns: 10.
	"TerminalObjectAppearance allSubclassesDo:[ :each | each release ]."

"	(TerminalWindowAppearance default fontMetrics) 
		fontName:  '-microsoft-lucida console-medium-r-normal--13-100-96-96-m-0-iso8859-1'
		;height: 15
		;width: 8
		;leftOffset: 0
		;bottomOffset: -4.

	(TerminalWindowAppearance default fontMetrics) 
		fontName:  '-microsoft-courier new ce-medium-r-normal--11-80-96-96-m-0-microsoft-other'
		;leftOffset: 1
		;bottomOffset: -3.
				
	(TerminalWindowAppearance default fontMetrics) 
		 fontName:  '-microsoft-hvraster-medium-r-normal--12-90-96-96-m-80-microsoft-oem'
		;width: 9
		;height: 14
		;leftOffset: 0
		;bottomOffset: -4.
				
	(TerminalWindowAppearance default fontMetrics) 
		 fontName:  '-microsoft-fixedsys-medium-r-normal--12-90-96-96-m-80-iso8859-1'
		;width: 9
		;height: 16
		;leftOffset: 1
		;bottomOffset: -4.
	
	(TerminalWindowAppearance default fontMetrics) 
		fontName:   '-microsoft-terminal-medium-r-normal--18-140-96-96-m-100-microsoft-oem'
		;leftOffset: 1
		;bottomOffset: -3.
		
	(TerminalWindowAppearance default fontMetrics) 
		fontName: '-microsoft-hvraster-medium-r-normal--17-130-96-96-m-80-microsoft-oem'
		;height: 18
		;leftOffset: 0
		;bottomOffset: -4.		
"
!

shoot: aController

	| r  k| 
	r := EsRandom new.
	k := OrderedCollection new.
	10 timesRepeat:[
		self keySelectors do:[ :s |
			r next > 0.5 ifTrue: [ k addFirst: s ] ifFalse: [ k addLast: s ]]].
	k do:[ :e | e last = $: ifFalse:[aController perform: e ]]!

source1
	| source |
	source := TerminalWidget textIn:(22 @1 corner: 40 @120).
	(source appearance) 
		background: Color navy
		;selectionForeground: Color black
		;selectionBackground: Color white.
	^source!

tearDown
	grid release!

test256Colors

	| t  s x image|
	s := TerminalWidget windowIn: (1 @ 1 extent: 16 @ 16).
	1 to: 16 do:[ :r |
		1 to: 16 do:[ :c |
			| w |
			w := TerminalWidget labelIn: (r @ c extent: 0@0).
			w appearance background: (Color red: r + c * 8 green: r + c * 8 blue: r + c * 8 ).
			w string: 'X'.
			s add: w ]].
	t 	:= Terminal show: s.
	self close: t!

testBigWindow
	"DoitTool open:'TerminalTests run: #testBigWindow'
	"
	| t  s txt text1|
	s := TerminalWidget windowIn: (1 @ 1 extent: 55 @ 174).
	txt := '1234567890'.
	s add:(text1 := TerminalWidget textIn: (1@1 extent: 55@174)).
"	#EsbSampler asClass notNil
		ifTrue:[(#EsbSampler asClass spyOn:[t show: s]) browse].
"
	t := Terminal show: s.
	450 timesRepeat:[text1 paste: txt].
	self close: t!

testButton
	| t s i list1 txt lbl but |
	t := Terminal new.
	t openWidget.
	s := TerminalWidget windowClass in: (1 @ 1 extent: 24 @ 40).
	but := TerminalWidget buttonIn: (9 @ 10 extent: 0 @ 10).
	but appearance background: Color gray darker.
	but label: 'Ok'.
	but help: 'This is a OK'.
	but accelerator: $O.
	but 
		when: TWidgetEvent clicked
		send: #label:
		to: but
		with: 'Applying'.
	s add: but.
	but := TerminalWidget buttonIn: (9 @ 22 extent: 0 @ 10).
	but appearance background: Color gray darker.
	but label: 'Cancel'.
	but accelerator: $C.
	but default: true.
	but help: 'This is a CANCEL'.
	but 
		when: TWidgetEvent clicked
		send: #label:
		to: but
		with: 'Closing'.
	s add: but.
	t show: s.
	self close: t!

testCharacterSet
	"DoitTool open:'TerminalTests run: #testCharacterSet'
	"
	| t  s x image label txt label1 label2 label3|
	s := TerminalWidget windowIn: (1 @ 1 extent: 16 @ 90).
	txt := '1234567890abcdefghijklmopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ~!!@#$%^&*()_+:;"''<>?/.,`\|'.
	s add:(label1 := TerminalWidget labelIn: (2@2 corner: 2@88)).
	label1 string: txt.
	s add:(label2 := TerminalWidget labelIn: (3@2 corner: 3@88)).
	label2 string: txt.
	label2 appearance background: Color blue.
	s add:(label3 := TerminalWidget labelIn: (4@2 corner: 4@88)).
	label3 string: txt.
	t := Terminal show: s.
	self close: t!

testCharactersInBounds

	grid charactersInBounds: (1@1 corner: grid rows@grid columns) do: [ :tc | tc character: $Z ].!

testCheckBox1
	| t s list  c1 c2|
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 20 ).
	c1 := TerminalCheckBox in: (2@2 extent: 0@10).
	c1 string: 'Pick me'.
	c1 selection: true.
	s add: c1.
	
	c2 := TerminalCheckBox in: (4@2 extent: 0@10).
	c2 alignment: 3. "right"
	c2 tickAlignment: 1. "left"
	c2 string: 'Pick me'.
	c2 selection: true.
	s add: c2.	
	
	t := Terminal show: s.
	self close: t!

testDamageInOutside1

	| w  outer inner areas|
	w := TerminalWidget new.
	outer := 1@1 corner: 24@64.
	inner := 2@2 corner: 23@63.
	areas := w damageIn: outer outside: inner.
	self assert: (areas at: 1) = (1@1 corner: 1@64).
	self assert: (areas at: 2) = (2@1 corner: 24@1).
	self assert: (areas at: 3) = (24@2 corner: 24@64).
	self assert: (areas at: 4) = (2@64 corner: 23@64).			!

testDamageInOutside2

	| w  outer inner areas|
	w := TerminalWidget new.
	outer := 1@1 corner: 20@20.
	inner := 1@1 corner: 20@20.
	areas := w damageIn: outer outside: inner.
	self assert: areas isEmpty!

testDamageInOutside3

	| w  outer inner areas|
	w := TerminalWidget new.
	outer := 1@1 corner: 20@20.
	inner := 0@0 corner: 30@30.
	areas := w damageIn: outer outside: inner.
	self assert: areas isEmpty!

testDamageInOutside4

	| w  outer inner areas|
	w := TerminalWidget new.
	outer := 1@1 corner: 20@20.
	inner := 10@10 corner: 20@20.
	areas := w damageIn: outer outside: inner.
	self assert: areas size = 2.!

testDropDownOverlap

	| t s i list1 txt  dd1 dd2|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 20 @ 30).
	dd1 := TerminalDropDownList in: (2 @ 6 corner: 2 @ 14).
	dd1 maximumVisibleRows: 10.
	dd1 appearance background: Color blue.
	dd1 items: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20).
	s add: dd1.
	
	dd2 := TerminalList in: (4 @ 2 corner: 14 @ 20).
	dd2 appearance background: Color green.
	dd2 items: #('12fdasfdsa3' '4fdsafdsa56' '78fdasfdsa9' '2004fdsafdsa' ).
	s add: dd2.
	
	t := Terminal show: s.
	self close: t!

testEditBackspace

	self doTestEdit: [ :input |
		input string: ''.
		input controller keyBackspace.
		self assert: input textlines first isEmpty]!

testEditBackspace2

	self doTestEdit: [ :input |
		input string: '12345678901'.
		input controller keyBackspace.
		self assert: input textlines first first = $1 ]!

testEditBackspace3

	self doTestEdit: [ :input |
		input string: '12345678901'.
		input controller requestCursorAt: input cursorAtEnd.
		input controller keyBackspace.
		self assert: input textlines first first = $1 ]!

testEditDeleteChar

	self doTestEdit: [ :input |
		input string: 'deleted'.
		input deleteCharacterAt: input origin + (0@0).
		self assert: input textlines first = 'eleted' ]!

testEditDeleteChar1

	self doTestEdit: [ :input |
		input string: 'deleted'.
		input deleteCharacterAt: input origin + (0@1).
		self assert: input textlines first = 'dleted' ]!

testEditDeleteChar2

	self doTestEdit: [ :input |
		input string: 'deleted'.
		input deleteCharacterAt: input origin + (0@6).
		self assert: input textlines first = 'delete' ]!

testEditDeleteChar3

	self doTestEdit: [ :input |
		input string: 'deleted
line'.
		input deleteCharacterAt: input origin + (1@0).
		self assert: input textlines first size = ('deleted' size + 1).
		self assert: input textlines last = 'ine' ]!

testEditDeleteCR

	self doTestEdit: [ :input |
		input string: 'deleted
line'.
		input deleteCharacterAt: input origin + (0@7).
		self assert: input textlines first = 'deletedline'.
		self assert: input textlines size = 1 ]!

testEditInsertChar

	self doTestEdit: [ :input |
		input insertCharacter: $? at: input origin ]!

testEditInsertChar10

	self doTestEdit: [ :input |
		input string: '=return'.
		input insertCharacter: Character cr at: input origin.
		self assert: input textlines first size = 1.
		self assert: (input textlines at: 2) = '=return'. ]!

testEditInsertChar11

	self doTestEdit: [ :input |
		input string: '=return'.
		input insertCharacter: Character cr at: input origin + (0@2).
		self assert: input textlines first = ('=r' copyWith: Character cr).
		self assert: (input textlines at: 2) = 'eturn'. ]!

testEditInsertChar2

	self doTestEdit: [ :input |
		input string: '=question'.
		input insertCharacter: $? at: input origin.
		input textlines first = '?=question' ]!

testEditInsertChar3

	self doTestEdit: [ :input |
		input string: '=question'.
		input insertCharacter: $? at: input origin + (0@2).
		self assert: input textlines first = '=q?uestion' ]!

testEditInsertChar4

	self doTestEdit: [ :input |
		input string: '=question'.
		input insertCharacter: $? at: input origin + (0@9).
		self assert: input textlines first = '=question?' ]!

testEmptyDropDown

	| t s i list1 txt  dd1 dd2|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 10 @ 20).
	dd1 := TerminalDropDownList in: (2 @ 2 corner: 2 @ 10).
	dd1 appearance background: Color blue.
	dd1 items: #().
	s add: dd1.
		
	t := Terminal show: s.
	self close: t!

testFileList3

	| t s table c1 c2 c3 dir names items label c0  drop|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 61).
	s appearance background: Color darkGray.
	
	
	drop := TerminalDropDownList in:(3@4 corner: 3@14).
	(drop appearance)
		background: Color black;
		foreground: Color yellow.
	drop items: #( 'java' 'c-sharp' 'basic' ).
	s add: drop.
	
	
	
	table := TerminalTableList in: (5 @ 2 corner: 20 @ 61).
	table name: 'files'.
	c0 := TTableColumn new.
	c0 header appearance background: Color darkRed lighter.
	c0 width: 3.
	c0 heading: 'R'.
	(c0 appearance)
		background: Color white;
		foreground: Color black.
	table addColumn: c0.
	c1 := TTableColumn new.
	c1 header appearance background: Color darkRed lighter.
	c1 width: 17.
	c1 maxLength: 10.
	c1 heading: 'path'.
	c1 appearance background: Color darkGreen.
	table addColumn: c1.
	c2 := TTableColumn new.
	c2 header appearance background: Color darkGreen lighter lighter.
	c2 width: 20.
	c2 appearance background: Color darkRed.
	c2 heading: 'name'.
	table addColumn: c2.
	table appearance background: Color blue.
	c3 := TTableColumn new.
	c3 header appearance background: Color blue.
	c3 width: 20.
	c3 appearance background: Color black.
	c3 appearance foreground: Color yellow.
	c3 heading: 'extension'.
	table addColumn: c3.
	dir := 'c:\winnt' asFilename.
	names := dir filesMatching: '*'.
	items := OrderedCollection new.
	names doWithIndex: 
			[:each :idx | 
			items add: (Array 
						with: idx asString
						with: dir asString
						with: each
						with: each asFilename fileExtension)].
	table items: items.
	s add: table.

	t := Terminal show: s.
	self close: t!

testFocusJumpOnTab

	| t  w menuBar menu1 menu2 txt menu3 input button|
	t := Terminal new.
	w := TerminalWidget windowClass in: (1 @ 1 corner: 20 @ 24).
	self addToMenuBar: w menuBar.
	menuBar := w menuBar.
	menuBar appearance background: Color white.
	w add: menuBar.

	txt := TerminalWidget textClass in: (2@1 corner: 8@24).
	txt appearance background: Color navy.
	txt string: 'This is a TerminalMenuBar Test'.
	w add: txt.

	input := TerminalWidget inputIn: (10@1 corner: 10@ 24).
	(input appearance) 
		background: Color yellow
		;foreground: Color black.
	input string: 'input'.
	w add: input.
	
	button := TerminalWidget buttonIn: (12@1 corner: 12@10).
	button label: 'button'.
	button appearance background: Color gray.
	w add: button.

	t show: w.
	t openWidget.
	self close: t.!

testFullScreenSpeed

	| t s   text ms|
	t := Terminal new.
	s := self screenFull.
	t := Terminal show: s.
	text := s widgetNamed: 'text'.
	
	ms := Time millisecondsToRun:[150 timesRepeat:[text paste: 'Smalltalk']].
	text string: ms printString , ' milliseconds'.
	
	self close: t.
!

testFullScreenSpeed2

	| t s  ms list|
	t := Terminal new.
	s := self screenListFull.
	t show: s.
	t openWidget.
	list := s widgetNamed: 'list'.
	list items: (Array new: 50 withAll: 'SmalltalkSmalltalkSmalltalkSmalltalkSmalltalkSmalltalkSmalltalkSmalltalk').
	ms := Time millisecondsToRun:[
		0 to: list items size - 1 do:[ :i | list selectionIndex: i]].
	list items: (Array with: ms printString , ' milliseconds').
	
	self close: t.
!

testFullScreenSpeedProfile

	| t s   text ms|
	t := Terminal new.
	t openWidget.
	s := self screenFull.
	t show: s.
	text := s widgetNamed: 'text'.	
"
	#EsbSampler asClass notNil
		ifTrue:[(#EsbSampler asClass spyOn:[400 timesRepeat:[text paste: 'Smalltalk']]) browse].
"
	self close: t.
!

testGetSet
	grid at: 1@3 put: 'a'.
	self assert: (grid at: 1@3) = 'a'!

testGetSet2
	grid at: 1@3 put: 'abc'.
	self assert: (grid at: 1@3 size: 3) = 'abc'!

testGetSetCharacter
	grid at: 1@3 put: $a.
	self assert: (grid at: 1@3) = 'a'!

testGraphics

	| t  s x image lab|
	t := Terminal new.
	s := TerminalWidget windowIn: (1 @ 1 extent: 40 @ 80).
	lab := TerminalWidget labelIn:(2@2 extent: 0@60).
	lab appearance background: Color blue.
	lab appearance borderColor: Color yellow.
	s add: lab.
	t show: s.
	t openWidget.

	(lab grid row: 2 column: 2) showBorderBottom: true.
	(lab grid row: 2 column: 4) showBorderLeft: true.	
	(lab grid row: 2 column: 6) showBorderRight: true.	
	(lab grid row: 2 column: 8) showBorderTop: true.	
	(lab grid row: 2 column: 10) showCR: true.
	(lab grid row: 2 column: 12) showGrid: true.
	(lab grid row: 2 column: 14) showSeparatorBottomLeft: true.
	(lab grid row: 2 column: 16) showSeparatorBottomRight: true.	
	(lab grid row: 2 column: 18) showSeparatorHorizontal: true.	
	(lab grid row: 2 column: 20) showSeparatorTopLeft: true.	
	(lab grid row: 2 column: 22) showSeparatorTopRight: true.	
	(lab grid row: 2 column: 24) showSeparatorVertical: true.	
	(lab grid row: 2 column: 25) 
		character: $K
		;showUnderline: true.		
	
	self close: t!

testInput
	| t s i list1 txt  lbl|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 24 @ 40).
	lbl := TerminalWidget labelIn: (9 @ 10 extent: 0 @ 10).
	lbl string: 'fixed,fit'.
	s add: lbl.
	
	txt := TerminalWidget inputIn: (10 @ 10 extent: 0 @ 3).
	txt appearance background: Color blue lighter.
	txt maxLength: 4.
	txt string: '1234'.
	s add: txt.
	
	lbl := TerminalWidget labelIn: (12 @ 10 extent: 0 @ 7).
	lbl string: 'variable'.
	s add: lbl.
	
	txt := TerminalWidget inputIn: (13 @ 10 extent: 0 @ 3).
	txt appearance background: Color blue lighter lighter.
	txt string: '1234'.
	s add: txt.
	
	lbl := TerminalWidget labelIn: (15 @ 10 extent: 0 @ 12).
	lbl string: 'one,variable'.
	s add: lbl.
	
	txt := TerminalWidget inputIn: (16 @ 10 extent: 0 @ 0).
	txt appearance background: Color blue lighter lighter lighter.
	txt string: '1'.
	s add: txt.	
	
	lbl := TerminalWidget labelIn: (18 @ 10 extent: 0 @ 8).
	lbl string: 'one,fixed'.
	s add: lbl.
	
	txt := TerminalWidget inputIn: (19 @ 10 extent: 0 @ 0).
	txt appearance background: Color blue lighter lighter lighter lighter.
	txt maxLength: 1.
	txt string: '1'.
	s add: txt.		
	
	lbl := TerminalWidget labelIn: (21 @ 10 extent: 0 @ 16).
	lbl string: 'fixed,oversized'.
	s add: lbl.
	
	txt := TerminalWidget inputIn: (22 @ 10 extent: 0 @ 8).
	txt appearance background: Color blue.
	txt maxLength: 4.
	txt string: '1234'.
	s add: txt.			
	
	t := Terminal show: s.
	self close: t!

testInputFormPanel

	| t |
	t := Terminal show: self inputFormPanel.
	self close: t!

testInputSetString

	| w m |
	w := TerminalInput new.
	m := IndexedMessageSend receiver: w index: TMStringSet argument: 'Hello'.
	m evaluate.
	self assert: w string = 'Hello'.!

testLabel1
	| t s list  c1 c2 c3|
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 20 ).
	c1 := TerminalLabel in: (2@2 extent: 0@10).
	c1 appearance background: Color blue.
	c1 string: 'Left'.
	c1 alignment: 1.
	s add: c1.

	c2 := TerminalLabel in: (4@2 extent: 0@10).
	c2 appearance background: Color blue.
	c2 string: 'Middle'.
	c2 alignment: 2.
	s add: c2.

	c3 := TerminalLabel in: (6@2 extent: 0@10).
	c3 appearance background: Color blue.
	c3 string: 'Right'.
	c3 alignment: 3.
	s add: c3.
	
	t := Terminal show: s.
	self close: t!

testList
	| list |
	list := TerminalList new.
	list items: #(1 2 3 4).
	list selectionIndex: -1.
	self assert: list selectedItem isNil.
	self assert: list selectionIndex = -1.
	self assert: list items size = 4.
	list selectionIndex: 2.
	self assert: list selectedItem = 3.!

testList2
	| t s list |
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 20 ).
	list := TerminalWidget listClass in: ( 2 @ 2 corner: 12 @ 10 ).
	list appearance background: Color darkGreen.
	list items: #('one' 'two' 'three' 'four').
	s add: list.
	t := Terminal show: s.
	self close: t!

testListFocus
	| t s list  list2|
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 30 ).
	list := TerminalWidget listClass in: ( 2 @ 2 corner: 12 @ 10 ).
	list appearance background: Color darkGreen.
	list items: #('one' 'two' 'three' 'four').
	s add: list.
	
	list2 := TerminalWidget listClass in: ( 2 @ 12 corner: 12 @ 20 ).
	list2 appearance background: Color darkGreen.
	list2 appearance borderColor: Color red.
	list2 items: #('one' 'two' 'three' 'four').
	s add: list2.	
	t := Terminal show: s.
	self close: t!

testListScrolling
	| t s list |
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 20 ).
	list := TerminalWidget listClass in: ( 2 @ 2 corner: 6 @ 10 ).
	list appearance background: Color darkGreen.
	list items: #('one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' 'eleven' ).
	s add: list.
	list appearance borderColor: Color red.
	t := Terminal show: s.
	self close: t!

testListViewSingleLine
	| t s i list1 txt  list|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 20 @ 20).
	list := TerminalWidget listClass in: (2 @ 2 corner: 2 @ 10).
	list items: #( 1 2 3 4 ).
	list selectionIndex: 1.
	s add: list.
	t := Terminal show: s.
	
	3 timesRepeat:[ list controller keyDown].
	self assert: list selectionIndex = 3.
	
	self close: t!

testMenu
	| t s i list1 txt menu transcript |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 80).
	transcript := TerminalWidget textClass in: (22 @ 1 extent: 0 @ 80).
	(transcript appearance)
		foreground: Color white;
		background: Color darkgreen.
	s add: transcript.
	menu := TerminalWidget menuClass in: (2 @ 2 corner: 20 @ 20).
	menu appearance background: Color navy.
	menu add: ('1. First Choice' asEmphasizedText from: 4 to: 15 setForeground: #red )
		message: (#value asMessageTo: [transcript string: 'first']).
	menu addSpace.
	menu add: '2. Second Choice'
		message: (#value asMessageTo: [transcript string: 'second']).
	menu addLine.
	menu add: '3. Close this window' message: (#closeWidget asMessageTo: t).
	s add: menu.
	t := Terminal show: s.
	self close: t!

testMenuBar
	"DoitTool open:'TerminalTests run: #testMenuBar'
	"
	
	| t  w menuBar menu1 menu2 txt menu3|
	w := TerminalWidget windowClass in: (1 @ 1 corner: 8 @ 24).
	self addToMenuBar: w menuBar.
	w menuBar appearance background: Color yellow.
	w menuBar appearance selectionForeground: Color white.

	txt := TerminalWidget textClass in: (2@1 corner: 8@24).
	txt appearance background: Color navy.
	txt string: 'This is a TerminalMenuBar Test'.
	w add: txt.

	t := Terminal show: w.
	self close: t.!

testOneItemDropDown

	| t s i list1 txt  dd1 dd2|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 10 @ 20).
	dd1 := TerminalDropDownList in: (2 @ 2 corner: 2 @ 10).
	dd1 appearance background: Color blue.
	dd1 items: #('one').
	dd1 when: TWidgetEvent selectedItem
		send: #echo
		to: self.
	s add: dd1.
	
	dd2 := TerminalDropDownList in: (4 @ 8 corner: 4 @ 16).
	dd2 appearance background: Color green.
	dd2 items: #('123' '456' ).
	s add: dd2.
	
	t := Terminal show: s.
	self close: t!

testRadioButton1
	| t s list  c1 c2 c11 c22|
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 20 ).
	c1 := TerminalRadioButton in: (2@2 extent: 0@10).
	c1 alignment: 1.
	c1 tickAlignment: 3.
	c1 string: 'English'.
	c1 selection: true.
	s add: c1.
	
	c11 := TerminalRadioButton in: (3@2 extent: 0@10).
	c11 alignment: 1.
	c11 tickAlignment: 3.
	c11 string: 'Dutch'.
	c11 selection: true.
	s add: c11.	
	
	c2 := TerminalRadioButton in: (5@2 extent: 0@10).
	c2 alignment: 3. "right"
	c2 tickAlignment: 1. "left"
	c2 string: 'Japanese'.
	c2 selection: true.
	s add: c2.	
	
	c22 := TerminalRadioButton in: (6@2 extent: 0@10).
	c22 alignment: 3. "right"
	c22 tickAlignment: 1. "left"
	c22 string: 'Chinese'.
	c22 selection: true.
	s add: c22.		
	
	"set group"
	c1 groupName: 'radios'.
	c11 groupName: 'radios'.
	c2 groupName: 'radios'.
	c22 groupName: 'radios'.
	
	t := Terminal show: s.
	self close: t!

testRadioGroup
	| t s list  c1 c11 tgb |
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 20 ).
	tgb := TerminalGroupBox in: (2 @ 2 extent: 5@12).
	tgb label: 'Lang'.
	tgb name: 'lang'.

	c1 := TerminalRadioButton in: (4@4 extent: 0@8).
	c1 string: 'English'.
	c1 selection: true.
	c1 groupName: 'lang'.
	tgb add: c1.

	c11 := TerminalRadioButton in: (5@4 extent: 0@8).
	c11 string: 'Dutch'.
	c11 selection: false.
	c11 groupName: 'lang'.
	tgb add: c11.	

	s add: tgb.

	tgb appearance background: Color yellow.
	tgb appearance foreground: Color blue.
	tgb label: 'foo'.

	t := Terminal show: s.
	self close: t!

testScreen

	| s t l |
	s := TerminalWidget screenIn: ( 1 @ 1 extent: 20 @ 20 ).
	l := TerminalWidget listIn: ( 2 @ 2 extent: 10 @ 6 ).
	l items: #('dennis' 'ruben' 'laura' 'jody').
	l when: #selectionItem send: #status: to: s.
	s add: l.
	t := Terminal show: s.
	self close: t!

testScreen2x2

	| t s |
	t := Terminal show: ( s := self screen2x2 ). 
	
	self close: t!

testScreen2x2_input
						
	| t s |
	t := Terminal show: (s := self screen2x2).
	t setFocusWidget: (s widgetNamed: 'text1' ).
	'text1' do:[ :ch |
		t focusWidget controller keySimpleCharacter: ch].
	t focusWidget controller keyTab.
	'text2' do:[ :ch |
		t focusWidget controller keySimpleCharacter: ch].
	t focusWidget controller keyTab.	
	'text3' do:[ :ch |
		t focusWidget controller keySimpleCharacter: ch].
	t focusWidget controller keyTab.
	'text4' do:[ :ch |
		t focusWidget controller keySimpleCharacter: ch].
	t focusWidget controller keyTab.			
	' has focus' do:[ :ch |
		t focusWidget controller keySimpleCharacter: ch].
		
	self close: t.!

testScreenBlackWhite

	| s t l  b s1 s2|
	s1 := TerminalWidget screenIn: ( 1 @ 1 extent: 20 @ 20 ).
	t := Terminal show: s1.
	(Delay forSeconds: 1) wait.
	s2 := TerminalWidget screenIn: ( 1 @ 1 extent: 20 @ 20 ).
	s2 background: Color white.
	t := Terminal show: s2.
	


	self close: t!

testScreenBlackWhiteByMessage
	| s t l b s1 s2 msg |
	s1 := TerminalWidget screenIn: (1 @ 1 extent: 20 @ 20).
	t := Terminal show: s1.
	(Delay forSeconds: 1) wait.
	msg := IndexedMessageSend 
				receiver: s1 name
				index: 3
				argument: (TWidgetAppearance new background: Color white).
	s1 dispatchIndexedMessage: msg.
	self close: t!

testScreenDropDown

	| t s i list1 txt  dd1 dd2|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 10 @ 20).
	dd1 := TerminalDropDownList in: (2 @ 2 corner: 2 @ 10).
	dd1 appearance background: Color blue.
	dd1 items: #('one' 'two' 'three' 'four'  ).
	dd1 when: TWidgetEvent selectedItem
		send: #echo
		to: self.
	s add: dd1.
	
	dd2 := TerminalDropDownList in: (4 @ 8 corner: 4 @ 16).
	dd2 appearance background: Color green.
	dd2 items: #('123' '456' '789' '2004' ).
	s add: dd2.
	
	t := Terminal show: s.
	self close: t!

testScreenFunctionKeys
	| s t l |
	s := TerminalWidget screenIn: (1 @ 1 extent: 20 @ 20).
	s 
		when: (TWidgetEvent F: 1)
		send: #echo
		to: self.
	s controller keyFunction: 1 shift: false.
	t := Terminal show: s.
	self close: t!

testScreenHuge

	| s t l |
	s := TerminalWidget screenIn: ( 1 @ 1 extent: 100 @ 100 ).
	t := Terminal show: s.
	self close: t!

testScreenInsertAfterTextReplace
	| t s i list1 txt |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 80).
	txt := TerminalWidget textClass in: (1 @ 1 corner: 20 @ 20).
	txt appearance background: Color blue.
	txt string: 'Hello,
	
	This is an InsertAfterTextReplace Test.'.
	s add: txt.
	t := Terminal show: s.
	txt controller keyCtrlEnd.
	txt controller keySimpleCharacter: $?.
	txt string: 'Hello'.
	txt controller keySimpleCharacter: $!!.
	
	self close: t!

testScreenMenuBar
	| s t menu help |
	s := TerminalWidget screenIn: (1 @ 1 extent: 20 @ 20).
	menu := TerminalMenuList new.
	menu add: 'Close' message: nil.
	help := TerminalMenuList new.
	help add: 'About' message: nil.
	s menuBar 
		add: 'File'
		key: $F
		menu: menu.
	s menuBar 
		add: 'Help'
		key: $H
		menu: help.
	t := Terminal show: s.
	self close: t!

testScreenMultiLineTest
	| t s i list1 txt |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 20 @ 30).
	txt := TerminalWidget textClass in: (1 @ 1 corner: 20 @ 20).
	txt appearance background: Color blue.
	txt appearance borderColor: Color yellow.
	txt string: 'Hello,
	
	This is a Multi-Line Test.'.
	s add: txt.
	t := Terminal show: s.
	self close: t!

testScreenNonFittingText
	| t s i list1 txt |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 20 @ 20).
	txt := TerminalWidget textClass in: (1 @ 1 corner: 4 @ 4).
	txt appearance background: Color blue.
	txt string: '1234567812345678'.
	s add: txt.
	t := Terminal show: s.
	self close: t!

testScreenPanelButton

	| s  p b t|
	s := TerminalWidget screenIn: (1@1 corner: 20@20).
	s background: Color blue.
	p := TerminalWidget panelIn: (2@2 corner: 19@19).
	p name: 'panel'. 
	p background: Color yellow.
	b := TerminalWidget buttonIn: (10@10 corner: 10@14).
	b label: 'OK'.
	b background: Color green.
	s add: p.
	p add: b.
	t := Terminal show: s.
	self close: t!

testScreenPanelButton2

	| s  p b t|
	s := TerminalWidget screenIn: (1@1 corner: 20@20).
	p := TerminalWidget panelIn: (2@2 corner: 19@19).
	p name: 'panel'. 
	p background: Color yellow.
	b := TerminalWidget buttonIn: (10@10 corner: 10@14).
	b label: 'OK'.
	b background: Color green.
	s add: p.
	p add: b.
	t := Terminal show: s.
	self close: t!

testScreenPanelButton3

	| s  p b t|
	s := TerminalWidget screenIn: (1@1 corner: 20@20).
	s background: Color blue.
	p := TerminalWidget panelIn: (2@2 corner: 19@19).
	p name: 'panel'. 
	p background: Color yellow.
	b := TerminalWidget buttonIn: (10@10 corner: 10@14).
	b label: 'OK'.
	b foreground: Color black.
	s add: p.
	p add: b.
	self assert: b visibleAppearance background = p appearance background.
	t := Terminal show: s.
	self close: t!

testScreenPanelButton4

	| s  p b t|
	s := TerminalWidget screenIn: (1@1 corner: 20@20).
	s background: Color blue.
	p := TerminalWidget panelIn: (2@2 corner: 19@19).
	p name: 'panel'. 
	p background: Color yellow.
	p borderColor: Color red.
	b := TerminalWidget buttonIn: (10@10 corner: 10@14).
	b label: 'OK'.
	b foreground: Color black.
	b borderColor: Color red.
	s add: p.
	p add: b.
	self assert: b visibleAppearance background = p appearance background.
	t := Terminal show: s.
	self close: t!

testScreenScrollingText
	| t s i list1 txt |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 20 @ 20).
	txt := TerminalWidget textClass in: (2 @ 2 corner: 5 @ 5).
	txt appearance background: Color blue.
	txt string: '123
abc
456
def
789'.
	s add: txt.
	t := Terminal show: s.
	self close: t!

testScreenTableList
	"DoitTool open:'TerminalTests run: #testScreenTableList'
	"

	| t s table c1 c2 c3 b1 |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 80).
	s appearance background: Color darkGray.
	table := TerminalTableList in: (2 @ 2 corner: 20 @ 52).
	table name: 'table'.
	c1 := TTableColumn new.
	c1 header appearance background: Color darkRed lighter.
	c1 width: 10.
	c1 maxLength: 10.
	c1 heading: 'left'.
	c1 appearance background: Color darkGreen.
	c1 appearance borderColor: Color black.	
	table addColumn: c1.
	c2 := TTableColumn new.
	c2 header appearance background: Color darkGreen.
	c2 width: 20.
	c2 appearance background: Color darkRed.
	c2 appearance borderColor: Color black.
	c2 heading: 'middle'.
	table addColumn: c2.
	table appearance background: Color blue.
	table appearance borderColor: Color yellow.
	c3 := TTableColumn new.
	c3 header appearance background: Color blue.
	c3 width: 20.
	c3 appearance background: Color white.
	c3 appearance foreground: Color yellow.
	c3 appearance borderColor: Color black.
	c3 heading: 'right'.
	table addColumn: c3.
	table items: #(#('one' 'two') #('three' 'four')).
	s add: table.
	b1 := TerminalButton in: (22 @ 2 extent: 0 @ 10).
	b1 label: 'Clear'.
	b1 help: 'Empty list'.
	b1 borderColor: Color green.
	b1 
		when: TWidgetEvent clicked
		send: #items:
		to: table
		with: #().
	s add: b1.
	t := Terminal show: s.
	self close: t!

testScreenTableListEditable
	"DoitTool open:'TerminalTests run: #testScreenTableList'
	"

	| t s table c1 c2 c3 b1 |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 80).
	s appearance background: Color darkGray.
	table := TerminalTableList in: (2 @ 2 corner: 20 @ 52).
	table name: 'table'.
	table appearance borderColor: Color white.
	table appearance focusColor: Color red.
	c1 := TTableColumn new.
	c1 header appearance background: Color darkRed lighter.
	c1 width: 10.
	c1 maxLength: 10.
	c1 heading: 'left'.
	c1 appearance background: Color darkGreen.
	table addColumn: c1.
	c2 := TTableColumn new.
	c2 header appearance background: Color darkGreen.
	c2 width: 20.
	c2 appearance background: Color darkRed.
	c2 heading: 'middle'.
	c2 editable: true.
	table addColumn: c2.
	table appearance background: Color blue.
	table appearance borderColor: Color yellow.
	c3 := TTableColumn new.
	c3 header appearance background: Color blue.
	c3 width: 20.
	c3 appearance background: Color white.
	c3 appearance foreground: Color yellow.
	c3 heading: 'right'.
	table addColumn: c3.
	table items: #(#('one' 'two') #('three' 'four')).
	s add: table.
	b1 := TerminalButton in: (22 @ 2 extent: 0 @ 10).
	b1 label: 'Clear'.
	b1 help: 'Empty list'.
	b1 borderColor: Color green.
	b1 
		when: TWidgetEvent clicked
		send: #items:
		to: table
		with: #().
	s add: b1.
	t := Terminal show: s.
	self close: t!

testScreenTableListNoRows
	"DoitTool open:'TerminalTests run: #testScreenTableListNoRows'
	"

	| t s table c1 c2 |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 80).
	s appearance background: Color darkGray.
	table := TerminalTableList in: (2 @ 2 corner: 2 @ 52).
	c1 := TTableColumn new.
	c1 width: 10.
	c1 maxLength: 10.
	c1 heading: 'left'.
	c1 appearance background: Color darkGreen.
	table addColumn: c1.
	c2 := TTableColumn new.
	c2 width: 20.
	c2 appearance background: Color darkRed.
	c2 heading: 'right'.
	table addColumn: c2.
	table appearance background: Color blue.
	s add: table.
	t := Terminal show: s.
	self close: t!

testScreenTabTest
	| t s i list1 txt |
	s := TerminalWidget windowClass in: (1 @ 1 extent: 20 @ 30).
	txt := TerminalWidget textClass in: (1 @ 1 corner: 20 @ 20).
	txt appearance background: Color blue.
	txt string: 'BeforeTab	AfterTab'.
	s add: txt.
	t := Terminal show: s.
	self close: t!

testScreenUnity

	| s t l |
	s := TerminalWidget screenIn: ( 1 @ 1 extent: 0 @ 0 ).
	t := Terminal show: s.
	self close: t!

testShowDialogNoDelay

	| t  d s|
	t := Terminal new.
	t openWidget.
	d := self dialogIn: (4@4 extent: 0@31)  text: '< How can this be Smalltalk ?  >'.
	s := self screen2.
	t show: s.
	s add: d.
	d show.
	d hide.
	s remove: d.
	s show.		
	t hideScreen.
	(Delay forSeconds:1) wait.		
	self close: t!

testShowScreenAfterOpen
	| t s i |
	t := Terminal new.
	t openWidget.
	s := TerminalWidget windowClass in: (1 @ 1 extent: 40 @ 80).
	s add: (i := TerminalWidget textIn: (2 @ 2 corner: 2 @ 32)).
	i string: 'Terminal Widgets'.
	i name: 'input1'.
	i appearance: (i appearance background: Color green).
	t show: s.
	self close: t!

testShowScreenSwap

	| t s1 s2  |
	t := Terminal new.
	t openWidget.
	s1 := self screen1.
	s2 := self screen2.	
	
	t show: s1.
	(Delay forSeconds:0.5) wait.
	t hideScreen.
	(Delay forSeconds:0.5) wait.	
	t show: s2.
	(Delay forSeconds:0.5) wait.
	t hideScreen.	
	(Delay forSeconds:0.5) wait.
	t show: s1.
	(Delay forSeconds:0.5) wait.
	t hideScreen.
	(Delay forSeconds:0.5) wait.	
	self close: t.
!

testShowScreenWithInputBeforeOpen
	| t s |
	t := Terminal new.
	s := self screen1.
	t show: s.
	t openWidget.
	(Delay forSeconds:0.5) wait.
	self close: t!

testSize
	| txt |
	txt := TerminalWidget textClass in: (2@2 corner:2@33).
	self assert: txt rows = 1.
	self assert: txt columns = 32.
	self assert: txt maxRow = 2.
	self assert: txt maxColumn = 33.!

testTextFromLines
	| txt lines some string |
	txt := TerminalWidget textIn: (1 @ 1 corner: 4 @ 4).
	some := '1234567890'.
	lines := txt linesFromText: some.
	string := txt textFromLines: lines.
	self assert: string = some!

testTextFromLines2
	| txt lines some string |
	txt := TerminalWidget textIn: (1 @ 1 corner: 4 @ 4).
	some := '12345 67890'.
	lines := txt linesFromText: some.
	string := txt textFromLines: lines.
	self assert: string size = some size!

testTextFromLinesCR
	| txt lines some string |
	txt := TerminalWidget textIn:(1 @ 1 corner: 4 @ 10).
	some := '123456
7890'.
	lines := txt linesFromText: some.
	self assert: lines first size = 7.
	self assert: lines last size = 4.	
	
	string := txt textFromLines: lines.
	self assert: string = some!

testTextLinesFrom
	| txt lines |
	txt := TerminalWidget textIn: (1 @ 1 corner: 4 @ 4).
	lines := txt linesFromText: '1234567890'.
	self assert: lines size = 3.
	self assert: lines last = '90'!

testTextLinesFromTab
	| txt lines  text|
	txt := TerminalWidget textIn: (1 @ 1 corner: 4 @ 4).
	txt tabSpacing: 2.
	lines := txt linesFromText: '1	2'.
	self assert: lines first size = 4.
	text := txt textFromLines: lines.
	self assert: text = '1  2'.!

testTextOffset

	| t  off|
	t := TerminalText in: (2@2 corner: 20@20).
	t string: 'Hello
From
Amersfoort'.

	off := t computeTextIndex: 2@2.
	self assert: off = 1.
	self assert: (t computeRowCharacterPoint: off) = (2@2).
	
	off := t computeTextIndex: 3@2.
	self assert: off = 7.	
	self assert: (t computeRowCharacterPoint: off) = (3@2).	
	
	off := t computeTextIndex: 3@3.
	self assert: off = 8.	
	self assert: (t computeRowCharacterPoint: off) = (3@3).		
	
	off := t computeTextIndex: 4@3.
	self assert: off = 13.		
	self assert: (t computeRowCharacterPoint: off) = (4@3).	!

testTextTwice

	| t s i list1 txt  txt2 lbl|
	s := TerminalWidget windowClass in: (1 @ 1 extent: 20 @ 30).

	lbl := TerminalWidget labelIn: (1@2 extent: 1@20).
	lbl label: 'Static text'.
	s add: lbl.
	
	txt := TerminalWidget textClass in: (2 @ 2 corner: 10 @ 20).	
	txt appearance background: Color blue.
	s add: txt.
	
	txt2 := TerminalWidget textClass in: (12 @ 2 corner: 20 @ 20).
	txt2 appearance background: Color navy.
	s add: txt2.	
	
	t := Terminal show: s.
	self close: t!

testWordWrap1
	| lines |
	lines := WordWrapper 
				on: ' 1234'
				columns: 4
				hyphenations: ' '
				tabSpacing: 4.
	self assert: lines first = ' '.
	self assert: lines last = '1234'!

testWordWrap2
	| lines |
	lines := WordWrapper 
				on: '1234'
				columns: 4
				hyphenations: ' '
				tabSpacing: 4.
	self assert: lines first = '1234'!

testWordWrap3
	| lines |
	lines := WordWrapper 
				on: '12 34'
				columns: 4
				hyphenations: ' '
				tabSpacing: 4.
	self assert: lines first = '12 '.
	self assert: lines last = '34'!

testWordWrap4
	| lines |
	lines := WordWrapper 
				on: '12 34 56 78'
				columns: 4
				hyphenations: ' '
				tabSpacing: 4.
	self assert: lines first = '12 '.
	self assert: (lines at: 2) = '34 '.
	self assert: (lines at: 3) = '56 '.
	self assert: (lines at: 4) = '78'!

testWordWrap5
	| lines |
	lines := WordWrapper 
				on: '12-34 56-78'
				columns: 4
				hyphenations: '-'
				tabSpacing: 4.
	self assert: lines first = '12-'.
	self assert: (lines at: 2) = '34 5'.
	self assert: (lines at: 3) = '6-78'!

testWordWrap6
	| lines |
	lines := WordWrapper 
				on: (( '12-34
56-78' ) copyWithout: Character lf)
				columns: 8
				hyphenations: '-'
				tabSpacing: 4.
	self assert: lines first size = (5 + 1).
	self assert: (lines at: 2) = '56-78'!

testWordWrap7
	| lines  text|
	text := 'one
two
three
four'.
	lines := WordWrapper 
				on:  (text copyWithout: Character lf)
				columns: 8
				hyphenations: ''
				tabSpacing: 4.
	self assert: lines size = 4.!

testWordWrap8
	| lines  text|
	text := 'one two three four five six seven eight nine ten
eleven twelf thirteen fourteen'.
	lines := WordWrapper 
				on:  (text copyWithout: Character lf)
				columns: 8
				hyphenations: ' '
				tabSpacing: 4.
	self assert: lines size = 14.! !
	

!WidgetDirtyTest publicMethods !

testAppearance
	| widget  msg|
	widget := TerminalInput in: (1@1 corner: 1@10).
	self deny: widget isDirty.
	widget foreground: Color yellow.
	self assert: widget isDirty.
	self assert: (widget isDirty: TMAppearanceSet).
	msg := OrderedCollection new.
	widget addStateMessagesTo: msg.
	self assert: msg size = 1.
	widget clearDirty.
	self deny: widget isDirty.	
	!

testBounds
	| widget  msg|
	widget := TerminalInput in: (1@1 corner: 1@10).
	self deny: widget isDirty.
	widget bounds: (2@2 corner: 3@8).
	self assert: widget isDirty.
	self assert: (widget isDirty: TMBoundsSet).
	msg := OrderedCollection new.
	widget addStateMessagesTo: msg.
	self assert: msg size = 1.
	widget clearDirty.
	self deny: widget isDirty.	
	!

testCheckBox
	| widget  msg|
	widget := TerminalCheckBox in: (1@1 corner: 1@10).
	self deny: widget isDirty.
	widget selection: true.
	self assert: widget isDirty.
	self assert: (widget isDirty: TMSelectionSet).
	msg := OrderedCollection new.
	widget addStateMessagesTo: msg.
	self assert: msg size = 1.
	widget clearDirty.
	self deny: widget isDirty.	
	!

testImage
	| widget  msg|
	widget := TerminalImageLabel in: (1@1 corner: 1@10).
	self deny: widget isDirty.
	widget scaleToFit:  widget scaleToFit not.
	widget url: 'http'.
	self assert: widget isDirty.
	self assert: (widget isDirty: TMScaleToFitSet).
	self assert: (widget isDirty: TMURLSet).	
	msg := OrderedCollection new.
	widget addStateMessagesTo: msg.
	self assert: msg size = 2.
	widget clearDirty.
	self deny: widget isDirty.	
	!

testInput
	| widget  msg|
	widget := TerminalInput in: (1@1 corner: 1@10).
	self deny: widget isDirty.
	widget string: 'Hello'.
	self assert: widget isDirty.
	self assert: (widget isDirty: TMStringSet).
	self deny: (widget isDirty: TMEnabledSet).	
	msg := OrderedCollection new.
	widget addStateMessagesTo: msg.
	self assert: msg size = 1.
	widget clearDirty.
	self deny: widget isDirty.	
	!

testList
	| widget  msg|
	widget := TerminalList in: (1@1 corner: 10@10).
	self deny: widget isDirty.
	widget items: #( 1 2 3 4).
	widget selectedItem: 1.
	self assert: widget isDirty.
	self assert: (widget isDirty: TMItemsSet).
	self assert: (widget isDirty: TMSelectionIndexSet).	
	msg := OrderedCollection new.
	widget addStateMessagesTo: msg.
	self assert: msg size = 2.
	widget clearDirty.
	self deny: widget isDirty.	
	!

testVisible
	| widget msg |
	widget := TerminalInput in: ( 1 @ 1 corner: 1 @ 10 ).
	self deny: widget isDirty.
	widget visible: true.
	self assert: widget isDirty.
	self assert: ( widget isDirty: TMVisibleSet ).
	msg := OrderedCollection new.
	widget addStateMessagesTo: msg.
	self assert: msg size = 1.
	widget clearDirty.
	self deny: widget isDirty! !
	

!TerminalGraphicalObject publicMethods !

inspectActions
	^#(isSelected isHighlight showBorderBottom showBorderLeft showBorderRight showBorderTop showCR showGrid
	showSeparatorBottomLeft showSeparatorBottomRight showSeparatorHorizontal showSeparatorTopLeft 
	showSeparatorTopRight showSeparatorVertical showShadow )! !
	

!IndexedMessageSend publicMethods !

printOn: aStream
	aStream nextPutAll: self class name ;nextPut: $( ;print: receiver ;nextPut:$, ;print: (UITerminalComponent terminalConstants keyAtValue: index) ;nextPut: $, ;print: arguments ;nextPut: $)! !
	

!TerminalWindow publicMethods !

inspectActions
	^super inspectActions, #(#showInTerminal #hierarchy)! !

