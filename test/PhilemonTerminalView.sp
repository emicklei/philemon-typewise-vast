"Philemon SmalltalkMT exporter has done all the work
Version PhilemonTerminalView EM 5-0622"
PROJECTNAME PhilemonTerminalView .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonTerminalCommon PhilemonTextEmphasisSupport .
POOLS TerminalConstants TerminalCharacterConstants .

PROFILE
BEGIN
END

CLASS TerminalGraphicalObject
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	platformColorValueFor:
END
INSTANCEMETHODS
BEGIN
	showSeparatorBottomRight
	currentDisplayRect
	isInsertMode
	showSeparatorHorizontal
	showSeparatorVertical:
	box
	primitiveDrawOn:gc:
	showBorderTop
	showCheckboxTick:
	showSeparatorTopRight:
	showCheckboxTick
	showRadioTick
	showRadioTick:
	showFocus
	width
	showSeparatorBottomLeft:
	showBorderLeft:
	setAppearance:
	initializeBox:height:at:
	showRadioCircle:
	showSeparatorVertical
	showGrid
	showCR:
	drawBackgroundOn:gc:
	showSeparatorHorizontal:
	showSeparatorBottomRight:
	showCR
	highlight:insertMode:
	showSeparatorTopLeft
	showBorderRight:
	location:appearance:
	isHighlight
	showSeparatorBottomLeft
	showBorderTop:
	showBorderLeft
	showUnderline
	appearance:
	showFocus:
	mask:enabled:
	select:
	resetDecoration
	parent:
	showUnderline:
	showSeparatorTopLeft:
	showChoiceMarker:
	height
	isSelected
	showChoiceMarker
	box:
	appearance
	showBorderBottom
	localUpdate
	showCheckBox:
	showSeparatorTopRight
	showRadioCircle
	showGrid:
	showCheckBox
	initialize
	showBorderBottom:
	showBorderRight
END

CLASS TerminalImage
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	release
	for:
END
INSTANCEMETHODS
BEGIN
	location:appearance:
	loadImage
	initializeImageBox:height:at:
	image:
	widget:
	primitiveDrawOn:gc:
	initializeBox:height:at:
	widget
	file:
	width
	image
	height
	redraw
	file
	hasImage
	scaleToFit
	scaleToFit:
END

CLASS TerminalCharacter
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	new
END
INSTANCEMETHODS
BEGIN
	clearSeparators
	drawDecorationOn:gc:
	drawGridOn:gc:
	drawCharacterStringOn:gc:
	height
	drawBorderOn:gc:
	width
	printOn:
	string
	drawGraphicsOn:gc:
	drawSelectionBackgroundOn:gc:
	clearWithAppearance:refresh:
	initialize
	clearWithAppearance:
	character
	drawHighlightOn:gc:
	primitiveDrawOn:gc:
	character:
	drawSeparationBorderOn:gc:
END

CLASS UITerminalComponent
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	show:
END
INSTANCEMETHODS
BEGIN
	requestCursorAt:
	focusWidget
	initialize
	losingFocus
	show:
	releaseScreen
	screen:
	startBlinker
	postOpen
	gettingFocus
	toggleHighlightCursor
	cursor
	grid
	activeCharacter
	messageHandler:
	preClose
	dispatchKeyEvent:to:
	handleButtonEvent:pressed:
	displayScreen
	preOpen
	stopBlinker
	reInitializeGrid
	checkGridSize
	handleKeyEvent:
	release
	sendButtonEvent:pressed:at:to:
	setFocusWidget:
	screen
	isInsertMode
	hideScreen
	isInsertMode:
	messageHandler
	killBlinker
END

CLASS TerminalBasicController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	new
	umlDefinitionString
END
INSTANCEMETHODS
BEGIN
	losingFocus
	buttonReleaseAt:
	widget:
	keyCancel
	keyCtrlHome
	keyUp
	keyShiftRight
	keyHome
	keyAlt:
	keyCtrlEnd
	keyFunction:shift:
	keyLeft
	keyShiftTab
	keyShift
	buttonDoubleClickAt:
	keyBackspace
	keyTab
	form
	buttonPressAt:
	gettingFocus
	initialize
	keyDelete
	defaultActionRequested
	widget
	keySimpleCharacter:
	keyRight
	keyPageUp
	keyCtrlCharacter:
	keyEnd
	requestTabForward:
	printOn:
	promoteKeyEvent:
	keyReturn
	keyDown
	keyShiftLeft
	windowController
	keyPageDown
	requestTab
END

CLASS TerminalWindowController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	keyFunction:shift:
	requestTabForward:
END

CLASS TerminalListController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	umlDefinitionString
END
INSTANCEMETHODS
BEGIN
	buttonReleaseAt:
	buttonSelectedIndex:
	keySimpleCharacter:
	canRotateSelection
	buttonDoubleClickAt:
	keyUp
	keyDown
	keyPageDown
	keyEnd
	keyPageUp
	keyHome
END

CLASS TerminalTableListController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
END

CLASS TerminalMenuController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	keyReturn
	messageOnReturn
	keySimpleCharacter:
	canRotateSelection
	indexForCharacter:
END

CLASS TerminalMenuBarController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	umlDefinitionString
END
INSTANCEMETHODS
BEGIN
	losingFocus
	keyRight
	keyReturn
	keySimpleCharacter:
	keyUp
	keyDown
	keyPageDown
	keyPageUp
	gettingFocus
	keyLeft
	keyCancel
END

CLASS TerminalChoiceListController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	losingFocus
	choiceController
	choiceController:
	buttonDoubleClickAt:
	buttonReleaseAt:
END

CLASS TerminalInputController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	umlDefinitionString
END
INSTANCEMETHODS
BEGIN
	losingFocus
	keyCtrlHome
	localCursor
	isInsertMode
	keyUp
	isCursorHome
	isCursorBeforeEndOfLine
	keyShiftRight
	keyHome
	keyCtrlEnd
	cursor
	visibleOffset
	isCursorBeyondLine
	keyLeft
	isCursorInserting
	keyBackspace
	visibleOffset:
	gettingFocus
	initialize
	isCursorOnEndOfLine
	lastCursor
	keyDelete
	requestCursorAt:
	keyCtrlV
	keySimpleCharacter:
	keyRight
	requestCursorOffset:
	isCursorOnFirstColumn
	isCursorAtEnd
	keyEnd
	lastCursor:
	isCursorOnFirstRow
	keyReturn
	keyDown
	keyShiftLeft
	characterOffset
	characterOffset:
	buttonReleaseAt:
END

CLASS TerminalTextController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	keyRight
	keyReturn
	initialize
	keyCtrlR
	keySimpleCharacter:
	keyUp
	keyDown
	keyPageDown
	keyPageUp
	keyBackspace
	keyLeft
END

CLASS TerminalTableCellController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	gettingFocus
	keyUp
	keyRight
	isCursorOnFirstRow
	keyLeft
	tableWidget:
	keyDown
	isCursorOnLastRow
	requestCursorAt:
	tableWidget
END

CLASS TerminalDropDownListController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	keyCancel
	showsList:
	hideList
	currentList
	initialize
	list
	droppedListLostFocus
	buttonReleaseAt:
	droppedListDefaultActionRequested
	changedListSelection
	keyUp
	keyAlt:
	showList
	losingFocus
	keyTab
	showOrHideList
	gettingFocus
	keyReturn
	keyDown
	showsList
	keyPageUp
	keyPageDown
END

CLASS TerminalCheckBoxController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	keyLeft
	keyDown
	keySimpleCharacter:
	keyRight
	keyUp
	buttonReleaseAt:
END

CLASS TerminalButtonController
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	gettingFocus
	keyUp
	keyRight
	keyDown
	keyLeft
	losingFocus
	keyReturn
END

CLASS TWidgetAppearanceHolder
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	appearance
	aboutToRemoveFromTerminalForm:
	addFormDataTo:
	desiresFocus
	foreground:
	selectionBackground:
	setAppearance:
	parentAppearance
	background:
	justAddedToTerminalForm:
	border:
	visibleAppearance
	defaultAppearance
	appearanceOrNil
	selectionForeground:
	borderColor:
	checkAppearance
	changedFont
	appearance:
END

CLASS TerminalWidget
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	labelClass
	umlDefinitionString
	tagID
	textClass
	buttonClass
	new
	labelIn:
	windowClass
	panelIn:
	inputClass
	listIn:
	dropDownListIn:
	screenIn:
	tableListIn:
	image:in:
	messageSendClass
	in:
	menuClass
	listClass
	windowIn:
	buttonIn:
	menuBarClass
	inputIn:
	textIn:
END
INSTANCEMETHODS
BEGIN
	cursorAtHome
	parent
	initialize
	showSeparatingGraphicBorder:in:
	bounds:
	maxColumn
	cursor
	widgetUnderPoint:
	wantsFocusOnTab
	form
	installMessageSend:onEvent:
	hide
	receiverID
	updateFocus:
	putLine:startingAt:from:to:on:
	localColumnAt:
	tabRequestedFrom:forward:
	printOn:
	showGraphicBorder:focus:in:
	invalidateRectangle:
	defaultControllerClass
	when:send:
	rows
	bounds
	kindDo:
	messageHandler
	installMessageToServer:event:
	haveFocus
	hasFocus
	visible:
	maxRow
	damageIn:outside:
	boundsForFocus
	setInputFocus
	origin
	setVisible:
	hierarchy
	grid
	minColumn
	lineAt:
	centerOnParent
	topWidget
	show
	size
	installMessageToClient:event:
	inspectActions
	columns
	parent:
	printOn:indent:
	updateBorderAppearance
	updateContentsIn:
	hide:
	minRow
	desiresFocus
	translateBy:
	visible
	installMessage:onEvent:
	installMessageToWidget:event:
	displayExtent
	update
	updateContents
	updateAppearanceIn:
	corner
	haveFocusAt:
	isDescendentOf:
	clearDisplay:
END

CLASS TerminalCompositeWidget
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	form:
	defaultActionButton
	widgetsDo:
	release
	parent:
	form
	kindDo:
	messageHandler
	tabRequestedFrom:forward:
	remove:
	show
	addEventConnectionsTo:
	submitInputNamesAndValuesTo:
	widgets
	receiverAt:
	invalidateRectangle:
	add:
	widgetUnderPoint:
	inputNamesAndValues
	printOn:indent:
	widgetNamed:
	addFirst:
	translateBy:
END

CLASS TerminalWindow
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagName
	tagID
END
INSTANCEMETHODS
BEGIN
	status:
	title
	title:
	hasMenuBar
	menuBar
	menuBar:
	stylesheet:
	initialize
	enabled
	stylesheet
	close
	kindDo:
	findWidgetNamed:
	widgetNamed:
	controller
	controller:
END

CLASS TerminalGroupBox
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	alignment:
	label:
	updateContentsIn:
	alignment
	label
	kindDo:
END

CLASS TerminalBasicWidget
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	umlDefinitionString
END
INSTANCEMETHODS
BEGIN
	isEnabled
	defaultControllerClass
	initialize
	controller
	newController
	inspectActions
	enabled:
	controller:
	editable:
	enabled
	help
END

CLASS TerminalList
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	selectionIndex:
	defaultControllerClass
	desiresFocus
	selectedItem:
	topRow
	rowsForItems
	stSelectionIndex
	setHighlightLineAt:to:
	scrollDownTo:
	kindDo:
	selectionBoundsAt:
	updateRowContents
	visibleItemRange
	maxUsableColumn
	separatorString
	haveFocusAt:
	updateSelection
	previousSelectionIndexOrNil
	nextSelectionIndexOrNil
	scrollUpTo:
	items:
	hasItems
	primItems:
	reset
	updateRangeStatus
	wantsRangeStatus
	primSelectionIndex:
	initialize
	broadcastSelectedItem
	items
	selectionIndex
	broadcastSelectionIsValid
	updateContentsIn:
	addFormDataTo:
	printableItemAt:
	selectedItem
	bounds:
END

CLASS TerminalTableList
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	widgetNamed:
	tabRequestedFrom:forward:
	selectCellRight:
	topRow
	selectCellBelow:
	beCellSelecting
	tableColumnCount
	newCellController
	tableColumns
	kindDo:
	allInputWidgetsDo:
	updateHeaderContents
	lastUsedColumn
	isCellSelecting
	isRowSelecting
	initialize
	inputWidgets
	inputAt:
	inBackwardReadOrderDo:
	headingWidgets
	addColumn:
	selectCellAbove:
	rowsForItems
	updateContentsIn:
	show
	updateUnusedColumnsContents
	editable
	updateRowContents
	widgetUnderPoint:
	headingWidgets:
	maxUsableColumn
	widgetsDo:
	selectCellLeft:
	lastEditableInputWidget
	updateAppearanceIn:
	tableRowCount
	columnForCell:
	inForwardReadOrderDo:
	setInputFocus
	showGraphicBorder:focus:in:
END

CLASS TerminalMenuList
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	addSpace
	broadcastSelectionIndex
	isVertical
	messageToSendAt:
	addLine
	updateContentsIn:
	labels
	characters
	activateRequestAt:
	previousSelectionIndexOrNil
	labelWidth
	kindDo:
	nextSelectionIndexOrNil
	add:key:message:
	setHighlightLineAt:to:
	lineIndices:
	wantsRangeStatus
	lineIndices
	labelWidth:
	labels:
	isPopUp
	messages
	messages:
	initialize
	add:message:
	isPopUp:
	howManyLines
	characters:
	defaultControllerClass
	printOn:
END

CLASS TerminalMenuBar
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	clearDisplay:
	isVertical
	wantsRangeStatus
	defaultControllerClass
	hasOpenMenu
	translateBy:
	useNative:
	labels
	activateRequestAt:
	broadcastSelectionIndex
	useNative
	computeColumnStartForMenuAt:
	openMenuIndex
	initialize
	showNextMenu
	wantsFocusOnTab
	desiresFocus
	showPreviousMenu
	release
	hideMenu
	setHighlightLineAt:to:
	add:key:menu:
	kindDo:
	menus:
	currentMenu
	labels:
	characters
	characters:
	showMenu
	updateContentsIn:
	bounds:
	computeBoundsForMenuAt:
	menus
	openMenuIndex:
END

CLASS TerminalDropDownList
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	defaultControllerClass
	kindDo:
	addFormDataTo:
	selectedItemString
	updateContentsIn:
	updateRangeStatus
	initializeRowVisibilty
	widgetUnderPoint:
	initialize
	desiresFocus
	maximumVisibleRows:
	maximumVisibleRows
	updateSelection
	reset
END

CLASS TerminalLabel
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	initializeAfterLoad
	tagID
END
INSTANCEMETHODS
BEGIN
	editable
	updateContentsIn:
	kindDo:
	string:
	setString:
	alignment:
	initialize
	label:
	wantsFocusOnTab
	alignment
	changedString
	string
	label
	sendClickedMessage
END

CLASS TerminalInput
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	umlDefinitionString
	tagID
END
INSTANCEMETHODS
BEGIN
	addFormDataTo:
	insertCharacter:at:
	lineBeginAt:
	insertSimpleCharacter:at:
	editable:
	editable
	clearSelection
	updateSelectionBlock:
	maxLength
	wantsFocusOnTab
	isMultiLine
	showCursorAt:
	insertString:at:
	cursorAtEnd
	maxLength:
	hasReachedMaxLength
	type:
	desiresFocus
	displayString
	type
	kindDo:
	replaceCharacter:at:
	copyLine:withCharacter:at:
	lineEndAt:
	hasSelection
	readOnly:
	isPassword
	paste:
	isMaxLengthDefined
	initialize
	string:
	lineCharacterPointAt:
	updateContentsIn:
	computeCursorAfterInserting:at:
	changeSelectWith:
	stringOffset
	invalidForegroundColor
	replaceSimpleCharacter:at:
	copyLine:withoutCharacterAt:
	readOnly
	defaultControllerClass
	deleteCharacterAt:
END

CLASS TerminalTimer
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	repeat:
	repeat
	kindDo:
	addStateMessagesTo:
	delayInSeconds
	interval
	initialize
	dispatchIndexedMessage:
	interval:
	delayInSeconds:
END

CLASS TerminalText
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	string:
	inspectActions
	defaultControllerClass
	stringNoLf
	lineIndexAt:
	hyphenationString:
	stats
	tabSpacing:
	showCursorAt:
	setVisibleLinesRangeStart:
	lineSeparator
	textFromLines:
	hyphenationString
	kindDo:
	cursorAtEnd
	textlines
	lineCharacterPointAt:
	insertString:at:
	deleteCharacterAt:
	rcPointFrom:
	string:reset:
	computeLocalPoint:
	linesFromText:
	replaceSimpleCharacter:at:
	computeRowCharacterPoint:
	isMultiLine
	initialize
	initializeLinesRange
	lineEndAt:
	computeTextIndex:
	updateContentsIn:
	string
	tabSpacing
	updateFrom:to:
	insertCharacter:at:
	setVisibleLinesRangeEnd:
	bounds:
END

CLASS TerminalTableCell
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	updateBorderAppearance
END

CLASS TerminalCheckBox
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	desiresFocus
	addFormDataTo:
	updateContentsIn:
	selection:
	boundsForFocus
	tickPosition
	editable
	wantsFocusOnTab
	tickAlignment
	defaultControllerClass
	getSelection
	updateTickIn:
	kindDo:
	setSelection:
	changedSelection
	initialize
	selection
	tickAlignment:
	toggleSelection
END

CLASS TerminalRadioButton
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	groupName:
	item:
	item
	kindDo:
	addFormDataTo:
	show
	updateTickIn:
	groupName
	toggleSelection
	changedSelection
END

CLASS TerminalButton
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	umlDefinitionString
	tagID
END
INSTANCEMETHODS
BEGIN
	label:
	stringOffset
	accelerator
	isDefault
	help:
	help
	initialize
	desiresFocus
	accelerator:
	default
	label
	defaultControllerClass
	kindDo:
	wantsFocusOnTab
	updateContentsIn:
	emphasizeAccelerator
	default:
END

CLASS TerminalImageLabel
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	changedFont
	wantsFocusOnTab
	image
	scaleToFit
	updateContentsIn:
	desiresFocus
	url
	scaleToFit:
	aboutToRemoveFromTerminalForm:
	initialize
	url:
	justAddedToTerminalForm:
	kindDo:
	sendClickedMessage
	defaultControllerClass
END

CLASS TerminalCharacterGrid
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	new
	rows:columns:
END
INSTANCEMETHODS
BEGIN
	at:
	printOn:
	reInitializeBoxes:
	at:size:
	size
	row:column:put:
	columns
	rows
	copyFrom:to:
	stringAt:column:size:
	characterAt:
	row:from:to:put:
	initializeCharacters
	charactersDo:
	at:put:
	setMatrix:
	charactersOn:from:to:do:
	charactersInBounds:do:
	removeFromGraphics:
	row:column:
	addToGraphicsIn:withAppearance:
	matrix
	includesLocation:
END

CLASS TerminalDialog
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	yesPressed
	text:
	text
	cancel:
	type
	cancel
	noPressed
	open
	no:
	yes
	kindDo:
	no
	cancelPressed
	type:
	yes:
END

CLASS TerminalFontMetrics
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	printOn:
	font
	isAvailable
	bottomOffset
	fontName
	label:
	fontName:
	width
	height
	width:
	label
	height:
	bottomOffset:
END

CLASS TerminalKeyEvent
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	initializeCodeMap
	codeMap
	singleton
END
INSTANCEMETHODS
BEGIN
	isUp
	isDown
	isControlPressed:
	isTab
	isFunction:
	isFunction
	isControlPressed
	printOn:
	isAltPressed
	name
	isInsert
	code
	isLeft
	isShiftPressed
	isRight
	code:
	isPageDown
	initializeFromOSEvent:
	numericFunctionCode
	isAlt
	isEsc
	isAltPressed:
	isEnter
	isControl
	isShift
	isPageUp
	isShiftPressed:
	isCharacter
	character
	isDelete
	isBackspace
END

CLASS TerminalMacroAccessor
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	scanLine:
	events
	events:
	terminal:
	terminal
END

CLASS TerminalMenu
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	items:
	name:
	items
	kindDo:
	name
END

CLASS WordWrapper
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	on:columns:hyphenations:tabSpacing:
END
INSTANCEMETHODS
BEGIN
	columns:
	breakLines:from:to:
	tabSpacing:
	isHyphenationChar:
	hyphenationString:
	computeLinesFrom:
	lines
END

CLASS Rectangle
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	asBounds
	asRectangle
	intersectOrLine:
END

CLASS TObjectAppearance
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	asPixelValue:
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

AbtGraphicalObject subclass: #TerminalGraphicalObject
    instanceVariableNames: 'box appearance flags dirty '
    classVariableNames: ''
    poolDictionaries: 'TerminalCharacterConstants '
	comment: '' 
	category: '' !
TerminalGraphicalObject subclass: #TerminalImage
    instanceVariableNames: 'image file scaleToFit widget '
    classVariableNames: 'Cache '
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalGraphicalObject subclass: #TerminalCharacter
    instanceVariableNames: 'characterString '
    classVariableNames: ''
    poolDictionaries: 'TerminalConstants TerminalCharacterConstants '
	comment: '' 
	category: '' !
AbtAppBldrView subclass: #UITerminalComponent
    instanceVariableNames: 'cursor blinker isTyping focusWidget grid screen appearance screenStack messageHandler isInsertMode widgetUnderPoint lastClickTime lastClickPos '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
EventModel subclass: #TerminalBasicController
    instanceVariableNames: 'widget '
    classVariableNames: ''
    poolDictionaries: 'TerminalConstants '
	comment: '' 
	category: '' !
TerminalBasicController subclass: #TerminalWindowController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicController subclass: #TerminalListController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalListController subclass: #TerminalTableListController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalListController subclass: #TerminalMenuController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalMenuController subclass: #TerminalMenuBarController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalListController subclass: #TerminalChoiceListController
    instanceVariableNames: 'choiceController '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicController subclass: #TerminalInputController
    instanceVariableNames: 'visibleOffset lastCursor '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalInputController subclass: #TerminalTextController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalInputController subclass: #TerminalTableCellController
    instanceVariableNames: 'tableWidget '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicController subclass: #TerminalDropDownListController
    instanceVariableNames: 'showsList '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicController subclass: #TerminalCheckBoxController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicController subclass: #TerminalButtonController
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TWidgetAppearanceHolder subclass: #TerminalWidget
    instanceVariableNames: 'bounds visible parent '
    classVariableNames: 'TagClassMap '
    poolDictionaries: 'TerminalConstants '
	comment: '' 
	category: '' !
TerminalWidget subclass: #TerminalCompositeWidget
    instanceVariableNames: 'form widgets '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalCompositeWidget subclass: #TerminalWindow
    instanceVariableNames: 'title menuBar showStatus controller stylesheet '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalCompositeWidget subclass: #TerminalGroupBox
    instanceVariableNames: 'label '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalWidget subclass: #TerminalBasicWidget
    instanceVariableNames: 'controller enabled '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicWidget subclass: #TerminalList
    instanceVariableNames: 'items selectionIndex visibleItemRange '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalList subclass: #TerminalTableList
    instanceVariableNames: 'tableColumns headingWidgets inputWidgets selectionPolicy '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalList subclass: #TerminalMenuList
    instanceVariableNames: 'messages labelWidth characters isPopUp lineIndices '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalList subclass: #TerminalMenuBar
    instanceVariableNames: 'menus characters openMenuIndex useNative '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalList subclass: #TerminalDropDownList
    instanceVariableNames: 'maximumVisibleRows '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicWidget subclass: #TerminalLabel
    instanceVariableNames: 'string alignment '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalLabel subclass: #TerminalInput
    instanceVariableNames: 'readOnly maxLength type selectionStart selectionStop '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalInput subclass: #TerminalTimer
    instanceVariableNames: 'delayInSeconds repeat interval countThread current '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalInput subclass: #TerminalText
    instanceVariableNames: 'textlines visibleLinesRange hyphenationString tabSpacing '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalInput subclass: #TerminalTableCell
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalLabel subclass: #TerminalCheckBox
    instanceVariableNames: 'selection tickAlignment '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalCheckBox subclass: #TerminalRadioButton
    instanceVariableNames: 'groupName item '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalLabel subclass: #TerminalButton
    instanceVariableNames: 'default accelerator help '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TerminalBasicWidget subclass: #TerminalImageLabel
    instanceVariableNames: 'url image scaleToFit '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TerminalCharacterGrid
    instanceVariableNames: 'matrix '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TerminalDialog
    instanceVariableNames: 'text type yes no cancel '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TerminalFontMetrics
    instanceVariableNames: 'fontName font width height bottomOffset label '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TerminalKeyEvent
    instanceVariableNames: 'isShift isControl isAlt isFunction code '
    classVariableNames: 'CodeMap '
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TerminalMacroAccessor
    instanceVariableNames: 'events terminal '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TerminalMenu
    instanceVariableNames: 'name items '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #WordWrapper
    instanceVariableNames: 'columns lines hyphenationString tabSpacing '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!TerminalGraphicalObject class publicMethods !

platformColorValueFor: aColor 
	"Answer the color pixel value."

	^self palette nearestPixelValue: aColor asRGBColor! !

!TerminalGraphicalObject publicMethods !

appearance
	^appearance!

appearance: anObject
	appearance = anObject
		ifTrue:[^self].
	appearance := anObject.
	self localUpdate!

box

	"Answer the value of box"

	^box!

box: aValue

	"Set the value of box"

	box := aValue!

currentDisplayRect

	^box expandBy: 1@1!

drawBackgroundOn: aDrawable gc: gc

	gc setForeground: appearance background.
	aDrawable 
		fillRectangle: gc 
		x: box left 
		y: box top 
		width: box width + 1
		height: box height + 1!

height
	self subclassResponsibility!

highlight: isHiglight insertMode: isInsert 
	self mask: TCHighlight enabled: isHiglight.
	self mask: TCInsertMode enabled: isInsert!

initialize
	super initialize.
	self visible: false.
	self resetDecoration.
	dirty := false.!

initializeBox: width height: height at: theLocation
	"Pre: location is set"
	
	box := Rectangle 
				origin: ((theLocation y - 1) * width) 	@ ((theLocation x - 1) * height)
				extent: (width - 1) @ (height - 1)!

isHighlight

	^(flags bitAnd: TCHighlight) = TCHighlight!

isInsertMode

	^(flags bitAnd: TCInsertMode) = TCInsertMode!

isSelected

	^(flags bitAnd: TCSelect) = TCSelect!

localUpdate
	dirty 
		ifTrue: 
			[ super localUpdate.
			dirty := false ]!

location: aRectangle appearance: anAppearance
	"Set the value of location"
	"aRetangle holds the rectangle of characters this graphicalobject may use "
	"'normal' graphical objects just need upper-left point, image for example wants to know the whole rectangle "

	appearance := anAppearance.
	self initializeBox: appearance characterWidth height: appearance characterHeight at: aRectangle origin!

mask: mask enabled: boolean
	"No update but dirty is set"
	
	boolean
		ifTrue: [flags := flags bitOr: mask]
		ifFalse: [flags := (flags bitOr: mask) bitXor: mask].
	dirty := true.!

parent: aParent
	super parent: aParent.
	aParent notNil
		ifTrue:[self visible: true].!

primitiveDrawOn: aDrawable gc: gc 
	self subclassResponsibility!

resetDecoration
	flags := 0.!

select: boolean 
	self mask: TCSelect enabled: boolean!

setAppearance: anObject
	"No update but set dirty flag"
	appearance := anObject.
	dirty := true.!

showBorderBottom

	^(flags bitAnd: TCBottom) = TCBottom!

showBorderBottom: boolean
	self mask: TCBottom enabled: boolean!

showBorderLeft

	^(flags bitAnd: TCLeft) = TCLeft!

showBorderLeft: boolean
	self mask: TCLeft enabled: boolean!

showBorderRight

	^(flags bitAnd: TCRight) = TCRight!

showBorderRight: boolean
	self mask: TCRight enabled: boolean!

showBorderTop

	^(flags bitAnd: TCTop) = TCTop!

showBorderTop: boolean
	self mask: TCTop enabled: boolean!

showCheckBox

	^(flags bitAnd: TCCheckBox) = TCCheckBox!

showCheckBox: boolean
	self mask: TCCheckBox enabled: boolean!

showCheckboxTick

	^(flags bitAnd: TCCheckTick) = TCCheckTick!

showCheckboxTick: boolean
	self mask: TCCheckTick enabled: boolean!

showChoiceMarker

	^(flags bitAnd: TCChoiceMarker) = TCChoiceMarker!

showChoiceMarker: boolean
	self mask: TCChoiceMarker enabled: boolean!

showCR

	^(flags bitAnd: 8) = 8!

showCR: boolean
	self mask: TCReturn enabled: boolean!

showFocus

	^(flags bitAnd: TCHasFocus ) = TCHasFocus!

showFocus: boolean
	self mask: TCHasFocus enabled: boolean!

showGrid

	^(flags bitAnd: TCGrid) = TCGrid!

showGrid: boolean
	self mask: TCGrid enabled: boolean!

showRadioCircle

	^(flags bitAnd: TCRadioCircle) = TCRadioCircle!

showRadioCircle: boolean
	self mask: TCRadioCircle enabled: boolean!

showRadioTick

	^(flags bitAnd: TCRadioTick) = TCRadioTick!

showRadioTick: boolean
	self mask: TCRadioTick enabled: boolean!

showSeparatorBottomLeft

	^(flags bitAnd: TCBottomLeft) = TCBottomLeft!

showSeparatorBottomLeft: boolean
	self mask: TCBottomLeft enabled: boolean!

showSeparatorBottomRight

	^(flags bitAnd: TCBottomRight) = TCBottomRight!

showSeparatorBottomRight: boolean
	self mask: TCBottomRight enabled: boolean!

showSeparatorHorizontal

	^(flags bitAnd: TCHorizontal) = TCHorizontal!

showSeparatorHorizontal: boolean
	self mask: TCHorizontal enabled: boolean!

showSeparatorTopLeft

	^(flags bitAnd: TCTopLeft) = TCTopLeft!

showSeparatorTopLeft: boolean
	self mask: TCTopLeft enabled: boolean!

showSeparatorTopRight

	^(flags bitAnd: TCTopRight) = TCTopRight!

showSeparatorTopRight: boolean
	self mask: TCTopRight enabled: boolean!

showSeparatorVertical

	^(flags bitAnd: TCVertical) = TCVertical!

showSeparatorVertical: boolean
	self mask: TCVertical enabled: boolean!

showUnderline

	^(flags bitAnd: TCUnderline) = TCUnderline!

showUnderline: boolean
	self mask: TCUnderline enabled: boolean!

width
	self subclassResponsibility! !
	

!TerminalImage class publicMethods !

for: aWidget
	| instance |
	instance := self new.
	instance widget: aWidget.
	^instance!

release
	Cache := nil! !

!TerminalImage publicMethods !

file
	^file!

file: anObject
	file := anObject!

hasImage
	"Strange but correct"
	
	image = 'error'
		ifTrue: [ ^false ].
	self image = 'error'
		ifTrue: [ ^false ].
	self image isNil
		ifTrue: [ ^false ].
	^true
	!

height
	^self hasImage
		ifTrue: [ self image height ]
		ifFalse:[ 4 ]!

image
	image ifNil:[ self loadImage].
	^image!

image: anObject
	image := anObject!

initializeBox: width height: height at: theLocation
	"Pre: location is set"
	
	| origin |
	origin := ((theLocation y - 1) * width) 	@ ((theLocation x - 1) * height).
	box := origin extent: (self width truncateTo: width) @ (self height truncateTo: height)!

initializeImageBox: width height: height at: theLocation
	"Pre: location is set"
	" Location is a rectangle because the image needs to know how large it can show itself "
	
	| origin |
	origin := ((theLocation origin y - 1) * width) 	@ ((theLocation origin x - 1) * height).
	box := origin corner: ((theLocation corner y ) * width) 	@ ((theLocation corner x ) * height)!

loadImage
	"VA Specific"
	
	self file isNil ifTrue:[ ^self ].
	Cache ifNil: [Cache := Dictionary new ].
	[image := Cache
		at: self file 
		ifAbsentPutUsing:[self class loadImageFromFile: self file]
	] when: ExError do:[ :err | image := 'error' ]!

location: aValue appearance: anAppearance
	"Set the value of location"
	" initializeImageBox: expects a rectangle instead of a point because an image should know how
	  large it is. "

	appearance := anAppearance.
	self initializeImageBox: appearance characterWidth height: appearance characterHeight at: aValue !

primitiveDrawOn: aDrawable gc: gc 

	self hasImage 
		ifTrue: 
			[ self scaleToFit
				ifTrue: 
					[ ^aDrawable 
						putDeviceIndependentImage: gc 
						image: self image 
						srcRect: ( 0 @ 0 extent: ( self width min: box width ) @ ( self height min: box height ) ) 
						destRect: self box ]
				ifFalse: 
					[ super drawBackgroundOn: aDrawable gc: gc.
					^aDrawable 
						putDeviceIndependentImage: gc 
						image: self image 
						srcRect: ( 0 @ 0 extent: self width - 1 @ ( self height - 1 ) ) 
						destRect: ( self box origin extent: (( self width min: box width ) - 1) @ (( self height min: box height ) - 1 ) ) ] ]. 	"draw cross and display missing file"
	gc setForeground: 15.
	aDrawable 
		drawRectangle: gc 
		x: self box left 
		y: self box top 
		width: self box width 
		height: self box height.
	aDrawable 
		drawLine: gc 
		x1: self box left 
		y1: self box top 
		x2: self box right 
		y2: self box bottom.
	aDrawable 
		drawLine: gc 
		x1: self box left 
		y1: self box bottom 
		x2: self box right 
		y2: self box top.
	aDrawable 
		drawString: gc 
		x: self box left 
		y: self box top + 12 
		string: self file asString!

redraw
	dirty := true.
	self localUpdate!

scaleToFit
	^scaleToFit!

scaleToFit: anObject
	scaleToFit := anObject!

widget
	^widget!

widget: newWidget
	widget := newWidget!

width
	^self hasImage
		ifTrue: [ self image width ]
		ifFalse:[ 4 ]! !
	

!TerminalCharacter class publicMethods !

new

	"Answer a new instance of the receiver and initialize it"

	^super new initialize! !

!TerminalCharacter publicMethods !

character

	"Answer the value of character"

	^characterString at: 1!

character: aChar
	"Set the value of character, no update"

	(characterString at: 1) == aChar ifTrue: [ ^self ].
	characterString at: 1 put: aChar.
	self select: false.!

clearSeparators
	self mask: TCTopLeft | TCTopRight | TCBottomLeft | TCBottomRight | TCVertical | TCHorizontal  enabled: false!

clearWithAppearance: anAppearance
	self clearWithAppearance: anAppearance refresh: true!

clearWithAppearance: anAppearance refresh: shouldRefresh
	appearance := anAppearance.
	self resetDecoration.
	self character: $ .
	shouldRefresh 
		ifTrue:
			[dirty := true. 
			self localUpdate]!

drawBorderOn: aDrawable gc: gc
	
	| x  y decorationColor |
	decorationColor := (self showFocus 
		ifTrue:[appearance focusColor] 
		ifFalse:[appearance borderColor]).
	decorationColor isNil ifTrue: [ ^self].
	gc setForeground: decorationColor.
	self showBorderLeft
		ifTrue: [aDrawable drawLine: gc x1: box left y1: box bottom x2: box left y2: box top].
	self showBorderTop
		ifTrue: [aDrawable drawLine: gc x1: box left y1: box top x2: box right y2: box top].		
	self showBorderRight
		ifTrue: [aDrawable drawLine: gc x1: box right y1: box top x2: box right y2: box bottom].
	self showBorderBottom
		ifTrue: [aDrawable drawLine: gc x1: box right y1: box bottom x2: box left y2: box bottom].								!

drawCharacterStringOn: aDrawable gc: gc 

	| showSelected displayString char |
	showSelected := self isSelected.
	gc setBackground: ( showSelected
		ifTrue: [ self appearance selectionBackground ]
		ifFalse: [ self appearance background ] ).
	gc setForeground: ( showSelected
		ifTrue: [ self appearance selectionForeground ]
		ifFalse: [ self appearance foreground ] ).
	displayString := characterString.
	char := displayString at: 1.
	(char == SPACE or:[char == TAB])
		ifTrue: [ ^self ].
	char == CR 
		ifTrue: 
			[ self showCR
				ifTrue: [ displayString := String with: (Character value: 170) ]
				ifFalse: [ ^self ] ].
	aDrawable 
		drawString: gc 
		boundedBy: box
		string: displayString!

drawDecorationOn: aDrawable gc: gc
	
	self drawBorderOn: aDrawable gc: gc.
	self drawSeparationBorderOn: aDrawable gc: gc!

drawGraphicsOn: aDrawable gc: gc 

	self showUnderline 
		ifTrue: 
			[ self isSelected
				ifTrue: [ gc setForeground: appearance selectionForeground ]
				ifFalse: [ gc setForeground: appearance foreground ].
			^aDrawable 
				drawLine: gc 
				x1: box left 
				y1: box bottom - 2 
				x2: box right 
				y2: box bottom - 2 ].
	self showCheckBox 
		ifTrue: 
			[  | points center radius | 
			center := box center.
			radius := box width // 2.
			gc setForeground: appearance foreground.
			aDrawable 
				drawRectangle: gc 
				x: center x - radius 
				y: center y - radius 
				width: box width 
				height: box width. 
			
			self showCheckboxTick 
				ifTrue: 
					[ aDrawable 
						fillRectangle: gc 
						x: center x - radius 
						y: center y - radius 
						width: box width 
						height: box width ] ].
	self showRadioCircle 
		ifTrue: 
			[  | points center radius | 
			center := box center.
			radius := self width // 2 - 1.
			gc setForeground: appearance foreground.
			self showRadioTick
				ifTrue: 
					[ aDrawable 
						fillArc: gc 
						x: box left 
						y: center y - radius 
						width: ( box width setBit: 1 ) 
						height: ( box width setBit: 1 ) 
						angle1: 0 
						angle2: 360 * 66 ]
				ifFalse: 
					[ aDrawable 
						drawArc: gc 
						x: box left 
						y: center y - radius 
						width: box width 
						height: box width 
						angle1: 0 
						angle2: 360 * 66 ] ].
	self showChoiceMarker 
		ifTrue: 
			[  | points | 
			points := Array new: 4.
			points at: 1 put: box leftCenter.
			points at: 2 put: box bottomCenter.
			points at: 3 put: box rightCenter.
			points at: 4 put: box leftCenter.
			points := points collect: [ :p | p + ( 0 @ -2 ) ].
			self isSelected
				ifTrue: [ gc setForeground: appearance selectionForeground ]
				ifFalse: [ gc setForeground: appearance foreground ].
			^aDrawable 
				fillPolygon: gc 
				points: points 
				shape: 2 
				mode: 0 ]!

drawGridOn: aDrawable gc: gc 
	gc setForeground: 0.
	aDrawable 
		drawRectangle: gc
		x: box left
		y: box top
		width: self width - 1
		height: self height - 1!

drawHighlightOn: aDrawable gc: gc
	"Appearance depends on INSERTMODE"
	
	| h |
	gc setForeground: appearance selectionBackground.
	h := self isInsertMode
		ifTrue: [ 4 ] ifFalse: [ 8 ].
	aDrawable 
		fillRectangle: gc 
		x: box left 
		y: box bottom - h
		width: self width 
		height: h.
	self isInsertMode
		ifFalse: [ self drawCharacterStringOn: aDrawable gc: gc]!

drawSelectionBackgroundOn: aDrawable gc: gc

	gc setForeground: appearance selectionBackground.
	aDrawable 
		fillRectangle: gc 
		x: box left 
		y: box top 
		width: self width 
		height: self height!

drawSeparationBorderOn: aDrawable gc: gc
	
	| x  y|
	gc setForeground: appearance foreground.
	self showSeparatorTopLeft
		ifTrue: 
			[aDrawable drawLine: gc x1: (x := box center x) y1:(y := box center y) x2: x y2: box bottom.
			aDrawable drawLine: gc x1: x y1: y x2: box right y2: y].		
	self showSeparatorTopRight
		ifTrue: 
			[aDrawable drawLine: gc x1: box left y1: (y := box center y) x2: (x := box center x) y2: y.
			aDrawable drawLine: gc x1: x y1: y x2: x y2: box bottom].							
	self showSeparatorBottomRight
		ifTrue: 
			[aDrawable drawLine: gc x1: (x := box topCenter x) y1: box top x2: x y2: (y := box center y).
			aDrawable drawLine: gc x1: box left y1: y x2: x y2: y].		
	self showSeparatorBottomLeft
		ifTrue: 
			[aDrawable drawLine: gc x1: (x := box topCenter x) y1: box top x2: x y2: (y := box center y).
			aDrawable drawLine: gc x1: x y1: y x2: box right y2: y].			
	self showSeparatorVertical
		ifTrue: [aDrawable drawLine: gc x1: (x := box center x) y1: box top x2: x y2: box bottom].
	self showSeparatorHorizontal
		ifTrue: [aDrawable drawLine: gc x1: box left y1: box leftCenter y x2: box right y2: box rightCenter y].
				
								!

height
	^box height + 1!

initialize
	super initialize.
	characterString :=  String with: $ !

primitiveDrawOn: aDrawable gc: gc 

	self isSelected
		ifTrue: [ self drawSelectionBackgroundOn: aDrawable gc: gc ]
		ifFalse: [ appearance background notNil 
			ifTrue: [ self drawBackgroundOn: aDrawable gc: gc ] ].
	characterString notEmpty 
		ifTrue: [ self drawCharacterStringOn: aDrawable gc: gc ].
	self drawDecorationOn: aDrawable gc: gc.
	self drawGraphicsOn: aDrawable gc: gc.
	self isHighlight 
		ifTrue: [ self drawHighlightOn: aDrawable gc: gc ].
	self showGrid 
		ifTrue: [ self drawGridOn: aDrawable gc: gc ]!

printOn: aStream
	aStream nextPut: $[ ;nextPut: self character ;nextPut: $]!

string
	^characterString!

width
	^box width + 1! !
	

!UITerminalComponent class publicMethods !

show: aTerminalWindow
	| term |
	term := self new.
	term show: aTerminalWindow.
	term open.	
	^term! !

!UITerminalComponent publicMethods !

activeCharacter
	"Pre: cursor not nil"
	
	^self grid row: cursor x column: cursor y!

checkGridSize
	"Pre: screen notNil
	Check whether the grid has sufficient character to display the current screen.
	If not replace with a new grid"
	
	(self screen rows ~= self grid rows
		or:[self screen columns ~= self grid columns])
			ifTrue: 
				[ self grid removeFromGraphics: self.
				self reInitializeGrid]!

cursor
	^cursor!

dispatchKeyEvent: event to: controller 
	"Answer whether the event was consumed"

	event isAltPressed
		ifTrue: [ ^controller keyAlt: event ].
	event isInsert
		ifTrue: [self isInsertMode: self isInsertMode not. ^true].
	event isShift
		ifTrue: [ ^controller keyShift ].
	event isUp
		ifTrue: [ ^controller keyUp ].
	event isDown
		ifTrue: [ ^controller keyDown ].
	event isLeft
		ifTrue: [^event isShiftPressed ifTrue: [controller keyShiftLeft  ] ifFalse:[controller keyLeft ]].
	event isRight
		ifTrue: [^event isShiftPressed ifTrue: [controller keyShiftRight  ] ifFalse:[controller keyRight ]].
	event isBackspace
		ifTrue: [ ^controller keyBackspace ].
	event isEnter
		ifTrue: [ ^controller keyReturn ].
	event isDelete
		ifTrue: [ ^controller keyDelete ].
	event isHome
		ifTrue: 
			[ event isControlPressed
				ifTrue: [ ^controller keyCtrlHome ]
				ifFalse: [ ^controller keyHome ] ].
	event isEnd
		ifTrue: 
			[ event isControlPressed
				ifTrue: [ ^controller keyCtrlEnd ]
				ifFalse: [ ^controller keyEnd ] ].
	event isNumericFunction
		ifTrue: [ ^controller keyFunction: event numericFunctionCode shift: event isShift].
	event isPageUp
		ifTrue: [ ^controller keyPageUp ].
	event isPageDown
		ifTrue: [ ^controller keyPageDown ].
	event isEsc
		ifTrue: [ ^controller keyCancel ].
	event isTab
		ifTrue: 
			[ event isShiftPressed
				ifTrue: [ ^controller keyShiftTab ]
				ifFalse: [ ^controller keyTab ] ]. 
	event isCharacter 
		ifFalse: [ ^true ].
	event character isDoubleByte
		ifTrue: [ ^true ]. "not supported"
	^event code notNil
		ifTrue: 
			[ event isControlPressed
				ifTrue: [ controller keyCtrlCharacter: event character ]
				ifFalse: [ controller keySimpleCharacter: event character ] ]!

displayScreen
	
	self screen justAddedToTerminalForm: self.
	self screen show!

focusWidget
	^focusWidget!

gettingFocus

	self focusWidget isNil 
		ifTrue: 
			[self screen isNil 
				ifTrue: [^self ]. 
			self screen setInputFocus]
		ifFalse:[self focusWidget controller gettingFocus]!

grid
	self screen isNil 
		ifTrue: [ ^TerminalCharacterGrid new ].
	grid isNil 
		ifTrue: [ self reInitializeGrid ].
	^grid!

handleButtonEvent: event pressed: isPressed

	| characterWidth characterHeight rcPoint oldWidgetUnderPoint |
	self screen isNil ifTrue: [ ^self ].
	characterWidth := self screen appearance characterWidth.
	characterHeight := self screen appearance characterHeight.
	rcPoint := event  y // characterHeight + 1 @ (event  x // characterWidth + 1).
	" 
	WHY? "
	oldWidgetUnderPoint := widgetUnderPoint. 
	widgetUnderPoint := self screen widgetUnderPoint: rcPoint.
	self screen = widgetUnderPoint 
		ifTrue: [ ^self ].
	widgetUnderPoint isNil
		ifTrue: [ ^self ].
	widgetUnderPoint desiresFocus
		ifFalse: [ ^self ].
	widgetUnderPoint enabled
		ifFalse:[ ^self ].
	widgetUnderPoint visible
		ifFalse:[^self].
	self setFocusWidget: widgetUnderPoint. 
	self sendButtonEvent: event pressed: isPressed at: rcPoint to: widgetUnderPoint!

handleKeyEvent: event

	| controller widgetUnderControl |
	widgetUnderControl := focusWidget.
	
	[ widgetUnderControl isNil
		ifTrue: [ widgetUnderControl := self screen ].
	controller := widgetUnderControl controller.
	controller isNil
		ifTrue: [^self].
	controller widget enabled
		ifFalse:[^self].
	(self dispatchKeyEvent: event to: controller) ~~ false
		ifTrue: [^self].
	widgetUnderControl := widgetUnderControl parent.
	true] whileTrue:[]!

hideScreen
	"We know this is because we are about to show a new screen.
	So we can safely hide the current screen without a refresh"
	
	self screen isNil ifTrue:[^self].
	self screen hide: false.
	self screen aboutToRemoveFromTerminalForm: self!

initialize
	screenStack := OrderedCollection new.
	self isInsertMode: true.
	super initialize.!

isInsertMode
	^isInsertMode!

isInsertMode: anObject
	isInsertMode := anObject!

killBlinker
	blinker isNil
		ifFalse: 
			[ blinker terminate.
			blinker := nil ]!

losingFocus
	"Do not loose the last widget that has focus.
	It wil get focus again once the window has it"
	
	focusWidget isNil ifTrue: [ ^self ].
	focusWidget controller losingFocus!

messageHandler
	^messageHandler!

messageHandler: anObject
	messageHandler := anObject!

postOpen
	self displayScreen!

preClose
	self killBlinker.
	self release.!

preOpen
	isTyping := false.!

reInitializeGrid
	grid := TerminalCharacterGrid rows: self screen maxRow columns: self screen maxColumn.
	grid addToGraphicsIn: self withAppearance: self screen appearance!

release
	super release.
	self releaseScreen.!

releaseScreen

	self screen release.
	screen := nil!

requestCursorAt: point 
	"Pre: point notNil"

	isTyping := true.
	"clear the old"
	cursor notNil
		ifTrue:
			[(self grid row: cursor x column: cursor y)
				highlight: false insertMode: self isInsertMode
				;localUpdate].
		
	"set the new"
	cursor := point.
	
	"update the new"
	(self grid row: cursor x column: cursor y)
		highlight: true insertMode: self isInsertMode
		;localUpdate.
	isTyping := false!

screen
	"Enter the new method definition below and click 'Resume'."
	^screen!

screen: aTWindow

 	screen := aTWindow!

sendButtonEvent: event pressed: isPressed at: rcPoint to: aWidget 

	( self isDoubleClick: event )
		ifTrue: 
			[ aWidget controller buttonDoubleClickAt: rcPoint ]
		ifFalse: 
			[ isPressed
				ifTrue: [ aWidget controller buttonPressAt: rcPoint ]
				ifFalse: [ aWidget controller buttonReleaseAt: rcPoint ] ]!

setFocusWidget: aWidget 

	focusWidget = aWidget 
		ifTrue: [ ^self ].
	focusWidget notNil 
		ifTrue: 
			["#losingFocus echo: focusWidget controller."
			focusWidget visible
				ifTrue:[focusWidget controller losingFocus]].
	focusWidget := aWidget.
	aWidget isNil 
		ifTrue: [ ^self ].
	aWidget visible 
		ifFalse:[^focusWidget := nil].
	#gettingFocus echo: focusWidget controller.
	focusWidget controller gettingFocus 	!

show: aTScreen 

	( self isOpened
		and: [ self screen notNil ] ) 
		ifTrue: 
			[ self stopBlinker.
			focusWidget := nil.  "No setFocusWidget"
			self hideScreen.
			self screen release.
			self screen form: nil ].
	self status: ''.
	self screen: aTScreen.
	self screen notNil 
		ifTrue: 
			[ self screen form: self.
			self title: aTScreen title asString.
			self isOpened 
				ifTrue: 
					[ self checkGridSize.
					self checkViewExtent. 
					self displayScreen ] ]!

startBlinker
	self stopBlinker.
	blinker := [
		[self toggleHighlightCursor.
		(Delay forSeconds: 0.2) wait] repeat
		] fork!

stopBlinker
	self killBlinker.
	focusWidget isNil
		ifTrue: [ ^self ].
	cursor isNil 
		ifTrue: [ ^self ].
	cursor < self grid size
		ifFalse:[^self].
	self activeCharacter highlight: false insertMode: true
!

toggleHighlightCursor
	"Pre: cursor notNil"
	
	(self grid includesLocation: cursor) ifFalse:[^self]. "safe"
	isTyping
		ifTrue: [ ( self grid row: cursor x column: cursor y )  highlight: true insertMode: self isInsertMode ; localUpdate ]
		ifFalse: [  | ch | 
		( ch := self grid row: cursor x column: cursor y )  highlight: ch isHighlight not insertMode: self isInsertMode ; localUpdate ]! !
	

!TerminalBasicController class publicMethods !

new
	^super new initialize.!

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalBasicController">
	<attribute name="lastCursor" type="Point"/>
	<attribute name="widget" type="TerminalWidget"/>
	<operation name="cursor" return="Point"/>
	<operation name="form" return="UITerminalComponent"/>
	<operation name="keyAlt" return="Boolean"/>
	<operation name="keyBackspace" return="Boolean"/>
	<operation name="keyCancel" return="Boolean"/>
	<operation name="keyCtrlA" return="Boolean"/>
	<operation name="keyCtrlCharacter:" return="Boolean">
		<parameter type="Character"/>
	</operation>
	<operation name="keyCtrlEnd" return="Boolean"/>
	<operation name="keyCtrlG" return="Boolean"/>
	<operation name="keyCtrlHome" return="Boolean"/>
	<operation name="keyCtrlP" return="Boolean"/>
	<operation name="keyCtrlR" return="Boolean"/>
	<operation name="keyCtrlV" return="Boolean"/>
	<operation name="keyDelete" return="Boolean"/>
	<operation name="keyDown" return="Boolean"/>
	<operation name="keyEnd" return="Boolean"/>
	<operation name="keyHome" return="Boolean"/>
	<operation name="keyLeft" return="Boolean"/>
	<operation name="keyPageDown" return="Boolean"/>
	<operation name="keyPageUp" return="Boolean"/>
	<operation name="keyReturn" return="Boolean"/>
	<operation name="keyRight" return="Boolean"/>
	<operation name="keyShift" return="Boolean"/>
	<operation name="keyShiftLeft" return="Boolean"/>
	<operation name="keyShiftRight" return="Boolean"/>
	<operation name="keyShiftTab" return="Boolean"/>
	<operation name="keySimpleCharacter:" return="Boolean">
		<parameter type="Object"/>
	</operation>
	<operation name="keyTab" return="Boolean"/>
	<operation name="keyUp" return="Boolean"/>
	<operation name="localCursor" return="Point"/>
	<operation name="requestCursorAt:" return="Point">
		<parameter type="Point"/>
	</operation>
	<operation name="requestCursorOffset:" return="Point">
		<parameter type="Point"/>
	</operation>
	<operation name="requestTab" return="Boolean"/>
	<operation name="requestTabForward:" return="Boolean">
		<parameter type="Boolean"/>
	</operation>
</class>'! !

!TerminalBasicController publicMethods !

buttonDoubleClickAt: rcPoint
	#double echo: rcPoint!

buttonPressAt: rcPoint
	"do nothing"!

buttonReleaseAt: rcPoint
	"Button activates the message send immediately"
	
	self widget sendClickedMessage!

defaultActionRequested
	"Find the default action button and make it perform any action"
	
	| container |
	container := self widget parent.
	container isNil ifTrue: [ ^self ].
	[ container defaultActionButton
		ifNotNil:[ :button | ^button sendClickedMessage ].
	container := container parent.
	container isNil ] whileFalse.!

form
	"adapter"
	^self widget form!

gettingFocus
	self widget updateFocus: true.
	self widget triggerEvent: TWidgetEvent gettingFocus!

initialize!

keyAlt: anEvent
	
	" Currently, check for the F"
	anEvent character == $f
		ifTrue: [ ^self form changeFont ].
		
	anEvent character == $g
		ifTrue: [ ^self form grid charactersDo:[ :ch | ch showGrid: ch showGrid not ;localUpdate ]]!

keyBackspace!

keyCancel
	"Esc button"
	self keyTab!

keyCtrlCharacter: char
	"To enable keybinding support for developper, an event is triggered by the widget.
	The event name is composed.  control_<lowercase char>"
	
	self promoteKeyEvent: ('control_' copyWith: char) asSymbol.!

keyCtrlEnd!

keyCtrlHome!

keyDelete
	"no action"!

keyDown
	"no action"!

keyEnd!

keyFunction: id shift: isShift
	"Forward to the WindowController"
	^self widget topWidget controller keyFunction: id shift: isShift!

keyHome!

keyLeft
	"no action"!

keyPageDown!

keyPageUp!

keyReturn
	self widget triggerEvent: TWidgetEvent enter.
	self defaultActionRequested!

keyRight
	"no action"!

keyShift!

keyShiftLeft!

keyShiftRight!

keyShiftTab
	self requestTabForward: false!

keySimpleCharacter: aCharacter!

keyTab
	self requestTab!

keyUp
	"no action"!

losingFocus
	self widget visible
		ifTrue:[self widget updateFocus: false].
	self widget triggerEvent: TWidgetEvent losingFocus!

printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' for: '.
	self widget printOn: aStream!

promoteKeyEvent: eventSymbol
	" Trigger event everywhere it could be interesting "

	self triggerEvent: eventSymbol.
	self widget triggerEvent: eventSymbol.!

requestTab
	self requestTabForward: true!

requestTabForward: isForward
	self widget parent tabRequestedFrom: self widget forward: isForward!

widget
	^widget!

widget: anObject
	widget = anObject
		ifTrue: [^self].
	widget := anObject.
	anObject controller: self!

windowController
	^self widget topWidget controller! !
	

!TerminalWindowController publicMethods !

keyFunction: id shift: isShift 
	| event |
	event := isShift 
				ifTrue: [TWidgetEvent shiftF: id]
				ifFalse: [TWidgetEvent F: id].
	self promoteKeyEvent: event!

requestTabForward: isForward
	self widget tabRequestedFrom: self widget forward: isForward! !
	

!TerminalListController class publicMethods !

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalListController" superclass="TerminalBasicController">
	<operation name="canRotateSelection" return="Boolean"/>
</class>'! !

!TerminalListController publicMethods !

buttonDoubleClickAt: rcPoint
	self buttonPressAt: rcPoint.
	self defaultActionRequested.!

buttonReleaseAt: rcPoint 

	"Find selectionIndex for row of rcPoint. 
	Selection index is zero-based.
	Compensate for visibleItemRange"

	| index |
	index := rcPoint x - self widget topRow.
	( index between: 0 and: self widget items size - 1 ) 
		ifTrue: [ self buttonSelectedIndex: index ]!

buttonSelectedIndex: index 
	| visibleIndex |
	visibleIndex := index + self widget visibleItemRange first.
	self widget selectionIndex: visibleIndex!

canRotateSelection
	^false!

keyDown
	
	| indexOrNil |
	( indexOrNil := self widget nextSelectionIndexOrNil ) isNil
		ifTrue: [^self].
	self widget selectionIndex: indexOrNil!

keyEnd
	self widget selectionIndex: self widget items size - 1!

keyHome
	self widget selectionIndex: 0!

keyPageDown
	self widget selectionIndex: ((self widget selectionIndex + self widget rows - 1) min: self widget items size - 1)!

keyPageUp
	self widget selectionIndex: ((self widget selectionIndex - self widget rows + 1) max: (self widget hasItems ifTrue: [0] ifFalse:[-1]))!

keySimpleCharacter: aCharacter 

	"Find the next item in the list the starts with aCharacter"

	| here item start |
	self widget items isEmpty 
		ifTrue: [ ^self ].
	start := String with: aCharacter.
	here := self widget selectionIndex.
	here = -1
		ifTrue: 
			[ here := 0 ]
		ifFalse: 
			[ item := self widget printableItemAt: here.
			( item startsWith: start ) 
				ifTrue: [ here := here + 1 ] ].
	[ here < self widget items size ]
		whileTrue: 
			[ item := self widget printableItemAt: here.
			( item startsWith: start ) 
				ifTrue: [ ^self widget selectionIndex: here ].
			here := here + 1 ]!

keyUp
	
	| indexOrNil |
	( indexOrNil := self widget previousSelectionIndexOrNil ) isNil
		ifTrue: [^self].
	self widget selectionIndex: indexOrNil! !
	
	

!TerminalMenuController publicMethods !

canRotateSelection
	^true!

indexForCharacter: char

	"Find the first label whose first character matches the character.
	Answer the index of the item, or 0 ifnone"

	^self widget characters findFirst: [ :each | each notNil and:[each asLowercase = char asLowercase]]!

keyReturn

	self widget activateRequestAt: self widget selectionIndex !

keySimpleCharacter: char 

	"Find the first label whose first character matches the character.
	Answer whether the event caused activation"

	| index |
	index := self indexForCharacter: char.
	index = 0 
		ifTrue: [ ^false ].
	^self widget activateRequestAt: index!

messageOnReturn

	^self widget messageToSendAt: self widget selectionIndex ! !
	

!TerminalMenuBarController class publicMethods !

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalMenuBarController" superclass="TerminalMenuController">
</class>'! !

!TerminalMenuBarController publicMethods !

gettingFocus
	"Indices are ZERO-based"
	
	| index |
	index := self widget hasOpenMenu
		ifTrue: [self widget openMenuIndex]
		ifFalse: [1 min: self widget menus size].
	self widget selectionIndex: index - 1.
	super gettingFocus.!

keyCancel

	self widget hasOpenMenu 
		ifTrue: [ self widget hideMenu ].
	self widget selectionIndex: 0.
	self requestTab!

keyDown
	self widget hasOpenMenu
		ifTrue: [ self widget currentMenu controller keyDown ]
		ifFalse: [ self widget showMenu ]!

keyLeft
	self widget hasOpenMenu
		ifTrue: [ self widget showPreviousMenu ]
		ifFalse: [ super keyUp ]!

keyPageDown
	self widget hasOpenMenu
		ifTrue: [ self widget currentMenu controller keyPageDown ]!

keyPageUp
	self widget hasOpenMenu
		ifTrue: [ self widget currentMenu controller keyPageUp ]!

keyReturn

	self widget hasOpenMenu
		ifTrue: 
			[ | messageOrNil |
			messageOrNil :=self widget currentMenu controller messageOnReturn.
			self widget hideMenu.
			self widget selectionIndex: -1.
			"do not know what to do with focus"
			self requestTab.
			messageOrNil isNil
				ifFalse: [messageOrNil sendUsingHandler: self widget messageHandler]]
		ifFalse: 
			[ self widget showMenu ]!

keyRight
	self widget hasOpenMenu
		ifTrue: [ self widget showNextMenu ]
		ifFalse: [ super keyDown ]!

keySimpleCharacter: char 

	self widget hasOpenMenu
		ifTrue: 
			[ | index |
			index := self widget currentMenu controller indexForCharacter: char.
			index = 0 ifTrue: [ ^self ].
			self widget currentMenu selectionIndex: index - 1.
			self keyReturn ]
		ifFalse: 
			[ super keySimpleCharacter: char.
			self keyReturn ]!

keyUp
	self widget hasOpenMenu 
		ifTrue: [ self widget currentMenu controller keyUp ]!

losingFocus
	super losingFocus.
	self widget hasOpenMenu 
		ifTrue: [ self widget hideMenu ].
	self widget selectionIndex: -1! !
	

!TerminalChoiceListController publicMethods !

buttonDoubleClickAt: rcPoint
	self buttonReleaseAt: rcPoint!

buttonReleaseAt: rcPoint 
	super buttonReleaseAt: rcPoint.
	self choiceController keyReturn.!

choiceController
	^choiceController!

choiceController: anObject
	choiceController := anObject!

losingFocus
	super losingFocus.
	self choiceController hideList! !
	

!TerminalInputController class publicMethods !

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalInputController" superclass="TerminalBasicController">
	<operation name="isCursorHome" return="Boolean"/>
	<operation name="isCursorOnFirstColumn" return="Boolean"/>
	<operation name="isCursorOnFirstRow" return="Boolean"/>
	<operation name="isInsertMode" return="Boolean"/>
</class>'! !

!TerminalInputController publicMethods !

buttonReleaseAt: rcPoint 

	"Check outside editable space"

	| end goto |
	self widget hasSelection
		ifTrue: [ self widget clearSelection ].
	goto := rcPoint.
	end := self widget cursorAtEnd.
	rcPoint x > end x 
		ifTrue: 
			[ self requestCursorAt: self widget cursorAtEnd.
			^self keyRight ].
	end := self widget lineEndAt: rcPoint.
	rcPoint y > end y 
		ifTrue: 
			[ self requestCursorAt: self widget cursorAtEnd.
			^self keyRight ].
	self requestCursorAt: goto!

characterOffset
	^visibleOffset!

characterOffset: int
	visibleOffset := int!

cursor
	"adaptor"
	^self lastCursor!

gettingFocus
	"The widget's contents might have changed while out of focus.
	This means that the cursor can be invalid. This detection is done in buttonSelectAt"
	
	self widget desiresFocus ifFalse:[^self].
	self buttonReleaseAt: self lastCursor.
	super gettingFocus.
	self form startBlinker!

initialize
	visibleOffset := 0.!

isCursorAtEnd
	
	^self form cursor y = self widget maxColumn!

isCursorBeforeEndOfLine

	| column |
	column := self widget localColumnAt: self widget form cursor.
	^self characterOffset + column < self widget string size!

isCursorBeyondLine
	| lc |
	lc := self widget localColumnAt: self form cursor.
	^lc > (self widget string size - self visibleOffset)!

isCursorHome
	^(self widget localColumnAt: self form cursor) = 1!

isCursorInserting
	^self form isInsertMode!

isCursorOnEndOfLine

	| column |
	self widget hasReachedMaxLength
		ifFalse:[^false].
	"full but is the cursor at end?"
	column := self widget localColumnAt: self widget form cursor.
	^column + self characterOffset = self widget string size
	!

isCursorOnFirstColumn
	^self cursor y = self widget bounds origin y!

isCursorOnFirstRow
	^self cursor x = self widget minRow!

isInsertMode
	^self form isInsertMode!

keyBackspace
	self isCursorOnFirstColumn
		ifTrue: 
			[ self isCursorOnFirstRow
				ifTrue: 
					[ self characterOffset > 0 
						ifTrue: 
							[ self characterOffset: self characterOffset - 1.
							^self widget updateContents ] ]
				ifFalse: 
					[ self keyLeft.
					self keyDelete ] ]
		ifFalse: 
			[ self keyLeft.
			self keyDelete ].
	"If no characters are displayed but text is scrolled 
	then scroll back one character at put the cursor on its right"
	self isCursorOnFirstColumn 
		ifTrue: [ self characterOffset > 0 
			ifTrue: 
				[ self characterOffset: self characterOffset - 1.
				self widget updateContents.
				self requestCursorOffset: 0 @ 1 ] ]!

keyCtrlEnd

	self form requestCursorAt: self widget cursorAtEnd.
	self keyEnd.  "fix" !

keyCtrlHome

	self form requestCursorAt: self widget cursorAtHome!

keyCtrlV

	self widget paste: 'this should be clipboard contents'.!

keyDelete
	"remove underlying character in string and replace with contracted one"
	
	widget editable ifFalse: [ ^self ].
	self widget deleteCharacterAt: self widget form cursor!

keyDown
	self keyTab!

keyEnd
	"Place the cursor past or at the end of the current line"
	
	| stringEnd |
	stringEnd := self widget lineEndAt: self form cursor.
	self widget hasReachedMaxLength
		ifTrue: [ self requestCursorAt: stringEnd ]
		ifFalse:[self requestCursorAt: stringEnd+ (0@1)]!

keyHome
	"Place the cursor at the beginning of the current line"
	
	self form requestCursorAt: ( self widget lineBeginAt: self form cursor)!

keyLeft

	| lcPoint |
	lcPoint := self localCursor.
	lcPoint = ( 1 @ 1 ) 
		ifTrue: 
			[ self visibleOffset > 0 
				ifTrue: 
					[ self visibleOffset: self visibleOffset - 1.
					self widget updateContents ].
			^self ].
	self widget showCursorAt: lcPoint - ( 0 @ 1 )!

keyReturn
	self widget broadcast: TWidgetEvent string with: self widget string.
	super keyReturn!

keyRight
	self isCursorBeyondLine 
		ifTrue: [ ^self ].
	self isCursorOnEndOfLine 
		ifTrue: [ ^self ].
	self isCursorAtEnd 
		ifTrue: 
			[ self widget hasReachedMaxLength 
				ifTrue: [ ^self ].
			self visibleOffset: self visibleOffset + 1.
			self widget updateContents.
			^self ].
	self requestCursorOffset: 0 @ 1!

keyShiftLeft

	"self widget changeSelectWith: self cursor."
	self keyLeft!

keyShiftRight
	
	"self widget changeSelectWith: self cursor."
	self keyRight
!

keySimpleCharacter: aCharacter 

	"case 
				1:not end of columns & not end of line -> advance the cursor to the right 
				2:end of columns & not end of line -> scroll line to left
				3:end of line -> tab"

	| where endOfColumn  full|
	( self isCursorInserting not
		and: [ self isCursorBeyondLine not ] ) 
		ifTrue: 
			[ self widget replaceCharacter: aCharacter at: self widget form cursor.
			self widget hasReachedMaxLength 
				ifFalse: [ self requestCursorOffset: 0 @ 1 ].
			^self ]. 
	
	where := self widget form cursor.
	endOfColumn := where y = self widget maxColumn.
	full := self widget hasReachedMaxLength. 
	
	full ifTrue: [
		 self widget replaceCharacter: aCharacter at: where.
		 self isCursorBeforeEndOfLine
			ifTrue:[self requestCursorOffset: 0 @ 1].
		^self].
	
	self widget insertCharacter: aCharacter at: where.
	full := self widget hasReachedMaxLength. 
	endOfColumn
		ifTrue: 
			[ full
				ifTrue: 
					[ self keyTab ]
				ifFalse: 
					[ visibleOffset := visibleOffset + 1.
					self widget updateContents ] ]
		ifFalse: 
			[ full
				ifTrue: [ self keyTab ]
				ifFalse: [ self requestCursorOffset: 0 @ 1 ] ]!

keyUp
	self keyShiftTab!

lastCursor
	^lastCursor ifNil:[lastCursor := self widget bounds origin]!

lastCursor: anObject
	lastCursor := anObject!

localCursor
	^self widget lineCharacterPointAt: self widget form cursor!

losingFocus
	self form stopBlinker.
	super losingFocus.	!

requestCursorAt: rcPoint 
	"Remember lastCursor from Form"
	
	self form requestCursorAt: rcPoint.
	self lastCursor: self form cursor!

requestCursorOffset: offset
	"answer whether cursor changed"
	
	| tryCursor |
	tryCursor := self form cursor + offset.
	(tryCursor x between: self widget minRow and: self widget maxRow)
		ifFalse: [^false].
	(tryCursor y between: self widget minColumn and: self widget maxColumn)
		ifFalse: [^false].
	self requestCursorAt: tryCursor.
	^true!

visibleOffset
	^visibleOffset!

visibleOffset: anObject
	visibleOffset := anObject! !
	

!TerminalTextController publicMethods !

initialize
	super initialize.
	self when: #control_r send: #keyCtrlR to: self!

keyBackspace
	self isCursorOnFirstColumn 
		ifTrue: [ self isCursorOnFirstRow 
			ifTrue: [ ^self ] ].
	self keyLeft.
	self keyDelete!

keyCtrlR

	"Toggle show carriage returns"
	self widget grid charactersDo:[ :c | c showCR: c showCR not ]	!

keyDown
	"Place the cursor on the next line 
	at the same character index 
	or at its end if the next line is shorter"

	"compute lcPoint and dispatch to the view
	to allow scrolling if needed"

	| charIndex lineIndex nextLineSize offset lcPoint |
	charIndex := self widget localColumnAt: self form cursor.
	lineIndex := self widget lineIndexAt: self form cursor.
	self widget textlines size <= lineIndex ifTrue: [^self].
	nextLineSize := (self widget textlines at: lineIndex + 1) size.
	charIndex <= nextLineSize 
		ifTrue: [lcPoint := (lineIndex + 1) @ charIndex]
		ifFalse: [lcPoint := (lineIndex + 1) @ nextLineSize].
	self widget showCursorAt: lcPoint!

keyLeft
	
	| lcPoint |
	lcPoint := self localCursor.
	lcPoint = (1@1)
		ifTrue: [^self].
	self widget showCursorAt: lcPoint - (0@1).!

keyPageDown

	"quick and..."
	| lcPoint |
	lcPoint := (self widget textlines size max: 1) @ 1.
	self widget showCursorAt: lcPoint!

keyPageUp
	"compute lcPoint and dispatch to widget"
	
	| lcPoint |
	lcPoint := 1@1.
	self widget showCursorAt: lcPoint!

keyReturn

	self isInsertMode
		ifTrue: [ self widget insertCharacter: Character cr at: self cursor ]
		ifFalse: [ self widget replaceCharacter: Character cr at: self cursor ]!

keyRight
	| charIndex lineIndex lcPoint line |
	self widget textlines isEmpty ifTrue: [^self].
	charIndex := self widget localColumnAt: self form cursor.
	lineIndex := self widget lineIndexAt: self form cursor.
	lcPoint := lineIndex @ charIndex.
	line := self widget textlines at: lineIndex.
	"before or on line end"
	charIndex <= line size 
		ifTrue: [^self widget showCursorAt: lcPoint + (0 @ 1)]!

keySimpleCharacter: aCharacter 

	self isInsertMode
		ifTrue: 
			[ self widget insertCharacter: aCharacter at: self cursor.
			self widget hasReachedMaxLength 
				ifTrue: [ ^self keyTab ] ]
		ifFalse: 
			[ self widget replaceCharacter: aCharacter at: self cursor ]!

keyUp
	"Place the cursor on the previous line 
	at the same character index 
	or at its end if the previous line is shorter"

	"compute lcPoint and dispatch to the view
	to allow scrolling if needed"

	| charIndex lineIndex previousLineSize lcPoint |
	charIndex := self widget localColumnAt: self form cursor.
	lineIndex := self widget lineIndexAt: self form cursor.
	lineIndex = 1 ifTrue: [^self].
	lcPoint := lineIndex @ charIndex.
	previousLineSize := (self widget textlines at: lineIndex - 1) size.
	previousLineSize < charIndex 
		ifTrue: [lcPoint := (lcPoint x - 1) @ previousLineSize]
		ifFalse: [lcPoint := lcPoint - (1 @ 0)].
	self widget showCursorAt: lcPoint! !
	

!TerminalTableCellController publicMethods !

gettingFocus
	self tableWidget selectionIndex: 0.
	super gettingFocus.
!

isCursorOnFirstRow
	"Override"
	^self tableWidget form cursor x = self tableWidget minRow!

isCursorOnLastRow
	^self tableWidget form cursor x = self tableWidget maxRow!

keyDown
	"
	CELLSELECT: Try to select the cell on the next row for the same column
	ROWSELECT: Try to select the next row
	"
	self tableWidget isRowSelecting 
		ifTrue:[^self tableWidget controller keyDown].
	self isCursorOnLastRow
		ifFalse:[self tableWidget selectCellBelow: self widget]!

keyLeft
	self widget editable
		ifTrue: [ super keyLeft ]
		ifFalse: [ self tableWidget selectCellLeft: self widget ]!

keyRight
	self widget editable
		ifTrue: [ super keyRight ]
		ifFalse: [ self tableWidget selectCellRight: self widget ]!

keyUp
	"
	CELLSELECT: Try to select the cell on the next row for the same column
	ROWSELECT: Try to select the next row
	"
	self tableWidget isRowSelecting 
		ifTrue:[^self tableWidget controller keyUp].
	self isCursorOnFirstRow
		ifFalse:[self tableWidget selectCellAbove: self widget]!

requestCursorAt: aPoint
	"If the cursor will enter the cell then make sure the table has no selection"
	self tableWidget selectionIndex: -1.
	super requestCursorAt: aPoint!

tableWidget
	^tableWidget!

tableWidget: anObject
	tableWidget := anObject! !
	

!TerminalDropDownListController publicMethods !

buttonReleaseAt: rcPoint
	self showOrHideList!

changedListSelection
	"Clear changes"
	showsList ifTrue:[	self currentList clearDirty]!

currentList
	"Pre: showsList is true"

	^self widget parent widgetNamed: self widget name , '_list'!

droppedListDefaultActionRequested
	| selection |
	selection := self currentList selectedItem.
	self hideList.
	self losingFocus.
	"Copy the selection"
	self widget selectedItem: selection!

droppedListLostFocus
	self hideList.
	self losingFocus.!

gettingFocus
	self widget selectionIndex > -1
		ifTrue:[self widget setHighlightLineAt: self widget selectionIndex to: true].
	super gettingFocus.!

hideList
	| list |
	self showsList ifFalse: [^self].
	list := self currentList.
	list removeActionsForEvent: TWidgetEvent losingFocus.
	list visible: false.
	self widget parent remove: list.
	self showsList: false!

initialize
	self showsList: false!

keyAlt: anEvent 
	( anEvent isAltPressed
		and: [ anEvent isDown ] ) 
		ifTrue: [ self showList ]!

keyCancel
	self showsList
		ifTrue: [ self hideList ]
		ifFalse: [ super keyCancel ]!

keyDown
	self showsList
		ifTrue: 
			[ self list controller keyDown.
			self widget updateContents ]
		ifFalse: 
			[ | index |
			index := self widget selectionIndex.
			self widget selectionIndex: ((index + 1) min: self widget items size - 1) ]!

keyPageDown
	self showsList
		ifTrue: 
			[ self list controller keyPageDown.
			self widget updateContents ]
		ifFalse: 
			[ | index |
			index := self widget selectionIndex.
			self widget selectionIndex: self widget items size - 1 ]!

keyPageUp
	self showsList
		ifTrue: 
			[ self list controller keyPageUp.
			self widget updateContents ]
		ifFalse: 
			[  | index | 
			index := self widget selectionIndex.
			index = -1 "no items" ifTrue: [ ^self ].
			self widget selectionIndex: 0 ]!

keyReturn
	self showsList 
		ifTrue: 
			[  | item | 
			item := self list selectedItem.
			self hideList.
			self widget selectedItem: item ]!

keyTab
	self showsList 
		ifTrue: [ self hideList ].
	super keyTab!

keyUp
	self showsList
		ifTrue: 
			[ self list controller keyUp.
			self widget updateContents ]
		ifFalse: 
			[  | index | 
			index := self widget selectionIndex.
			index = -1 "no items" ifTrue: [ ^self ].
			self widget selectionIndex: ( ( index - 1 ) max: 0) ]!

list

	"Pre: list is shown"
	^self widget parent widgetNamed: self widget name , '_list'!

losingFocus

	"Only highlight the selection if we have focus"

	"self showsList ifTrue: [ self hideList ]."
	self widget selectionIndex = -1 
		ifFalse: [ self widget setHighlightLineAt: self widget selectionIndex to: false ].
	super losingFocus!

showList
	| list |
	(self showsList or: [self widget items isEmpty]) ifTrue: [^self].
	list := TerminalWidget 
				listIn: ((self widget minRow + 1) @ self widget minColumn 
						corner: (self widget minRow 
								+ (self widget items size min: self widget maximumVisibleRows)) 
									@ self widget maxColumn).
	list controller: (TerminalChoiceListController new choiceController: self).
	list appearance foreground: Color black.
	list appearance background: Color white.
	list appearance borderColor: Color black.
	list name: self widget name , '_list'.
	list items: self widget items.
	"set selection before install event handler"
	list selectedItem: self widget selectedItem.
	list 
		when: TWidgetEvent selectedItem
		send: #changedListSelection
		to: self.
	list 
		when: TWidgetEvent enter
		send: #droppedListDefaultActionRequested
		to: self.
"	list 
		when: TWidgetEvent losingFocus
		send: #hideList
		to: self.
"
	"make sure this temporary list does not send state-change messages"
	list clearDirty.
	self widget parent add: list.
	list show.
	list haveFocus.
	self showsList: true!

showOrHideList
	self showsList
		ifTrue: 
			[ self list controller keyDown.
			self widget selectedItem: self list selectedItem.
			self widget updateContents ]
		ifFalse: 
			[ self widget initializeRowVisibilty.
			self showList ]!

showsList
	^showsList!

showsList: anObject
	showsList := anObject! !
	

!TerminalCheckBoxController publicMethods !

buttonReleaseAt: rcPoint

	self widget toggleSelection.
	!

keyDown
	^self keyTab!

keyLeft
	^self keyShiftTab!

keyRight
	^self keyTab!

keySimpleCharacter: aCharacter

	aCharacter ~= $ ifTrue: [ ^self windowController keySimpleCharacter: aCharacter].
	self widget toggleSelection!

keyUp
	^self keyShiftTab! !
	

!TerminalButtonController publicMethods !

gettingFocus
	self widget form status: self widget help asString.
	super gettingFocus.!

keyDown
	self keyTab!

keyLeft
	self keyShiftTab!

keyReturn
	self widget sendClickedMessage
!

keyRight
	self keyTab!

keyUp
	self keyShiftTab!

losingFocus
	super losingFocus.
	self widget form status: ''! !
	

!TWidgetAppearanceHolder publicMethods !

aboutToRemoveFromTerminalForm: aForm
	self widgetsDo: [ :each | each ~~ self ifTrue: [each aboutToRemoveFromTerminalForm: aForm ]]!

addFormDataTo: aMap
	"Answer whether data was inserted"
	^false!

appearance
	"If my appearance is not set then get a copy preferrable from my parent"

	appearance isNil ifTrue: [self appearance: TWidgetAppearance new].
	^appearance!

appearance: newAppearance

	(newAppearance notNil and:[appearance notNil])
		ifTrue: [ newAppearance parentAppearance: self parentAppearance ].
	self setAppearance: newAppearance.
	newAppearance isNil ifTrue: [ ^self ].
	self visible ifTrue: [ self update ].!

appearanceOrNil
	^appearance!

background: aColor
	self checkAppearance.
	appearance background: aColor.
	self markDirty: TMAppearanceSet.
	self visible = true ifTrue: [self update].!

border: aColor
	"DEPRECATED"
	self borderColor: aColor!

borderColor: aColor
	self checkAppearance.
	appearance borderColor: aColor.
	self markDirty: TMAppearanceSet.	
	self visible = true ifTrue: [self update].!

changedFont
	"Redefine this method to handle changes in fonts"!

checkAppearance
	appearance isNil 
		ifTrue: 
			[appearance := TWidgetAppearance new.
			self parent isNil 
				ifFalse: [appearance parentAppearance: self parent visibleAppearance]]!

defaultAppearance
	^TWidgetAppearance default!

desiresFocus
	^false!

foreground: intOrColor
	self checkAppearance.
	appearance foreground: intOrColor.
	self markDirty: TMAppearanceSet.	
	self visible = true ifTrue: [self update].!

justAddedToTerminalForm: aForm
	self widgetsDo: [ :each | each ~~ self ifTrue: [each justAddedToTerminalForm: aForm ]]!

parentAppearance
	"Answer the window appearance for the receiver"

	^self parent isNil 
		ifTrue: [TWidgetAppearance default]
		ifFalse: [self parent visibleAppearance]!

selectionBackground: intOrColor
	self checkAppearance.
	appearance selectionBackground: intOrColor.
	self markDirty: TMAppearanceSet.	
	self visible = true ifTrue: [self update].!

selectionForeground: intOrColor
	self checkAppearance.
	appearance selectionForeground: intOrColor.
	self markDirty: TMAppearanceSet.	
	self visible = true ifTrue: [self update].!

setAppearance: newAppearance
	appearance := newAppearance.
	self markDirty: TMAppearanceSet.!

visibleAppearance
	"Answer the widget appearance for displaying the receiver"

	| here |
	here := self.
	[here isNil not and: [here appearanceOrNil isNil and: [self parent notNil]]] 
		whileTrue: [here := here parent].
	^(here isNil or: [here appearanceOrNil isNil]) 
		ifTrue: [TWidgetAppearance default]
		ifFalse: [here appearance]! !
	

!TerminalWidget class publicMethods !

buttonClass
	^TerminalButton!

buttonIn: aRectangle
	^self buttonClass in: aRectangle!

dropDownListIn: aRectangle
	^TerminalDropDownList in: aRectangle!

image: anUrl in: aRectangle 
	
	| label |	
	label := TerminalImageLabel new.
	label url: anUrl.
	label bounds: aRectangle.
	^label!

in: aRectangle
	aRectangle extent x >= 0 ifFalse: [^self error: 'invalid rectangle'].
	aRectangle extent y >= 0 ifFalse: [^self error: 'invalid rectangle'].	
	^(super new initialize)
		bounds: aRectangle
		;clearDirty!

inputClass
	^TerminalInput!

inputIn: aRectangle
	^self inputClass in: aRectangle!

labelClass
	^TerminalLabel!

labelIn: aRectangle
	^self labelClass in: aRectangle!

listClass
	^TerminalList!

listIn: aRectangle
	^self listClass in: aRectangle!

menuBarClass
	^TerminalMenuBar!

menuClass
	^TerminalMenuList!

messageSendClass
	^MessageSend!

new

	"Answer a new instance of the receiver and initialize it"

	^self in: (1@1 extent: 0@0)!

panelIn: aRectangle
	^self windowClass in: aRectangle!

screenIn: aRectangle
	^self windowClass in: aRectangle!

tableListIn: aRectangle
	^TerminalTableList in: aRectangle!

tagID
	self subclassResponsibility!

textClass
	^TerminalText!

textIn: aRectangle
	^self textClass in: aRectangle!

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalWidget">
	<attribute name="appearance"/>
	<attribute name="bounds"/>
	<attribute name="name"/>
	<attribute name="parent"/>
	<attribute name="visible"/>
	<operation name="aboutToRemoveFromTerminalForm:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="addElementNamed:with:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="addStateMessagesTo:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="addTagAttributesTo:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="addToComposite:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="appearanceOrNil" return="Object"/>
	<operation name="asByteArray" return="Object"/>
	<operation name="backgroundColor:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="broadcast:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="broadcast:with:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="characterAppearance" return="Object"/>
	<operation name="clearDisplay" return="Object"/>
	<operation name="columns" return="Object"/>
	<operation name="corner" return="Object"/>
	<operation name="cursor" return="Object"/>
	<operation name="damageIn:outside:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="defaultAppearance" return="Object"/>
	<operation name="desiresFocus" return="Object"/>
	<operation name="displayExtent" return="Object"/>
	<operation name="foregroundColor:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="form" return="Object"/>
	<operation name="gettingFocus" return="Object"/>
	<operation name="grid" return="Object"/>
	<operation name="hide" return="Object"/>
	<operation name="initialCursor" return="Object"/>
	<operation name="invalidateRectangle:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="isDescendentOf:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="justAddedToTerminalForm:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="lineAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="losingFocus" return="Object"/>
	<operation name="maxColumn" return="Object"/>
	<operation name="maxRow" return="Object"/>
	<operation name="messageHandler" return="Object"/>
	<operation name="minColumn" return="Object"/>
	<operation name="minRow" return="Object"/>
	<operation name="origin" return="Object"/>
	<operation name="receiverID" return="Object"/>
	<operation name="rows" return="Object"/>
	<operation name="setAppearanceFromTaggedData:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="setInputFocus" return="Object"/>
	<operation name="show" return="Object"/>
	<operation name="showGraphicBorder:in:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="showSeparatingGraphicBorder:in:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="size" return="Object"/>
	<operation name="tabRequestedFrom:forward:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="topWidget" return="Object"/>
	<operation name="translateBy:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="update" return="Object"/>
	<operation name="updateAppearance" return="Object"/>
	<operation name="updateAppearanceIn:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="updateContents" return="Object"/>
	<operation name="updateContentsIn:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="wantsBorderOnFocus" return="Object"/>
	<operation name="wantsFocusOnTab" return="Object"/>
	<operation name="widgetNamed:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="widgetsDo:" return="Object">
		<parameter type="Object"/>
	</operation>
</class>'!

windowClass
	^TerminalWindow!

windowIn: aRectangle
	^self windowClass in: aRectangle! !

!TerminalWidget publicMethods !

bounds

	"Answer the value of bounds"

	^bounds!

bounds: aValue

	"Set the value of bounds"

	bounds := aValue.
	self markDirty: TMBoundsSet.!

boundsForFocus
	^self bounds!

centerOnParent
	"Center the receiver with respect to my container.
	Pre: parent notNil"
	
	| offset |
	offset := ((parent rows - self rows)
		@(parent columns - self columns)) // 2.
	self translateBy: offset.!

clearDisplay: shouldRefresh 

	| r c maxR maxC parentAppearance |
	self visible 
		ifFalse: [ ^self ].
	r := self bounds origin x.
	c := self bounds origin y.
	maxR := self bounds corner x.
	maxC := self bounds corner y.
	parentAppearance := self parentAppearance.
	r 
		to: maxR 
		do: [ :row | 
			c 
				to: maxC 
				do: [ :column | ( self grid row: row column: column ) 
					clearWithAppearance: parentAppearance 
					refresh: shouldRefresh ] ]!

columns
	^self bounds height + 1!

corner
	^self bounds corner!

cursor
	^self form cursor!

cursorAtHome
	^self origin!

damageIn: outerArea outside: innerArea 

	"Answer a collection of rectangles comprising the parts of the outerArea which do not lie
	 within innerArea."

	| answer left top right bottom |
	outerArea corner < innerArea origin 
		ifTrue: [ ^Array with: outerArea ].
	outerArea origin > innerArea corner 
		ifTrue: [ ^Array with: outerArea ]. 
	
	answer := OrderedCollection new. 
	
	outerArea left < innerArea left
		ifTrue: 
			[ left := innerArea left - 1.
			answer add: ( outerArea origin corner: left @ outerArea bottom ).
			left := left + 1 ]
		ifFalse: 
			[ left := outerArea left + 1 ]. 
	
	outerArea top < innerArea top
		ifTrue: 
			[ top := innerArea top - 1.
			answer add: ( left @ outerArea top corner: outerArea right @ top ).
			top := top + 1 ]
		ifFalse: 
			[ top := outerArea top ]. 
	
	outerArea right > innerArea right
		ifTrue: 
			[ right := innerArea right + 1.
			answer add: ( right @ top corner: outerArea corner ).
			right := right - 1 ]
		ifFalse: 
			[ right := outerArea right ]. 
	
	outerArea bottom > innerArea bottom 
		ifTrue: 
			[ bottom := innerArea bottom + 1.
			answer add: ( left @ bottom corner: right @ outerArea bottom ) ]. 
	
	^answer!

defaultControllerClass
	^TerminalWindowController!

desiresFocus
	^false!

displayExtent

	"cannot compute if not shown by a terminal form"
	| w h |
	self form isNil 
		ifTrue: [ ^0 @ 0 ].
	w := ( self bounds height + 1 ) * self appearance characterWidth.
	h := ( self bounds width + 1 ) * self appearance characterHeight.
	^w @ h!

form
	self parent isNil ifTrue: [ ^nil ].
	^self parent form!

grid
	^self form grid!

hasFocus
	| form |
	form := self form .
	form isNil ifTrue: [ ^false ].
	^form focusWidget = self!

haveFocus
	"Make the receiver the new focus widget"
	self form setFocusWidget: self
	
	!

haveFocusAt: rcPoint
	self haveFocus!

hide
	"Hide the receiver and refresh the damaged region"
	
	self hide: true!

hide: shouldRefresh
	| reversed |
	reversed := OrderedCollection new.
	self widgetsDo:[ :each | reversed addFirst: each ].
	reversed do:[ :each |
		each clearDisplay: shouldRefresh.
		each visible: false ]!

hierarchy
	| out |
	out := WriteStream on: String new.
	self printOn: out indent: 0.
	^out contents.!

initialize
	"Subclasses that redefine this method should call the super first"
	super initialize.
	self setVisible: false!

inspectActions
	^#(rows columns form cursor asByteArray)!

installMessage: aMessageOrCollection onEvent: eventID 

	^aMessageOrCollection installForWidget: self onEvent: eventID!

installMessageSend: aMessageSend onEvent: eventID 
	"Receiver = 
	1: _client
	2: _server
	3: other"

	| event |
	event := TWidgetEvent fromID: eventID.
	aMessageSend receiver = '_client' 
		ifTrue: [^self installMessageToClient: aMessageSend event: event].
	aMessageSend receiver = '_server' 
		ifTrue: [^self installMessageToServer: aMessageSend event: event].
	^self installMessageToWidget: aMessageSend event: event!

installMessageToClient: aMessageSend event: event 

	| action |
	event isTriggeredWithArgument 
		ifTrue: 
			[action := MessageSend 
						receiver: self messageHandler
						selector: #with:dispatchIndexedMessage:
						arguments: (Array with: nil with: aMessageSend)]
		ifFalse: 
			[action := MessageSend 
						receiver: self messageHandler
						selector: #dispatchIndexedMessage:
						arguments: (Array with: aMessageSend)].
	^self when: event evaluate: action!

installMessageToServer: aMessageSend event: event 

	| action |
	event isTriggeredWithArgument 
		ifTrue: 
			[action := MessageSend 
						receiver: self messageHandler
						selector: #with:request:
						arguments: (Array with: nil with: aMessageSend selector)]
		ifFalse: 
			[aMessageSend arguments isEmpty 
				ifTrue: 
					[action := MessageSend 
								receiver: self messageHandler
								selector: #request:
								arguments: (Array with: aMessageSend selector)]
				ifFalse: 
					[	aMessageSend isIndexed 
						ifTrue: [
							action := MessageSend 
									receiver: self messageHandler
									selector: #indexedRequest:withArguments:
									arguments: (Array with: aMessageSend index with: aMessageSend arguments)]
						ifFalse: [
							action := MessageSend 
									receiver: self messageHandler
									selector: #request:withArguments:
									arguments: (Array with: aMessageSend selector with: aMessageSend arguments)]							
						]].
	^self when: event evaluate: action!

installMessageToWidget: aMessageSend event: event 

	| action |
	event isTriggeredWithArgument 
		ifTrue: 
			[action := MessageSend 
						receiver: (self messageHandler receiverAt: aMessageSend receiver)
						selector: #with:dispatchIndexedMessage:
						arguments: (Array with: nil with: aMessageSend)]
		ifFalse: 
			[action := MessageSend 
						receiver: (self messageHandler receiverAt: aMessageSend receiver)
						selector: #dispatchIndexedMessage:
						arguments: (Array with: aMessageSend)].
	self when: event evaluate: action!

invalidateRectangle: damage

	| region |
	(damage areaIntersects: self bounds)
		ifFalse:[^self].
	region := damage intersectOrLine: self bounds.
	self updateAppearanceIn: region.
	self updateContentsIn: region.!

isDescendentOf: parentWidget
	"Answer whether the receiver is a child or sub-child etc of the parentWidget"
	
	| here |
	here := self.
	[parentWidget = here
		ifTrue: [ ^true ].
	here = nil
		ifTrue: [ ^false ].
	here := here parent] repeat
	!

kindDo: aRequestor
	"Dispatch the request to @aRequestor and answer its return value"
	^aRequestor doTerminalWidget: self!

lineAt: row
	"Answer the string line for the receiver at @row"
	"self should: [bounds origin x <= row and:[ bounds corner x <= row]]."
	
	^self grid at: row @ self bounds origin y size: self columns !

localColumnAt: rcPoint 
	^rcPoint y - self minColumn + 1!

maxColumn
	^self bounds bottom!

maxRow
	^self bounds right!

messageHandler
	"Answer the object that can handle terminal message sends"
	
	^parent isNil
		ifTrue: [ self ]
		ifFalse:[ parent messageHandler]!

minColumn
	^self bounds origin y!

minRow
	^self bounds origin x!

origin
	^self bounds origin!

parent
	"Answer the value of parent"

	^parent!

parent: anObject
	parent := anObject!

printOn: aStream
	aStream nextPutAll: self name ;nextPut: $( ;print: self class ;nextPut: $)!

printOn: aStream indent: level
	self printOn: aStream.
	aStream 
		nextPut:$[
		;nextPutAll: 'left=' 
		;print: self minColumn
		;nextPutAll: ' right=' 
		;print: self maxColumn		
		;nextPutAll: ' top=' 
		;print: self minRow
		;nextPutAll: ' bottom=' 
		;print: self maxRow		
		;nextPut: $].
	appearance isNil
		ifTrue: [ ^self ].
	aStream cr.
	level timesRepeat:[aStream tab].
	aStream print: self appearance
	!

putLine: aString startingAt: startIndex from: firstColumn to: lastColumn on: row 

	| index |
	index := startIndex.
	self grid 
		charactersOn: row 
		from: firstColumn 
		to: lastColumn 
		do: [ :tchar | 
			tchar character: ( aString at: index ).
			tchar localUpdate.
			index := index + 1 ].
	self grid 
		charactersOn: row 
		from: lastColumn + 1 
		to: self corner y 
		do: [ :tchar | 
			tchar character: $ .
			tchar localUpdate ]!

receiverID
	^self name!

rows
	^self bounds width + 1!

setInputFocus
	
	self widgetsDo: [ :each | each ~~ self & each desiresFocus
		ifTrue: [^each haveFocus]]
	!

setVisible: aBoolean
	visible := aBoolean!

show
	self visible: true.
	self update.!

showGraphicBorder: boolean focus: hasFocus in: rcBounds 

	"Set decoration and localupdate"

	| r c maxR maxC |
	r := rcBounds origin x.
	c := rcBounds origin y.
	maxR := rcBounds corner x.
	maxC := rcBounds corner y.
	r 
		to: maxR 
		do: [ :row | 
			( self grid row: row column: maxC )  showBorderRight: boolean ; showFocus: hasFocus.
			( self grid row: row column: c )  showBorderLeft: boolean ; showFocus: hasFocus ].
	c 
		to: maxC 
		do: [ :column | 
			( self grid row: r column: column ) showBorderTop: boolean  ; showFocus: hasFocus.
			( self grid row: maxR column: column )  showBorderBottom: boolean ; showFocus: hasFocus ].
	r 
		to: maxR 
		do: [ :row | 
			( self grid row: row column: maxC ) localUpdate.
			( self grid row: row column: c ) localUpdate ].
	c 
		to: maxC 
		do: [ :column | 
			( self grid row: r column: column ) localUpdate.
			( self grid row: maxR column: column ) localUpdate ]!

showSeparatingGraphicBorder: boolean in: rcBounds 

	| r c maxR maxC |
	r := rcBounds origin x.
	c := rcBounds origin y.
	maxR := rcBounds corner x.
	maxC := rcBounds corner y.
	r + 1 
		to: maxR - 1 
		do: [ :row | 
			( self grid row: row column: maxC )  showSeparatorVertical: boolean ; localUpdate.
			( self grid row: row column: c )  showSeparatorVertical: boolean ; localUpdate ].
	c + 1 
		to: maxC - 1 
		do: [ :column | 
			( self grid row: r column: column )  showSeparatorHorizontal: boolean ; localUpdate.
			( self grid row: maxR column: column )  showSeparatorHorizontal: boolean ; localUpdate ].
	( self grid row: r column: c )  showSeparatorTopLeft: boolean ; localUpdate.
	( self grid row: r column: maxC )  showSeparatorTopRight: boolean ; localUpdate.
	( self grid row: maxR column: c )  showSeparatorBottomLeft: boolean ; localUpdate.
	( self grid row: maxR column: maxC )  showSeparatorBottomRight: boolean ; localUpdate!

size
	^self bounds extent + (1@1)!

tabRequestedFrom: aWidget  forward: forward

	^(self visible and:[ self wantsFocusOnTab ])
			ifTrue: [ self form setFocusWidget: self ]
			;yourself
	!

topWidget
	self parent notNil ifTrue: [^self parent topWidget]!

translateBy: aPoint
	"Pre: not realized"
	self bounds: (self bounds translateBy: aPoint).!

update
	self updateAppearanceIn: self bounds.
	self updateContentsIn: self bounds!

updateAppearanceIn: region 

	"Pre: region is within the receiver's bounds.
	Each terminal character in my bounds will have an empty character and its appearance cleared"

	| leftTop rightBottom drawable gc displayAppearance  overlap|
	(region areaIntersects: self bounds) 
		ifFalse:[^self].
	overlap := self bounds intersect: region.
	displayAppearance := self visible
		ifTrue: [ self visibleAppearance ]
		ifFalse: [ self parentAppearance ].
	self grid 
		charactersInBounds: overlap 
		do: [ :tc | tc clearWithAppearance: displayAppearance refresh: false ].
	leftTop := self grid row: overlap left column: overlap top.
	rightBottom := self grid row: overlap right column: overlap bottom.
	drawable := leftTop parent window.
	gc := leftTop parent overDrawGC.
	gc setForeground: displayAppearance background.
	drawable 
		fillRectangle: gc 
		x: leftTop box left 
		y: leftTop box top 
		width: rightBottom box right - leftTop box left + 1 
		height: rightBottom box bottom - leftTop box top + 1.
	self updateBorderAppearance!

updateBorderAppearance

	self visibleAppearance borderColor notNil
		ifTrue: [ self showGraphicBorder: true focus: false in: self bounds]		!

updateContents
	self updateContentsIn: self bounds!

updateContentsIn: region
	"Pre: region is within the receiver's bounds"!

updateFocus: hasFocus 

	hasFocus
		ifTrue: 
			[ self showGraphicBorder: true focus: true in: self boundsForFocus ]
		ifFalse: 
			[ self visibleAppearance borderColor isNil
				ifTrue: [ self showGraphicBorder: false focus: false in: self bounds ]
				ifFalse: [ self showGraphicBorder: true focus: false in: self bounds ] ]!

visible
	^visible!

visible: aBoolean 

	aBoolean = visible 
		ifTrue: [ ^self ].
	visible := aBoolean.
	self markDirty: TMVisibleSet!

wantsFocusOnTab
	^true!

when: eventName send: aMessageSend
	"Shortcut for adding event to messagesend connections"
	self 
		when: eventName 
		evaluate: aMessageSend!

widgetUnderPoint: rcPoint

	self visible ifFalse:[^nil].
	^(self bounds areaIncludesPoint: rcPoint)
		ifTrue: [ self ]
		ifFalse:[ nil ]! !
	

!TerminalCompositeWidget class publicMethods !

tagID
	^7! !

!TerminalCompositeWidget publicMethods !

add: aWidget
	self widgets add: aWidget.
	aWidget parent: self.
	aWidget appearanceOrNil notNil
		ifTrue: [ aWidget appearance parentAppearance: self visibleAppearance ].
	^aWidget!

addEventConnectionsTo: aCollection 
	self widgets do: [ :each | each addEventConnectionsTo: aCollection ]!

addFirst: aWidget
	self widgets addFirst: aWidget.
	aWidget parent: self.
	aWidget appearanceOrNil notNil
		ifTrue: [ aWidget appearance parentAppearance: self visibleAppearance ].
	^aWidget!

defaultActionButton

	"Answer the button for receiving the default action request.
	Answer nil if no such button was found"

	self widgets do: [ :each | ( each class = self class buttonClass
		and: [ each isDefault ] ) 
		ifTrue: [ ^each ] ].
	^nil!

form
	form isNil
		ifTrue: [ parent isNil
			ifFalse:[ form := parent form]].
	^form!

form: anObject
	form := anObject!

inputNamesAndValues

	"Answer a dictionary of input values associated to widget names"

	| map |
	map := Dictionary new.
	self widgetsDo: [ :each | each addFormDataTo: map ].
	^map!

invalidateRectangle: damage 

	self updateAppearanceIn: (damage intersectOrLine: self bounds).
	self widgets do: [ :eachWidget | eachWidget invalidateRectangle: damage].
!

kindDo: aRequestor
	"Dispatch the request to @aRequestor and answer its return value"
	^aRequestor doTerminalCompositeWidget: self!

messageHandler

	"Answer the object that can handle terminal message sends"

	^form isNil
		ifTrue: [super messageHandler ]
		ifFalse: [ form messageHandler ]!

parent: aWidget 
	super parent: aWidget.
	aWidget notNil
		ifTrue: 
			[ form := aWidget form.
			self visible: aWidget visible ]
		ifFalse: 
			[ form := nil ]!

printOn: aStream indent: level
	super printOn: aStream indent: level.
	self widgets do:[ :each |
		aStream cr.
		level + 1 timesRepeat:[aStream tab].
		each printOn: aStream indent: level + 1]!

receiverAt: key
	"Context-API"
	^self widgetNamed: key!

release
	self widgets do:[:each | each release ].
	super release.!

remove: aWidget

	| descendentHadFocus |
	self visible == true
		ifTrue:
		[(descendentHadFocus := ( self form focusWidget notNil and:[ self form focusWidget isDescendentOf: aWidget ] ))
			ifTrue:[self form setFocusWidget: nil]].
	"aWidget clearDisplay: true."
	widgets remove: aWidget.
	aWidget parent: nil.
	self topWidget invalidateRectangle: aWidget bounds.
	self visible == true
		ifTrue:[ descendentHadFocus
			ifTrue: [self topWidget setInputFocus]]!

show
	super show.
	self widgets do:[ :each | each show ]!

submitInputNamesAndValuesTo: operationName

	^self messageHandler
		request: operationName
		withArguments: (Array with: self inputNamesAndValues)!

tabRequestedFrom: aWidget forward: isForward

	| index nextWidget interval |
	index := self widgets indexOf: aWidget.
	interval := isForward
		ifTrue: [ index + 1 to: self widgets size ]
		ifFalse: [ index - 1 to: 1 by:  -1].
	interval do: [ :i | 
		nextWidget := self widgets at: i.
		(nextWidget tabRequestedFrom: aWidget forward: isForward)
			ifTrue: [^true ] ].
	interval := isForward
		ifTrue: [ 1 to: index - 1 ]
		ifFalse: [ self widgets size to: index + 1 by: -1].
	interval do: [ :i | 
		nextWidget := self widgets at: i.
		(nextWidget tabRequestedFrom: aWidget forward: isForward)
			ifTrue: [^true ] ].	
	^false		
	!

translateBy: aPoint
	super translateBy: aPoint.
	self widgets do:[ :each | each translateBy: aPoint ]!

widgetNamed: aString 
	| found |
	found := super widgetNamed: aString.
	found notNil 
		ifTrue: [ ^found ].
	self widgets do: [ :each | 
		found := each widgetNamed: aString.
		found notNil 
			ifTrue: [ ^found ] ].
	^nil
!

widgets

	"Answer the value of widgets"

	^widgets ifNil:[widgets := OrderedCollection new]!

widgetsDo: oneArgBlock

	super widgetsDo: oneArgBlock.
	self widgets do: [ :each | each widgetsDo: oneArgBlock]!

widgetUnderPoint: rcPoint 

	self widgets reverseDo: [ :each | ( each widgetUnderPoint: rcPoint ) ifNotNil: [ :w | ^w ] ].
	^nil! !
	

!TerminalWindow class publicMethods !

tagID
	^19!

tagName
	^'window'! !

!TerminalWindow publicMethods !

close
	"Close the terminal showing the receiver"
	self form close!

controller
	^controller!

controller: anObject
	controller := anObject!

enabled
	^true!

findWidgetNamed: aString
	| found |
	found := super widgetNamed: aString.
	found isNil ifFalse: [ ^found ].
	^nil!

hasMenuBar
	^menuBar notNil!

initialize
	super initialize.
	self controller: (self defaultControllerClass new widget: self)!

kindDo: aRequestor
	^aRequestor doTerminalWindow: self!

menuBar
	"Lazy initialize the menuBar if asked for. Use hasMenuBar to detect whether a menuBar exists"
	
	menuBar isNil 
		ifTrue: 
			[ menuBar := TerminalMenuBar in: ( self minRow @ self minColumn corner: self minRow @ self maxColumn ) ].
	^menuBar!

menuBar: anObject
	menuBar := anObject!

status: aString
	self form isNil ifTrue: [ ^self ].
	self form status: aString!

stylesheet
	^stylesheet!

stylesheet: anObject
	stylesheet := anObject!

title
	^title!

title: anObject
	title := anObject.
	self form ifNotNil: [ :nonNilForm | nonNilForm title: self title ].
	self markDirty: TMTitleSet!

widgetNamed: aString
	| found |
	found := super widgetNamed: aString.
	found isNil ifFalse: [ ^found ].
	self error: '[Widget] child widget not found:' , aString! !
	

!TerminalGroupBox class publicMethods !

tagID
	^20! !

!TerminalGroupBox publicMethods !

alignment
	" Stub "
	^0!

alignment: anAlignment
	" Stub "
	self markDirty: TMAlignmentSet!

kindDo: aRequestor
	^aRequestor doTerminalGroupBox: self!

label
	^label!

label: anObject
	label := anObject.
	self markDirty: TMStringSet!

updateContentsIn: region 

	super updateContentsIn: region.
	self 
		putLine: self label 
		startingAt: 1 
		from: self minColumn + 1 
		to: self minColumn + 1 + self label size - 1 
		on: self minRow.
	self showSeparatingGraphicBorder: true in: self bounds. 	"remove those lines under label"
	self minColumn + 1 
		to: self minColumn + 1 + self label size - 1 
		do: [ :column | ( self grid row: self minRow column: column )  showSeparatorHorizontal: false ; localUpdate ]! !
	

!TerminalBasicWidget class publicMethods !

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalBasicWidget" superclass="TerminalWidget">
	<attribute name="controller" type="TerminalBasicController"/>
	<operation name="defaultControllerClass" return="Class"/>
	<operation name="newController" return="TerminalBasicController"/>
</class>'! !

!TerminalBasicWidget publicMethods !

controller
	^controller!

controller: anObject

	controller = anObject
		ifTrue: [^self].
	controller := anObject.
	anObject widget: self
!

defaultControllerClass
	^TerminalBasicController!

editable: boolean
	"I don't care"!

enabled
	^enabled!

enabled: anObject
	enabled := anObject.
	self markDirty: TMEnabledSet!

help
	^''
	!

initialize
	super initialize.
	self enabled: true.
	self controller: self newController.!

inspectActions
	^super inspectActions , #( asXML )!

isEnabled
	^self enabled!

newController
	^self defaultControllerClass new widget: self! !
	

!TerminalList class publicMethods !

tagID
	^3! !

!TerminalList publicMethods !

addFormDataTo: aMap
	"Answer whether data was inserted"

	self selectionIndex = -1 ifTrue: [ ^false ].	
	aMap at: self name put: self selectedItem.
	^true!

bounds: aRectangle
	super bounds: aRectangle.
	self reset.!

broadcastSelectedItem
	self broadcast: TWidgetEvent selectedItem with: self selectedItem.
	(self visible == true and: [self wantsRangeStatus]) 
		ifTrue: [self updateRangeStatus]!

broadcastSelectionIsValid
	self broadcast: TWidgetEvent selectionIsValid
		with: self selectedItem notNil!

defaultControllerClass
	^TerminalListController!

desiresFocus
	^true!

hasItems
	^self items notEmpty!

haveFocusAt: rcPoint
	"Find selectionIndex for row of rcPoint"
	| index |
	super haveFocusAt: rcPoint.
	index := rcPoint x - self topRow.
	(index between: 0 and: self items size - 1)
		ifTrue:[self selectionIndex: index]!

initialize
	"Selection index is zero-based"

	super initialize.
	selectionIndex := -1.
	self items: #().!

items
	^items!

items: aCollection

	self primItems: aCollection.
	self markDirty: TMItemsSet!

kindDo: aRequestor
	"Dispatch the request to @aRequestor and answer its return value"
	^aRequestor doTerminalList: self!

maxUsableColumn
	^self bounds corner y!

nextSelectionIndexOrNil
	"Index zero-based"
	
	| newIndex |
	self items isEmpty
		ifTrue: [^nil].
	self selectionIndex = -1
		ifTrue: [^0].
	newIndex := self selectionIndex + 1.
	newIndex = self items size
		ifTrue: [self controller canRotateSelection
			ifTrue: [newIndex := 0]
			ifFalse: [newIndex := self items size - 1]].
	^newIndex!

previousSelectionIndexOrNil
	"Index zero-based"
	
	| newIndex |
	self items isEmpty
		ifTrue: [^nil].
	self selectionIndex = -1 ifTrue: [^0].
	newIndex := self selectionIndex - 1.
	newIndex = -1
		ifTrue: [self controller canRotateSelection
			ifTrue: [newIndex := self items size - 1]
			ifFalse: [newIndex := 0]].
	^newIndex!

primItems: aCollection

	items := aCollection.
	self reset.
	self visible ifTrue:[self updateContents]!

primSelectionIndex: int 
	"Index is zero-based"

	| lastIndex  localIndex|
	int = self selectionIndex 
		ifTrue: [ ^self ].
	lastIndex := self selectionIndex.
	selectionIndex := int.
	self markDirty: TMSelectionIndexSet.
	(self visible and:[lastIndex > -1])
		ifTrue: [ self setHighlightLineAt: lastIndex to: false ]. 	"update visuals"
	( visibleItemRange includes: selectionIndex) 
		ifFalse: 
			[ selectionIndex < lastIndex
				ifTrue: [ self scrollUpTo: selectionIndex ]
				ifFalse: [ self scrollDownTo: selectionIndex ] ].
	(self visible and:[int > -1])
		ifTrue: [ self setHighlightLineAt: int to: true ]!

printableItemAt: index 
	"Index is ZERO-based"

	| object |
	object := items at: index + 1.
	^object isString
		ifTrue: [ object ]
		ifFalse: [ object asString ]!

reset
	"Indices and range are zero-based"
	
	self primSelectionIndex: -1.
	self items isEmpty
		ifTrue:[visibleItemRange := #()]
		ifFalse:[visibleItemRange := 0 to: (self rowsForItems min: self items size) - 1].!

rowsForItems
	^self rows!

scrollDownTo: int 
	| rangeEnd |
	rangeEnd := int.
	visibleItemRange := ((rangeEnd - self rowsForItems + 1) max: 0) to: rangeEnd.
	self visible ifTrue:[self updateRowContents]!

scrollUpTo: int 
	| rangeEnd |
	rangeEnd := int + self rowsForItems - 1 min: items size - 1.
	visibleItemRange := (rangeEnd - self rowsForItems + 1 max: 0) to: rangeEnd.
	self updateRowContents!

selectedItem
	self selectionIndex = -1
		ifTrue: [^nil].
	^self items at: self stSelectionIndex!

selectedItem: anObject
	"-1 means no selection"

	self selectionIndex: (items indexOf: anObject) - 1!

selectionBoundsAt: int

	| row |
	row := int - visibleItemRange first + self bounds origin x.
	^row @ self minColumn corner: row @ self maxColumn!

selectionIndex
	^selectionIndex!

selectionIndex: int 
	"Indices are ZERO-based"

	int = self selectionIndex 
		ifTrue: [ ^self ].
	self primSelectionIndex: int.
	self broadcastSelectedItem.
	"self broadcastSelectionIsValid."!

separatorString
	^'-------'!

setHighlightLineAt: int to: bool 

	"translate index into correct row in grid"

	self grid 
		charactersOn: (int - visibleItemRange first + self topRow)
		from: self minColumn
		to: self maxUsableColumn
		do: [ :char | char select: bool ;localUpdate ]!

stSelectionIndex
	^selectionIndex + 1!

topRow
	^self minRow!

updateContentsIn: region 

	"Pre: region intersects bounds of receiver"

	| startC  stopC startR stopR copyStart row copyEnd|
	startC := region top max: self minColumn.
	stopC := region bottom min: self maxColumn.
	startR := region left max: self minRow.
	stopR := region right min: self maxRow.
	copyStart := ( region top - self minColumn + 1 ) max: 1.
	row := self minRow.
	visibleItemRange do: [ :index | 
		 | item trimmedItem | 
		( row between: startR and: stopR) 
			ifTrue: 
				[ item := self printableItemAt: index. 
				copyEnd := ( copyStart + stopC - startC ) min: item size.
				trimmedItem := item copyFrom: copyStart to: copyEnd.
				self grid 	row: row from: startC to: stopC 	put: trimmedItem ].
		row := row + 1 ].
	[ row <= stopR ]
		whileTrue: 
			[ self grid row: row from: startC to: stopC put: ''.
			row := row + 1 ].
	self updateSelection!

updateRangeStatus

	| stat |
	self visibleItemRange isEmpty 
		ifTrue: [ ^self form status: '' ].  "Need to find out why: EM"
	self selectionIndex = -1
		ifTrue: [ ^self form status: '' ].
	stat := WriteStream on: String new.
	stat print: self selectionIndex + 1.
	stat nextPutAll: ' of '.
	stat print: self items size. 
	
	self visibleItemRange first > 0 
		ifTrue: 
			[ stat nextPutAll: ' - [ '.
			stat print: self visibleItemRange first + 1.
			stat nextPutAll: ' .. '.
			stat print: self visibleItemRange last + 1.
			stat nextPutAll: ' ]' ].
	self form status: stat contents!

updateRowContents
	self updateContents!

updateSelection

	self selectionIndex > -1
		ifTrue: [ self setHighlightLineAt: self selectionIndex to: true ]!

visibleItemRange
	"For controller, may want to refactor later"
	^visibleItemRange!

wantsRangeStatus
	^true! !
	

!TerminalTableList class publicMethods !

tagID
	^17! !

!TerminalTableList publicMethods !

addColumn: aTableColumn 

	"Add the column and update the inputWidgets"

	| columnWidgets columnIndex right top left |
	self tableColumns add: aTableColumn.
	columnWidgets := Array new: self rows - 1.
	columnIndex := self tableColumns size.
	right := self tableColumns 
		inject: self minColumn - 1
		into: [ :sum :each | sum + each width ].
	"check bounds"
	right > self maxColumn
		ifTrue: [ ^self error: 'column does not fit' ].
	top := self bounds left.
	left :=  right - aTableColumn width + 1.
	1 to: self rows do: [ :rowIndex | 
			 | input box header | 
			box := ( top + rowIndex - 1 ) @ left extent: 0 @ ( aTableColumn width - 1 ).
			rowIndex = 1
				ifTrue: 
					[ (aTableColumn header)
						name:self name , '_heading' , columnIndex asString
						;bounds: box
						;parent: self.
					headingWidgets add: aTableColumn header.
					aTableColumn hasAppearance
						ifTrue:[aTableColumn appearance borderColor: self appearance borderColor]
						ifFalse:[aTableColumn appearance: self appearance ]]
				ifFalse: 
					[ aTableColumn editable
						ifTrue:
							[input := self class inputIn: box.
							input controller: self newCellController.
							input maxLength: (aTableColumn maxLength ifNil:[-1]).
							"make sure policy is correct"
							self beCellSelecting]
						ifFalse:
							[input := self class labelIn: box].
					input name: self name , '_' , (rowIndex - 1) asString , '_' , columnIndex asString.			
					input parent: self.
					aTableColumn hasAppearance
						ifTrue:[input appearance: aTableColumn appearance].
					columnWidgets at: rowIndex - 1 put: input ] ].
	inputWidgets add: columnWidgets!

allInputWidgetsDo: oneArgBlock
	self inputWidgets do:[ :column |
		column do: oneArgBlock ]!

beCellSelecting
	selectionPolicy := 2!

columnForCell: cellWidget
	^self inputWidgets detect:[ :each | each includes: cellWidget ]!

editable
	^false!

headingWidgets
	^headingWidgets!

headingWidgets: anObject
	headingWidgets := anObject!

inBackwardReadOrderDo: oneArgBlock
	"traverse from right to left, bottom to top"
	self tableRowCount - 1 to: 1 by: -1 do:[ :rowIndex |
		self tableColumnCount to: 1 by: -1 do:[ :columnIndex |
			| input |
			input := self inputAt: rowIndex @ columnIndex.
			oneArgBlock value: input ]]
	!

inForwardReadOrderDo: oneArgBlock
	"traverse from left to right, top to bottom"
	1 to: self tableRowCount - 1 do:[ :rowIndex |
		1 to: self tableColumnCount do:[ :columnIndex |
			| input |
			input := self inputAt: rowIndex @ columnIndex.
			oneArgBlock value: input ]]
	!

initialize
	super initialize.
	tableColumns := OrderedCollection new.
	inputWidgets := OrderedCollection new.
	headingWidgets := OrderedCollection new.
	selectionPolicy := 1 "RowSelect"!

inputAt: rcPoint
	"Answer the input widget @rcPoint.
	Header is row 0"
	^(inputWidgets at: rcPoint y) at: rcPoint x!

inputWidgets
	^inputWidgets!

isCellSelecting
	^selectionPolicy = 2!

isRowSelecting
	^selectionPolicy = 1!

kindDo: aRequestor
	^aRequestor doTerminalTableList: self!

lastEditableInputWidget
	"Pre: receiver has input widgets"

	self items isEmpty
		ifTrue: [ ^nil ].
	^self inputWidgets last at: (self rowsForItems min: self items size)!

lastUsedColumn
	| column |
	column := self minColumn.
	self tableColumns do:[ :tc |
		column := column + tc width ].
	^column - 1!

maxUsableColumn
	^self minColumn + (self tableColumns inject:-1 into:[ :sum :each | sum + each width ])!

newCellController
	^TerminalTableCellController new tableWidget: self!

rowsForItems
	"with header"
	^super rowsForItems - 1!

selectCellAbove: cellWidget 

	| column row |
	column := self columnForCell: cellWidget.
	( row := column indexOf: cellWidget ) = 1 
		ifTrue: [ ^self ].
	( column at: row - 1 ) haveFocus!

selectCellBelow: cellWidget 
	"Pre: there exists a cell below"
	| column row |
	column := self columnForCell: cellWidget.
	( row := column indexOf: cellWidget ) = (self rowsForItems min: self items size)
		ifTrue: [ ^self ].
	row := row + 1.
	( column at: row ) haveFocus!

selectCellLeft: cellWidget 

	| column row index |
	column := self columnForCell: cellWidget.
	row := column indexOf: cellWidget.
	index := self inputWidgets indexOf: column.
	index = 1 
		ifTrue: [ ^self ].
	( ( self inputWidgets at: index - 1 ) at: row ) haveFocus!

selectCellRight: cellWidget 

	| column row index |
	column := self columnForCell: cellWidget.
	row := column indexOf: cellWidget.
	index := self inputWidgets indexOf: column.
	index = self inputWidgets size 
		ifTrue: [ ^self ].
	( ( self inputWidgets at: index + 1 ) at: row ) haveFocus!

setInputFocus
	
	self desiresFocus
		ifTrue: [self haveFocus]
	!

show
	super show.
	self headingWidgets do: [ :each | each show ].
	self inputWidgets do: [ :column | column do: [ :each | each show ] ].
!

showGraphicBorder: boolean focus: hasFocus in: rcBounds 
	"1 halt."
	super showGraphicBorder: boolean focus: hasFocus in: rcBounds !

tableColumnCount
	^self tableColumns size
	!

tableColumns
	^tableColumns!

tableRowCount
	^self rows
	!

tabRequestedFrom: aWidget forward: isForward 

	| found |
	found := false.
	self inputWidgets isEmpty 
		ifTrue: [ ^super tabRequestedFrom: aWidget forward: isForward  ].
	self lastEditableInputWidget isNil
		ifTrue: [ ^false ].
	aWidget = self lastEditableInputWidget
		ifTrue: [ ^false ].
	isForward
		ifTrue: [ self inForwardReadOrderDo: [ :input | 
			found
				ifTrue: [ input editable
						ifTrue:[^input tabRequestedFrom: aWidget forward: isForward ]]
				ifFalse: [ found := input editable & (input = aWidget) ] ] ]
		ifFalse: [ self inBackwardReadOrderDo: [ :input | 
			found
				ifTrue: [ input editable
					ifTrue:[^input tabRequestedFrom: aWidget forward: isForward ]]
				ifFalse: [ found := input editable & (input = aWidget) ] ] ].
	"if this point is reached then leave the columns"
	^super tabRequestedFrom: self forward: isForward.!

topRow
	^self minRow + 1!

updateAppearanceIn: region 

	super updateAppearanceIn: region.
	self headingWidgets do: [ :each | each updateAppearanceIn: region ].
	self inputWidgets do: [ :columnWidgets | columnWidgets do: [ :each | each updateAppearanceIn: region ] ].
!

updateContentsIn: region 

	self updateRowContents.
	"self updateUnusedColumnsContents."
	self updateHeaderContents.
	self updateSelection.!

updateHeaderContents

	self headingWidgets do: [ :each | each updateContentsIn: self bounds ]!

updateRowContents

	| offset |
	visibleItemRange do: [ :tableRow | 
		 | cells | 
		offset := visibleItemRange first.
		cells := self items at: tableRow + 1.
		cells doWithIndex: [ :each :tableColumn | 
			 | input columnDef | 
			input := self inputAt: ( tableRow - offset + 1 ) @ tableColumn.
			columnDef := tableColumns at: tableColumn.
			input editable: true.
			input alignment: (columnDef alignment ifNil: [ 1 ]).
			input string: each ] ].
	self items size + 1 
		to: self rows - 1 
		do: [ :tableRow | 
			1 
				to: self tableColumnCount 
				do: [ :tableColumn | 
					 | input   | 
					input := self inputAt: tableRow @ tableColumn.
					input editable: false.
					input string: '' ] ].
	self selectionIndex > -1 
		ifTrue: [ self setHighlightLineAt: self selectionIndex to: true ].!

updateUnusedColumnsContents

	"There might be space between to most right
	tablecolumn and the most right column of the tablelist.
	Make sure it does not contain any characters"

	| left right |
	left := self lastUsedColumn + 1.
	right := self maxColumn.
	self minRow 
		to: self maxRow 
		do: [ :row | 
			self grid 
				row: row 
				from: left 
				to: right 
				put: '' ]!

widgetNamed: aString
	"Check for accessing a TableColumn"
	| dotStart  dotEnd columnIndex|
	(aString includes: $.)
		ifFalse:[^super widgetNamed: aString].
	dotStart := aString indexOf: $. startingAt: 1.
	dotEnd := aString indexOf: $. startingAt: dotStart + 1.
	columnIndex := (aString copyFrom: dotStart + 1 to: dotEnd - 1) asNumber + 1. "indices are zero-based"
	"for now assume it is the label"
	^(self tableColumns at: columnIndex) header!

widgetsDo: oneArgBlock

	super widgetsDo: oneArgBlock.
	self headingWidgets do: oneArgBlock.
	self allInputWidgetsDo: oneArgBlock.
	self tableColumns do: oneArgBlock.
!

widgetUnderPoint: rcPoint

	self widgetsDo:[ :each |
		(each editable and:[(each widgetUnderPoint: rcPoint) notNil])
			ifTrue: [ ^each ]].
	^super widgetUnderPoint: rcPoint! !
	

!TerminalMenuList class publicMethods !

tagID
	^29! !

!TerminalMenuList publicMethods !

activateRequestAt: index
	"Answer if ok"
	"Pre: 0 <= index <= menu items size"	

	| message |
	index = 0 ifTrue: [ ^self ].
	message := self messages at: index.
	self selectionIndex: index - 1. "selectionIndex is zero-based"
	message isNil 
		ifTrue: [ ^false ].
	message sendUsingHandler: self messageHandler.
	^true!

add: label key: character message: aMessage
	characters add: character.
	self labels: (self labels copyWith: label).
	messages add: aMessage.!

add: label message: aMessage
	self add: label key: (label isEmpty ifTrue: [nil] ifFalse: [label first]) message: aMessage!

addLine
	"LineIndices are zero-based"
	
	self add: '' message: nil.
	self lineIndices add: self labels size - 1.!

addSpace
	self add: '' message: nil!

broadcastSelectionIndex
	"Nobody listenes to the menu index..."!

characters
	^characters!

characters: anObject
	characters := anObject!

defaultControllerClass
	^TerminalMenuController!

howManyLines
	^self lineIndices size!

initialize
	super initialize.
	self characters: OrderedCollection new.	
	self labels: OrderedCollection new.
	self messages: OrderedCollection new.
	self lineIndices: OrderedCollection new.	
	self isPopUp: false.!

isPopUp
	^isPopUp!

isPopUp: anObject
	isPopUp := anObject!

isVertical
	^true
	!

kindDo: aRequestor
	"Dispatch the request to @aRequestor and answer its return value"
	^aRequestor doTerminalMenuList: self!

labels
	^self items!

labels: anObject
	self items: anObject!

labelWidth
	labelWidth isNil
		ifTrue: 
			["compute it" 
			labelWidth := 1.
			1 to: self items size do:[ :index | labelWidth := labelWidth max: (self printableItemAt: index - 1) size ]].
	^labelWidth!

labelWidth: anObject
	labelWidth := anObject!

lineIndices
	^lineIndices!

lineIndices: anObject
	lineIndices := anObject!

messages
	^messages!

messages: anObject
	messages := anObject!

messageToSendAt: index
	"Answer if ok. Index is zero-based"
	
	| message |
	message := self messages at: index + 1.
	self selectionIndex: index.
	^message!

nextSelectionIndexOrNil
	"Indices are zero-based"
	"skip lines"

	| oldIndex newIndex |
	oldIndex := self selectionIndex.
	[ newIndex := super nextSelectionIndexOrNil.
	newIndex = oldIndex 
		ifTrue: [ ^oldIndex ].
	self lineIndices includes: newIndex]
		whileTrue: 
			["temporary invalid selection"
			self selectionIndex: newIndex ].
	^newIndex!

previousSelectionIndexOrNil
	"Indices are zero-based"
	"skip lines"

	| oldIndex newIndex |
	oldIndex := self selectionIndex.
	[ newIndex := super previousSelectionIndexOrNil.
	newIndex = oldIndex 
		ifTrue: [ ^oldIndex ].
	self lineIndices includes: newIndex]
		whileTrue: 
			["temporary invalid selection"
			self selectionIndex: newIndex ].
	^newIndex!

printOn: aStream
	super printOn: aStream.
	self labels doWithIndex:[ :each :index | aStream cr ;print: index ;nextPut: $: ;nextPutAll: each  ]!

setHighlightLineAt: lineIndex to: boolean

	(self lineIndices includes: lineIndex)
		ifFalse: [super setHighlightLineAt: lineIndex to: boolean]!

updateContentsIn: region 
	super updateContentsIn: region.
	self updateFocus: true. 
	 	"should this be done by super?"
	self showSeparatingGraphicBorder: false in: self bounds. 
	 	"set lines"
	self lineIndices do: [ :lineIndex | ( visibleItemRange includes: lineIndex ) 
		ifTrue: 
			[ self grid 
				charactersInBounds: ( self selectionBoundsAt: lineIndex) 
				do: [ :tc | tc  showSeparatorHorizontal: true ; localUpdate ] ] ]!

wantsRangeStatus
	^false! !
	

!TerminalMenuBar class publicMethods !

tagID
	^6! !

!TerminalMenuBar publicMethods !

activateRequestAt: index
	"Answer if ok"
	
	self selectionIndex: index.
	self showMenu.
	"?"
	^true!

add: label key: character menu: aMenu

	self labels: (self labels copyWith: label).
	self characters add: character.
	self menus add: aMenu.
	
	aMenu bounds: (self computeBoundsForMenuAt: self menus size).
	aMenu isPopUp: true.!

bounds: aBounds 
	super bounds: aBounds.
	1 to: self menus size do: [ :i | (self menus at: i) bounds: ( self computeBoundsForMenuAt: i) ]!

broadcastSelectionIndex
	"Nobody listenes to the menu index..."!

characters
	^characters!

characters: anObject
	characters := anObject!

clearDisplay: shouldRefresh
	"Make sure no menu is open"
	
	self hideMenu.
	super clearDisplay: shouldRefresh!

computeBoundsForMenuAt: index

	| left right bottom menu |
	menu := self menus at: index.
	left := self computeColumnStartForMenuAt: index.
	right := left + menu labelWidth - 1.
	bottom := self minRow + menu labels size.
	^(self minRow + 1) @ left corner: bottom @ right!

computeColumnStartForMenuAt: index

	| fromColumn  toColumn|
	toColumn := self minColumn - 1.
	1 to: index do:[ :i |
		fromColumn := toColumn + 1.
		toColumn := fromColumn + (self labels at: i) size - 1].
	^fromColumn!

currentMenu
	"Pre: one menu is open"
	^self menus at: self openMenuIndex!

defaultControllerClass
	^TerminalMenuBarController!

desiresFocus
	^true!

hasOpenMenu
	^self openMenuIndex > 0!

hideMenu

	"Hide current open menu"

	| menu |
	self openMenuIndex = 0 
		ifFalse: 
			[ menu := self menus at: self openMenuIndex.
			self parent remove: menu.
			self openMenuIndex: 0]!

initialize
	super initialize.
	self characters: OrderedCollection new.
	self menus: OrderedCollection new.	
	self openMenuIndex: 0.
	self useNative: false.!

isVertical
	^false
	!

kindDo: aRequestor
	"Dispatch the request to @aRequestor and answer its return value"
	^aRequestor doTerminalMenuBar: self!

labels
	^self items!

labels: aCollection
	self items: aCollection!

menus
	^menus!

menus: anObject
	menus := anObject!

openMenuIndex
	^openMenuIndex!

openMenuIndex: anObject
	openMenuIndex := anObject!

release
	super release.
	self menus do:[ :each | each release ]!

setHighlightLineAt: int to: bool 
	"Index is ZERO-based"

	| fromColumn  toColumn|
	toColumn := self minColumn - 1.
	0 to: int do:[ :i |
		fromColumn := toColumn + 1.
		toColumn := fromColumn + (self labels at: i+1) size - 1].
	self grid 
		charactersOn: self minRow
		from: fromColumn
		to: toColumn
		do: [ :char | char select: bool ;localUpdate ]!

showMenu

	"Hide current open menu and show the current selected.
	Selection indices are zero-based"

	| menu |
	self openMenuIndex > 0 
		ifTrue:[self hideMenu].
	self selectionIndex = -1
		ifTrue: [^self].
	self openMenuIndex: self stSelectionIndex.
	menu := self menus at: self openMenuIndex.
	self parent add: menu.
	menu show.
	menu selectionIndex: 0.
	menu controller gettingFocus!

showNextMenu
	self hasOpenMenu
		ifTrue: 
			[self hideMenu.
			self controller keyRight.
			self controller keyDown]!

showPreviousMenu
	self hasOpenMenu 
		ifTrue: 
			[ self hideMenu. 
			self controller keyLeft.
			self controller keyDown ]!

translateBy: aPoint
	super translateBy: aPoint.
	self menus do:[ :each | each translateBy: aPoint ]!

updateContentsIn: region

	| column |
	column := self minColumn.
	self items doWithIndex: [ :item :index | 
		| labelWidth |
		labelWidth := (self labels at: index) size.
		self grid 
			row: self minRow
			from: column 
			to: column + labelWidth - 1 
			put: item.
		column := column + labelWidth ]!

useNative
	^useNative!

useNative: anObject
	useNative := anObject!

wantsFocusOnTab
	^true!

wantsRangeStatus
	^false! !
	

!TerminalDropDownList class publicMethods !

tagID
	^15! !

!TerminalDropDownList publicMethods !

addFormDataTo: aMap
	"Answer whether data was inserted"
	
	aMap at: self name put: self selectedItem.
	^true!

defaultControllerClass
	^TerminalDropDownListController!

desiresFocus
	^true!

initialize
	super initialize.
	self maximumVisibleRows: -1. "not set"!

initializeRowVisibilty

	"If the row maximum is not set, then determine it using the items
	and the available rows on the top window"

	maximumVisibleRows = -1
		ifTrue: [ self maximumVisibleRows: ( self items size min: self topWidget rows - self minRow ) ]
		ifFalse: [ self maximumVisibleRows: ( self maximumVisibleRows min: self topWidget rows - self minRow ) ]!

kindDo: aRequestor
	^aRequestor doTerminalDropDownList: self!

maximumVisibleRows
	^maximumVisibleRows = -1
		ifTrue: [ self items size ]
		ifFalse: [ maximumVisibleRows ]!

maximumVisibleRows: anObject
	maximumVisibleRows := anObject!

reset
	"Initially, no items are visible"
	super reset.
	visibleItemRange := #()!

selectedItemString
	^self selectedItem isNil
		ifTrue: 
			[ '' ]
		ifFalse: 
			[ self selectedItem isString
				ifTrue: [ self selectedItem ]
				ifFalse: [ self selectedItem asString ] ]!

updateContentsIn: region 

	"Pre: region is within the receiver's bounds"

	self updateSelection.
	self grid 
		row: self minRow 
		from: self minColumn 
		to: self maxColumn - 1 
		put: self selectedItemString.
	(self grid row: self minRow column: self maxColumn)
		character: $
		;showChoiceMarker: true
		;localUpdate!

updateRangeStatus

	| stat |
	self visibleItemRange isEmpty 
		ifTrue: [ ^self form status: '' ].  "Need to find out why: EM"
	self selectionIndex = -1
		ifTrue: [ ^self form status: '' ].
	stat := WriteStream on: String new.
	stat print: self selectionIndex + 1.
	stat nextPutAll: ' of '.
	stat print: self items size. 
	self form status: stat contents!

updateSelection

	self selectionIndex > -1
		ifTrue: [ self setHighlightLineAt: self selectionIndex to: self hasFocus ]!

widgetUnderPoint: rcPoint
	"If we are showing the list, then expand my detection bounds"
	
	(super widgetUnderPoint: rcPoint) == self
		ifTrue: [ ^self ].
	^nil! !
	

!TerminalLabel class publicMethods !

initializeAfterLoad
	TTableColumn labelClass: TerminalLabel!

tagID
	^21! !

!TerminalLabel publicMethods !

alignment
	^alignment!

alignment: constant
	"Alignment constants: 1=left, 2=center, 3=right"
	
	alignment := constant.
	self markDirty: TMAlignmentSet.
	
	!

changedString
	self broadcast: TWidgetEvent string with: self string!

editable
	^false!

initialize
	super initialize.
	" Default label is an empty label "
	string := ''.
	self alignment: 1 "left"!

kindDo: aRequestor
	^aRequestor doTerminalLabel: self!

label
	^self string!

label: aString
	self string: aString.!

sendClickedMessage
	self triggerEvent: TWidgetEvent clicked!

setString: aString 
	string := aString.
	self visible == true 
		ifTrue: [ self updateContents ].
	self markDirty: TMStringSet!

string
	^string!

string: aString 
	self setString: aString.
	self changedString!

updateContentsIn: region 

	"Use alignment setting"

	| label |
	label := String new: self maxColumn - self minColumn + 1 withAll: $ .
	self alignment = 1  "left" 
		ifTrue: 
			[ 1 
				to: ( self string size min: label size ) 
				do: [ :i | label at: i put: ( self string at: i ) ] ].
	self alignment = 2  "middle" 
		ifTrue: 
			[  | offset | 
			offset := ( self columns - self string size ) // 2.
			1 
				to: ( self string size min: label size ) 
				do: [ :i | label at: i + offset put: ( self string at: i ) ] ].
	self alignment = 3  "right" 
		ifTrue: 
			[  | offset | 
			offset := ( self columns - self string size ) max: 0.
			1 
				to: ( self string size min: label size ) 
				do: [ :i | label at: i + offset put: ( self string at: i ) ] ].
	self 
		putLine: label 
		startingAt: 1 
		from: self minColumn 
		to: self maxColumn 
		on: self minRow!

wantsFocusOnTab
	^false! !
	

!TerminalInput class publicMethods !

tagID
	^1!

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalInput" superclass="TerminalBasicWidget">
	<attribute name="string"/>
	<attribute name="readOnly"/>
	<attribute name="textlines"/>
	<attribute name="visibleLinesRange"/>
	<attribute name="isMultiLine"/>
	<attribute name="validateSelector"/>
	<operation name="losingFocus" return="Object"/>
	<operation name="deleteCharacterAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="textFromLines:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="deleteCarriageReturnAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="setVisibleLinesRangeStart:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="showCursorAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="linesFromText:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="bounds:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="insertCharacter:at:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="mergeWithLineAfter:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="copyLine:withCharacter:at:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="lineIndexAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="removeLineAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="characterIndexAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="paste:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="initializeLinesRange" return="Object"/>
	<operation name="addTagAttributesTo:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="cursorAtEnd" return="Object"/>
	<operation name="updateContentsIn:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="attributeValueAt:put:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="gettingFocus" return="Object"/>
	<operation name="copyLine:withoutCharacterAt:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="insertCarriageReturnAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="addStateMessagesTo:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="lineBeginAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="setVisibleLinesRangeEnd:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="inspectActions" return="Object"/>
	<operation name="updateFrom:to:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="replaceCharacter:at:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="cursorAtHome" return="Object"/>
	<operation name="insertSimpleCharacter:at:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="defaultControllerClass" return="Object"/>
	<operation name="insertString:at:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="computeCursorAfterInserting:at:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="extendSelectAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="putLine:startingAt:from:to:on:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
		<parameter type="Object"/>
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="wantsFocusOnTab" return="Object"/>
	<operation name="desiresFocus" return="Object"/>
	<operation name="lineCharacterPointAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="replaceLineAt:with:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="lineEndAt:" return="Object">
		<parameter type="Object"/>
	</operation>
	<operation name="insertLine:after:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
	<operation name="replaceSimpleCharacter:at:" return="Object">
		<parameter type="Object"/>
		<parameter type="Object"/>
	</operation>
</class>'! !

!TerminalInput publicMethods !

addFormDataTo: aMap
	"Answer whether data was inserted"
	
	self editable not ifTrue: [ ^false ].
	aMap at: self name put: self string.
	^true!

changeSelectWith: lcPoint
	"Modify the selection:
	- begin it
	- extend it
	- shrink it
	- end it" 

	self hasSelection
		ifTrue: 
			[ 
			self updateSelectionBlock: false. 
			selectionStop := self lineCharacterPointAt: lcPoint.
		 	self updateSelectionBlock: true]
		ifFalse:
			[selectionStart := self lineCharacterPointAt: lcPoint.
			selectionStop := selectionStart.
			self updateSelectionBlock: true ].!

clearSelection
	self updateSelectionBlock: false.
	selectionStart := 0@0.
	selectionStop := 0@0.!

computeCursorAfterInserting: howMany at: rcPoint 
	"Compute the cursor position after inserting @howMany non-CR characters starting at @rcPoint"

	| lineIndex characterIndex end |
	lineIndex := self lineIndexAt: rcPoint.
	characterIndex := self localColumnAt: rcPoint.
	end := characterIndex + howMany.
	[end > self columns] whileTrue: 
			[lineIndex := lineIndex + 1.
			end := end - self columns].
	^self origin + (lineIndex @ end) - (1 @ 1)!

copyLine: line withCharacter: aCharacter at: characterIndex 

	^( line copyFrom: 1 to: characterIndex - 1 ) , ( String with: aCharacter ) , ( line copyFrom: characterIndex to: line size )!

copyLine: line withoutCharacterAt: characterIndex

	^(line copyFrom: 1 to: characterIndex - 1) , (line copyFrom: characterIndex + 1 to: line size)
	!

cursorAtEnd
	^self isMaxLengthDefined
		ifTrue: [ self minRow @ ( self minColumn + ( self string size min: self maxLength ) - 1 - self controller visibleOffset ) ]
		ifFalse: [ self minRow @ ( self minColumn + self string size - 1 - self controller visibleOffset ) ]!

defaultControllerClass
	^self isMultiLine
		ifTrue: [ TerminalTextController ]
		ifFalse: [ TerminalInputController ]!

deleteCharacterAt: rcPoint 
	| newLine characterIndex |
	characterIndex := (self localColumnAt: rcPoint) + self controller characterOffset.
	newLine := self copyLine: self string withoutCharacterAt: characterIndex.
	self setString: newLine!

desiresFocus
	^self editable!

displayString
	self type isNil 
		ifTrue: [ ^string ].
	self type isEmpty 
		ifTrue: [ ^string ].
	self isPassword 
		ifTrue: [ ^String new: string size withAll: $x ]. 
	 	" Case ELSE: unknown type "
	^string!

editable
	^self readOnly not!

editable: bool
	self readOnly: bool not!

hasReachedMaxLength
	^self string size = self maxLength!

hasSelection
	^selectionStart x ~= 0!

initialize
	super initialize.
	self editable: true.
	self maxLength: -1.
	string := ''.
	selectionStart := 0@0.
	selectionStop := 0@0.!

insertCharacter: aCharacter at: rcPoint 
	"pre: Non-CR"
	self insertSimpleCharacter: aCharacter at: rcPoint
!

insertSimpleCharacter: aCharacter at: rcPoint 
	| characterIndex newLine |
	characterIndex := (self localColumnAt: rcPoint) + self controller characterOffset.
	newLine := self 
				copyLine: self string
				withCharacter: aCharacter
				at: characterIndex.
	self setString: newLine!

insertString: aString at: aPoint
	aString reverse doWithIndex: [ :each :index | self insertCharacter: each at: aPoint + (Point x: index - 1 y: 0) ]!

invalidForegroundColor
	^Color red!

isMaxLengthDefined
	^maxLength > -1!

isMultiLine
	^false!

isPassword
	^type = 'password'!

kindDo: aRequestor
	"Dispatch the request to @aRequestor and answer its return value"
	^aRequestor doTerminalInput: self!

lineBeginAt: rcPoint
	
		^rcPoint x @ self minColumn!

lineCharacterPointAt: rcPoint 
	^1 @ (self localColumnAt: rcPoint)!

lineEndAt: rcPoint
	
	^rcPoint x @ ((self minColumn + self string size - 1) min: self maxColumn)!

maxLength
	^maxLength!

maxLength: anObject
	maxLength := anObject!

paste: aString 
	" Paste implementation is different from TerminalText implementation "
	self insertString: aString at: self controller cursor.!

readOnly
	^readOnly!

readOnly: bool 
	readOnly := bool.
	( bool
		and: [ self hasFocus ] ) 
		ifTrue: [ self form setFocusWidget: nil ].
	self markDirty: TMEditableSet!

replaceCharacter: aCharacter at: rcPoint 

	aCharacter = Character cr
		ifTrue:[^self].
	self replaceSimpleCharacter: aCharacter at: rcPoint
!

replaceSimpleCharacter: aCharacter at: rcPoint

	| characterIndex newLine |
	characterIndex := self localColumnAt: rcPoint.
	newLine := (self string copy)
				at: characterIndex put: aCharacter;
				yourself.
	self setString: newLine!

showCursorAt: localPoint

	| column |
	column := ( localPoint y + self minColumn - 1 ) min: self maxColumn.
	self controller requestCursorAt: self minRow @ column!

string: aString 
	aString = string
		ifTrue: [ ^self ].
	"super does broadcast and update"
	super string: aString.
	self visible == true 
		ifTrue: 
			[ self form cursor isNil ifFalse:[self controller keyHome]]!

stringOffset
	^1 + self controller visibleOffset!

type
	^type!

type: aString
	"available:  password, zipcode
	"
	type := aString!

updateContentsIn: region 

	| display |
	display := self displayString.
	self 
		putLine: display 
		startingAt: self stringOffset
		from: self minColumn 
		to: ( self minColumn + display size - self stringOffset min: self maxColumn ) 
		on: self minRow!

updateSelectionBlock: isSelected

	| row column endColumn |
	row := selectionStart x.
	column := selectionStart y.
	selectionStop x = row 
		ifTrue: 
			[  "same line"
			endColumn := selectionStop y.
			column 
				to: endColumn 
				do: [ :c | 
					 | tc | 
					tc := self grid row: row column: c.
					tc select: isSelected ;localUpdate ] ]!

wantsFocusOnTab
	^self editable! !
	

!TerminalTimer class publicMethods !

tagID
	^100! !

!TerminalTimer publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	( self isDirty: TMRepeatSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMRepeatSet argument: self repeat ) ].
	( self isDirty: TMCountSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMCountSet argument: current ) ].
	( self isDirty: TMDelaySet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMDelaySet argument: self delayInSeconds ) ].
	( self isDirty: TMIntervalSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMIntervalSet argument: self interval ) ]!

delayInSeconds
	^delayInSeconds!

delayInSeconds: anObject 
	delayInSeconds = anObject 
		ifTrue: [ ^self ].
	delayInSeconds := anObject.
	self markDirty: TMDelaySet!

dispatchIndexedMessage: anIndexedMessage 
	TMIntervalSet " 524289 " = anIndexedMessage index 
		ifTrue: 
			[  | args | 
			args := anIndexedMessage arguments.
			^self interval: ( ( args at: 1 ) to: ( args at: 2 ) by: ( args at: 3 ) ) ].
	TMCountGet = anIndexedMessage index 
		ifTrue: [ ^current ].
	TMRepeatSet = anIndexedMessage index 
		ifTrue: [ ^self repeat: anIndexedMessage argument ].  
	TMRepeatGet = anIndexedMessage index 
		ifTrue: [ ^self repeat ].  
	TMDelaySet = anIndexedMessage index 
		ifTrue: [ ^self delayInSeconds: anIndexedMessage argument ].  
	TMDelayGet = anIndexedMessage index 
		ifTrue: [ ^self delayInSeconds ].  
		
	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage!

initialize
	super initialize.
	self interval: (0 to: 0 by: 1).
	self delayInSeconds: 1.
	self editable: false.
	self repeat: false!

interval
	^interval!

interval: anObject 
	interval = anObject 
		ifTrue: [ ^self ].
	interval := anObject.
	self markDirty: TMIntervalSet!

kindDo: aRequestor 
	^aRequestor doTerminalCounter: self!

repeat
	^repeat!

repeat: anObject 
	repeat = anObject 
		ifTrue: [ ^self ].
	repeat := anObject.
	self markDirty: TMRepeatSet! !
	

!TerminalText class publicMethods !

tagID
	^16! !

!TerminalText publicMethods !

bounds: aValue

	super bounds: aValue.
	self initializeLinesRange!

computeLocalPoint: textIndex 
	"TODO: rewrite this"
	
	| charIndex lineIndex lineSize |
	charIndex := 0.
	lineIndex := 1.
	[ lineIndex > textlines size
		ifTrue: 
			[ lineIndex := lineIndex - 1.
			charIndex := charIndex - (textlines at: lineIndex) size.
			false ]
		ifFalse: 
			[ lineSize := ( textlines at: lineIndex ) size.
			charIndex + lineSize < textIndex ] ]
		whileTrue: 
			[ charIndex := charIndex + lineSize.
			lineIndex := lineIndex + 1 ].
	^lineIndex @ ( textIndex - charIndex )!

computeRowCharacterPoint: textIndex

	| charIndex  lineIndex lineSize|
	charIndex := 0.
	lineIndex := 1.
	[ lineSize := (textlines at: lineIndex) size.
	charIndex + lineSize < textIndex ]
		whileTrue:
			[ charIndex := charIndex + lineSize.
			lineIndex := lineIndex + 1].
	^self rcPointFrom: ( lineIndex @ (textIndex - charIndex) )
	
		
	!

computeTextIndex: rcPoint 

	| lineIndex charIndex offset |
	lineIndex := self lineIndexAt: rcPoint.
	charIndex := self localColumnAt: rcPoint.
	offset := 0.
	1 
		to: lineIndex - 1 
		do: [ :i | offset := offset + ( textlines at: i ) size ].
	offset := offset + charIndex.
	^offset!

cursorAtEnd

	| maxLineIndex maxColumn |
	self textlines isEmpty
		ifTrue:[^self origin].
	maxLineIndex := self textlines size min: self rows.
	maxColumn := (self textlines at: maxLineIndex) size min: self columns - 1.
	^self origin + ((maxLineIndex - 1)@maxColumn)!

defaultControllerClass
	^TerminalTextController!

deleteCharacterAt: rcPoint 

	| old offset new |
	old := self stringNoLf.
	offset := self computeTextIndex: rcPoint. 
	"delete"
	new := old copyFrom: 1 to: offset - 1.
	new := new , ( old copyFrom: offset + 1 to: old size ).
	self string: new reset: false.
	self showCursorAt: ( self computeLocalPoint: offset )!

hyphenationString
	^hyphenationString!

hyphenationString: aString
	hyphenationString := aString!

initialize
	super initialize.
	textlines := OrderedCollection new.
	hyphenationString := '- &=<>'.
	tabSpacing := 2.
	!

initializeLinesRange

	visibleLinesRange := 1 to: self rows
	!

insertCharacter: aCharacter at: rcPoint 

	"Cases
			cr: begin, middle, end
			non-rc: begin, middle, end
	"
	aCharacter = self lineSeparator
		ifTrue: [ self insertString: ( String with: self lineSeparator ) at: rcPoint ]
		ifFalse: [ self insertString: ( String with: aCharacter ) at: rcPoint ]!

insertString: aString at: rcPoint

	| old  offset new|
	old := self stringNoLf.
	offset := self computeTextIndex: rcPoint.
	offset > old size
		ifTrue:
			[ "append" 
			new := old , aString]
		ifFalse:
			["insert"			
			new := old copyFrom: 1 to: offset - 1.
			new := new , aString.
			new := new , ( old copyFrom: offset to: old size) ].
	self string: new reset: false.
	self showCursorAt: (self computeLocalPoint: offset) + (0@aString size)!

inspectActions
	^super inspectActions , #( stats )!

isMultiLine
	^true!

kindDo: aRequestor
	"Dispatch the request to @aRequestor and answer its return value"
	^aRequestor doTerminalText: self!

lineCharacterPointAt: rcPoint 
	^(self lineIndexAt: rcPoint) @ (self localColumnAt: rcPoint)!

lineEndAt: rcPoint
	"Answer the absolute row-column point of the last character on the row rcPoint.x"

	| lineIndex  charIndex line|
	lineIndex := self lineIndexAt: rcPoint.
	line := lineIndex > textlines size "new last line"
		ifTrue: ['']
		ifFalse: [textlines at: lineIndex].
	line isEmpty
		ifTrue: [charIndex := 1]
		ifFalse: [charIndex := line size ].
	lineIndex = textlines size
		ifTrue: [charIndex := charIndex min: self columns].
	(line notEmpty and:[(line at: charIndex) == Character cr])
		ifTrue: [charIndex := charIndex - 1 min: self columns].
	^rcPoint x @ ( self minColumn + charIndex - 1)!

lineIndexAt: rcPoint 
	^rcPoint x - self minRow + visibleLinesRange first!

lineSeparator
	^Character cr!

linesFromText: aString 
	"aString may use CRLF (Windows) or just LF (X/Motif,Java) for line delimiter
	Here, lines are terminated by CR only
	Return
		<OrderedCollection of: String>unterminated comment"

	| noLF  expandTabs|
	noLF := aString copyWithout: Character lf.
	expandTabs := noLF copyReplaceAll: (String with: Character tab) with: (String new: self tabSpacing withAll: $ ).	
	^WordWrapper 
		on: expandTabs
		columns: self columns
		hyphenations: self hyphenationString
		tabSpacing: self tabSpacing!

rcPointFrom: lcPoint 

	^lcPoint x - visibleLinesRange first + self minRow @ ( self minColumn + lcPoint y - 1 )!

replaceSimpleCharacter: aCharacter at: rcPoint 

	"Non-CR"

	| old offset new |
	old := self stringNoLf.
	offset := self computeTextIndex: rcPoint. 
	"replace"
	new := old copyFrom: 1 to: offset - 1.
	new := new copyWith: aCharacter.
	new := new , ( old copyFrom: offset + 1 to: old size ).
	self string: new reset: false.
	self showCursorAt: ( self computeLocalPoint: offset ) + ( 0 @ 1 )!

setVisibleLinesRangeEnd: lineIndex
	visibleLinesRange := lineIndex - self rows + 1 to: lineIndex!

setVisibleLinesRangeStart: lineIndex
	visibleLinesRange := lineIndex to: lineIndex + self rows - 1!

showCursorAt: lcPoint
	"May need to scroll up or down (thus modifying the visibleLinesRange)
	before setting the cursor of the controller.
	Pre: lcPoint is within the text of the receiver"
	
	| rcPoint |
	"going to next line?"
	lcPoint y > self columns
		ifTrue: [^self showCursorAt: (lcPoint x + 1) @ 1].
	"going past end of line?"
	(self textlines size >= lcPoint x) 
		ifTrue: 
			[ | line |
			line := self textlines at: lcPoint x.
			(lcPoint y > 1 and:[ (line at: lcPoint y - 1) = self lineSeparator ])
				ifTrue:[^self showCursorAt: (lcPoint x + 1) @ 1]].
	"going to previous line?"
	lcPoint y < 1
		ifTrue: 
			[ | previousLineSize |
			previousLineSize := (textlines at: lcPoint x - 1) size.
			^self showCursorAt: (lcPoint x - 1) @ previousLineSize].
	lcPoint x > visibleLinesRange last
		ifTrue: 
			[self setVisibleLinesRangeEnd: lcPoint x.
			self updateContents.
			^self showCursorAt: lcPoint].
	lcPoint x < visibleLinesRange first
		ifTrue: 
			[self setVisibleLinesRangeStart: lcPoint x.
			self updateContents.
			^self showCursorAt: lcPoint].
	rcPoint := self rcPointFrom: lcPoint.
	self controller requestCursorAt: rcPoint
		
	!

stats

	| aStream |
	aStream := String new writeStream.
	textlines doWithIndex:[ :each :index |
		aStream cr.
		index printOn: aStream.
		aStream nextPut: $:  .
		each printOn: aStream ].
	aStream cr ;print: visibleLinesRange.
	^aStream contents!

string
	^self textFromLines: textlines!

string: aString
	self string: aString reset: true!

string: aString reset: resetScrolling 

	"Assume the string is going to be changed"

	textlines := self linesFromText: aString.
	resetScrolling 
		ifTrue: [ self initializeLinesRange ].
	super string: aString asString.
	self visible == true 
		ifTrue: [ self updateContents ]!

stringNoLf
	"temp"
	^string copyWithout: Character lf!

tabSpacing
	^tabSpacing!

tabSpacing: posInt
	tabSpacing := posInt!

textFromLines: aTextCollection 

	"CR is expanded to CRLF, TABS are merged"

	| out line linesIn charsIn char |
	out := WriteStream on: String new.
	linesIn := ReadStream on: aTextCollection.
	[ linesIn atEnd ]
		whileFalse: 
			[ line := linesIn next.
			charsIn := ReadStream on: line.
			[ charsIn atEnd ]
				whileFalse: 
					[ char := charsIn next.
					char == CR
						ifTrue: 
							[ out  nextPut: CR ; nextPut: LF ]
						ifFalse: 
							[ char == TAB 
								ifTrue: [ self tabSpacing - 1 timesRepeat: [ charsIn next ] ].
							out nextPut: char ] ] ].
	^out contents!

textlines
	^textlines!

updateContentsIn: region

	self updateFrom: visibleLinesRange first to: visibleLinesRange last!

updateFrom: startIndex to: stopIndex 

	"indices in the collection of textlines
	must be converted to rowIndices using
	the visibleLinesRange"

	| line |
	( startIndex max: visibleLinesRange first ) to: ( stopIndex min: visibleLinesRange last ) do: [ :index | 
		index <= textlines size
			ifTrue: [ line := textlines at: index ]
			ifFalse: [ line := '' ].
		self 
			putLine: line 
			startingAt: 1 
			from: self minColumn 
			to: self minColumn + line size - 1 
			on: self minRow + index - visibleLinesRange first ]! !
	

!TerminalTableCell publicMethods !

updateBorderAppearance

	self visibleAppearance borderColor notNil 
		ifTrue: 
			[ self minColumn 
				to: self maxColumn 
				do: [ :column | ( self grid row: self maxRow column: column )  showBorderBottom: true ; localUpdate ] ]! !
	

!TerminalCheckBox class publicMethods !

tagID
	^24! !

!TerminalCheckBox publicMethods !

addFormDataTo: aMap
	"Answer whether data was inserted"
	
	aMap at: self name put: self selection.
	^true!

boundsForFocus

	self minColumn = self maxColumn
		ifTrue:[^self bounds].

	^self tickAlignment = 1  "left"
		ifTrue: [ self minRow @ ( self minColumn + 1 ) corner: self bounds corner ]
		ifFalse: [ self bounds origin corner: ( self maxRow @ (self maxColumn - 1 )) ]!

changedSelection
	"Update the tick"
	self visible ifTrue:[self updateTickIn: self bounds].!

defaultControllerClass
	^TerminalCheckBoxController!

desiresFocus
	^true!

editable
	^true!

getSelection
	^selection!

initialize
	super initialize.
	self alignment: 3. "right"
	self selection: false.
	self tickAlignment: 1 "left"!

kindDo: aRequestor
	^aRequestor doTerminalCheckBox: self!

selection
	^selection!

selection: anObject

	selection = anObject
		ifTrue: [ ^self ].
	selection := anObject.
	self markDirty: TMSelectionSet.
	self changedSelection.
	self visible
		ifTrue: [ self updateContents ]!

setSelection: bool
	self selection: bool.
	self broadcast: TWidgetEvent selectedItem with: self selection.!

tickAlignment
	^tickAlignment!

tickAlignment: constant
	"There are two alignments for the tick: 1=Left and 2=Right (default)"
	tickAlignment := constant.
	self markDirty: TMAlignmentSet.!

tickPosition

	^self tickAlignment = 1 "left"
		ifTrue: [ self minRow @ self minColumn ]
		ifFalse: [self minRow @ self maxColumn ]!

toggleSelection
	self setSelection: self selection not!

updateContentsIn: region 

	super updateContentsIn: region.
	self updateTickIn:region.
	!

updateTickIn: region 

	| tickPosition tchar |
	tickPosition := self tickPosition.
	tchar := self grid row: tickPosition x column: tickPosition y. 
	tchar character: $ . "make sure no char is displayed"
	tchar showCheckBox: true.
	tchar showCheckboxTick: self selection. 
	 	"finally"
	tchar localUpdate 
	!

wantsFocusOnTab
	^true! !
	

!TerminalRadioButton class publicMethods !

tagID
	^25! !

!TerminalRadioButton publicMethods !

addFormDataTo: aMap
	"Answer whether data was inserted"
	
	self selection 
		ifTrue: 
			[ aMap at: self name put: self item.
			^true].!

changedSelection

	"All other radion buttons in the same group need 
	to be deselected when the receiver is selected"

	| others |
	self visible 
		ifFalse: [ ^self ].
	super changedSelection.
	self selection 
		ifFalse: [ ^self ].
	others := self parent widgets select: [ :each | each ~~ self
		and: [ each class == self class
			and: [ each groupName = self groupName ] ] ].
	others do: [ :each | each setSelection: false ]!

groupName
	^groupName!

groupName: anObject
	groupName := anObject!

item
	^item!

item: anObject
	item := anObject.
	self markDirty: TMItemSet!

kindDo: aRequestor
	^aRequestor doTerminalRadioButton: self!

show
	super show.
	self changedSelection.!

toggleSelection
	"Only if deselected, the receiver can be selected"
	self selection
		ifFalse: [super toggleSelection]!

updateTickIn: region 

	| tickPosition tchar |
	tickPosition := self tickPosition.
	tchar := self grid row: tickPosition x column: tickPosition y. 
	tchar character: $ . "make sure no char is displayed"	
	tchar showRadioCircle: true.
	tchar showRadioTick: self selection. 
	 	"finally"
	tchar localUpdate ! !
	

!TerminalButton class publicMethods !

tagID
	^2!

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalButton" superclass="TerminalInput">
	<attribute name="clickedMessage" type="TerminalMessageSend"/>
	<operation name="defaultControllerClass" return="TerminalButtonController"/>
	<operation name="desiresFocus" return="Boolean"/>
	<operation name="gettingFocus"/>
	<operation name="label">
		<parameter type="String"/>
	</operation>
	<operation name="losingFocus" return="Object"/>
	<operation name="sendClickedMessage" return="Object"/>
	<operation name="setHighlightTo:">
		<parameter type="Boolean"/>
	</operation>
</class>'! !

!TerminalButton publicMethods !

accelerator
	^accelerator!

accelerator: anObject
	accelerator := anObject!

default
	^default!

default: showAsDefault
	default := showAsDefault!

defaultControllerClass
	^TerminalButtonController!

desiresFocus
	^true!

emphasizeAccelerator
	self accelerator isNil
		ifTrue: [ ^self ].
	self string doWithIndex:[ :char :index |
		char == self accelerator
			ifTrue: [ | tc app |
				tc := self grid row: self minRow column: self minColumn + index - 1.
				app := tc appearance copy.
				app foreground: app selectionForeground. 
				tc appearance: app ;localUpdate ]]
!

help
	help isNil ifTrue: [ ^'' ].
	^help!

help: anObject
	help := anObject!

initialize
	super initialize.
	" Override label alignment to show button label centered "
	self alignment: 2 "middle".
	self default: false.!

isDefault
	^default!

kindDo: aRequestor
	^aRequestor doTerminalButton: self!

label
	^self string trimBlanks!

label: aString
	"Make sure the label does fit "
	
	| label |
	label := aString size > self columns
		ifTrue:[(aString copyFrom: 1 to: self columns - 2) , '..']
		ifFalse:[ aString].
	super label: label!

stringOffset
	^1!

updateContentsIn: region 
	super updateContentsIn: region.
	self emphasizeAccelerator!

wantsFocusOnTab
	^true! !
	

!TerminalImageLabel class publicMethods !

tagID
	^8! !

!TerminalImageLabel publicMethods !

aboutToRemoveFromTerminalForm: aForm 

	super aboutToRemoveFromTerminalForm: aForm.
	aForm graphics removeGO: self image fromLayer: 1.
	aForm grid 
		charactersInBounds: self bounds 
		do: [ :char | char visible: true ].
!

changedFont
"	self image initializeImageBox: Terminal currentFontMetrics width height: Terminal currentFontMetrics height at: self bounds"!

defaultControllerClass
	^TerminalButtonController!

desiresFocus
	^true!

image

	image isNil 
		ifTrue: 
			[ image := TerminalImage for: self.
			image file: self url.
			image scaleToFit: self scaleToFit].
	^image!

initialize
	super initialize.
	self scaleToFit: true!

justAddedToTerminalForm: aForm 

	" pass image a rectangle to make sure it knows how large it is. "
	self image location: self bounds appearance: self topWidget appearance.
	aForm grid 
		charactersInBounds: self bounds 
		do: [ :char | char visible: false ].
	aForm graphics addGO: self image toLayer: 1 visible: false!

kindDo: aReq
	^aReq doTerminalImageLabel: self!

scaleToFit
	^scaleToFit!

scaleToFit: anObject

	scaleToFit = anObject
		ifTrue: [ ^self ].	
	scaleToFit := anObject.
	image notNil ifTrue: [ image scaleToFit: anObject ].
	self markDirty: TMScaleToFitSet.!

sendClickedMessage
	self triggerEvent: TWidgetEvent clicked!

updateContentsIn: region
	super updateContentsIn: region.
	self image redraw.!

url
	^url!

url: anObject 
	url = anObject 
		ifTrue: [ ^self ].
	url := anObject.
	self markDirty: TMURLSet!

wantsFocusOnTab
	^false! !
	

!TerminalCharacterGrid class publicMethods !

new
	^self rows: 1 columns: 1!

rows: rows columns: columns

	^self basicNew setMatrix: ((Array new: rows) collect:[ :ar | Array new: columns ]) ;initializeCharacters! !

!TerminalCharacterGrid publicMethods !

addToGraphicsIn: aForm withAppearance: appearance

	| width height |
	width := appearance characterWidth.
	height := appearance characterHeight.
	1 to: self rows do:[ :r |
		1 to: self columns do:[ :c |
			| tc |
			tc := self row: r column: c.
			aForm addGraphicalObject: tc.
			tc setAppearance: appearance.
			tc initializeBox: width height: height at: (Point x: r y: c) ]]!

at: rowColumnPoint
	^(self row: rowColumnPoint x column: rowColumnPoint y) string!

at: rowColumnPoint put: characterOrString
	"slow version"
	
	characterOrString isCharacter
		ifTrue: [ ^(self row: rowColumnPoint x column: rowColumnPoint y) character: characterOrString].
	rowColumnPoint y to: rowColumnPoint y + characterOrString size - 1 do:[ :i |
		(self row: rowColumnPoint x column: i) character: (characterOrString at: i - rowColumnPoint y + 1) ] !

at: rowColumnPoint size: howMany
	^self stringAt: rowColumnPoint x column: rowColumnPoint y size: howMany!

characterAt: rcPoint
	^(self row: rcPoint x column: rcPoint y) character!

charactersDo: aBlock 
	self charactersInBounds:(1@1 corner: self rows@self columns) do: aBlock!

charactersInBounds: aRectangle do: aBlock
	| from  to|
	from :=  aRectangle origin.
	to := aRectangle corner.
	from x to: to x do:[ :r |
		from y to: to y do:[ :c |
			aBlock value: (self row: r column: c)]]!

charactersOn: row from: startColumn to: stopColumn do: oneArgBlock

	startColumn to: stopColumn do: [:column | oneArgBlock value: (self row: row column: column) ]!

columns
	matrix isEmpty ifTrue: [^0].
	^(matrix at: 1) size!

copyFrom: startPoint to: endPoint 

	"Answer a new Grid from the selection between startPoint and endPoint"

	| newMatrix |
	newMatrix := Array new: endPoint x - startPoint x + 1 withAll: ( Array new: endPoint y - endPoint y + 1 ).
	^self class basicNew setMatrix: newMatrix!

includesLocation: aPoint 
	^aPoint <= self size!

initializeCharacters
	| char |
	1 to: self rows do:[ :r |
		1 to: self columns do:[ :c |
			char := TerminalCharacter new.
			self row: r column: c put: char.]]!

matrix

	"Answer the value of matrix"

	^matrix!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $[ ;print: self size ;nextPut: $]!

reInitializeBoxes: fontMetricsProvider

	| width height |
	width := fontMetricsProvider currentFontMetrics width.
	height := fontMetricsProvider currentFontMetrics height.
	1 
		to: self rows 
		do: [ :r | 
			1 
				to: self columns 
				do: [ :c | | tc |
					tc := self row: r column: c.
					tc initializeBox: width height: height at: ( Point x: r y: c ) ] ]!

removeFromGraphics: aForm
	self charactersDo:[ :ch | aForm removeGraphicalObject: ch ].!

row: r column: c
	^(matrix at: r) at: c!

row: r column: c put: termchar
	(matrix at: r) at: c put: termchar!

row: row from: startColumn to: stopColumn put: aString
	"
	If aString is too short, append spaces
	If aString is too long, truncate it
	"
	1 to: (aString size min: stopColumn - startColumn + 1) do:[ :index |
		(self row: row column: startColumn + index - 1) 
			clearSeparators
			; character: (aString at: index)
			; localUpdate ].
	startColumn + aString size  to: stopColumn do:[ :column |
		(self row: row column: column)
		 	clearSeparators
			; character: $
			; localUpdate ]!

rows
	^matrix size!

setMatrix: aMatrix
	matrix := aMatrix!

size
	^self rows@self columns!

stringAt: r column: c size: howMany
	"slow version, no wrap"
	
	| out |
	out := WriteStream on: (String new: howMany).
	c to: c + howMany - 1 do:[ :i |
		out nextPut: (self row: r column: i) character ].
	^out contents! !
	

!TerminalDialog class publicMethods !

tagID
	^30! !

!TerminalDialog publicMethods !

cancel
	^cancel!

cancel: aTerminalMessage
	cancel := aTerminalMessage!

cancelPressed
	cancel isNil ifFalse: [
		cancel evaluate ]!

kindDo: aHandler
	aHandler doTerminalDialog: self.!

no
	^no!

no: aTerminalMessage
	no := aTerminalMessage!

noPressed
	no isNil ifFalse: [
		no evaluate ].!

open
	" Should use constants for this "
	self type = 0 ifTrue: [ ^self okDialog ].
	self type = 1 ifTrue: [ ^self okCancelDialog ].
	self type = 2 ifTrue: [ ^self yesNoCancelDialog ].
	self type = 3 ifTrue: [ ^self warnDialog ].
	self type = 4 ifTrue: [ ^self infoDialog ].
	self type = 5 ifTrue: [ ^self errorDialog ].
	!

text
	^text!

text: aString
	text := aString!

type
	type isNil ifTrue: [ ^0 ].
	^type!

type: newType
	type := newType!

yes
	^yes!

yes: aTerminalMessage
	yes := aTerminalMessage!

yesPressed
	yes isNil ifFalse: [
		yes evaluate ].! !
	

!TerminalFontMetrics publicMethods !

bottomOffset
	^bottomOffset!

bottomOffset: anObject
	bottomOffset := anObject!

font
	^font!

fontName
	^fontName!

fontName: aFontName

	fontName := aFontName.
	self loadFont!

height
	^height!

height: int
	height := int!

isAvailable
	^self font notNil!

label
	^label!

label: anObject
	label := anObject!

printOn: aStream
	aStream nextPutAll: self label!

width
	^width!

width: int
	width := int! !
	

!TerminalKeyEvent class publicMethods !

codeMap
	CodeMap isNil 
		ifTrue: 
			[ CodeMap := Dictionary new.
			self initializeCodeMap ].
	^CodeMap!

initializeCodeMap
	^self subclassResponsibility!

singleton
	"For now"
	^self new! !

!TerminalKeyEvent publicMethods !

character
	self subclassResponsibility!

code
	^code!

code: anObject
	code := anObject!

initializeFromOSEvent: anEvent
	self subclassResponsibility!

isAlt
	self subclassResponsibility!

isAltPressed
	^isAlt!

isAltPressed: anObject
	isAlt := anObject!

isBackspace
	self subclassResponsibility!

isCharacter
	"Why not? (in this implementation) "
	"For reason why not, see StMt implementation "
	^true!

isControl
	^self subclassResponsibility!

isControlPressed
	^isControl!

isControlPressed: anObject
	isControl := anObject!

isDelete
	self subclassResponsibility!

isDown
	self subclassResponsibility!

isEnter
	self subclassResponsibility!

isEsc
	self subclassResponsibility!

isFunction
	^isFunction!

isFunction: anObject
	isFunction := anObject!

isInsert
	self subclassResponsibility!

isLeft
	self subclassResponsibility!

isPageDown
	self subclassResponsibility!

isPageUp
	self subclassResponsibility!

isRight
	self subclassResponsibility!

isShift
	self subclassResponsibility!

isShiftPressed
	^isShift!

isShiftPressed: anObject
	isShift := anObject!

isTab
	self subclassResponsibility!

isUp
	self subclassResponsibility!

name
	code isNil ifTrue: [ ^'' ].
	^self class codeMap at: code ifAbsent:[ code asString ]!

numericFunctionCode
	^self subclassResponsibility!

printOn: aStream 
	self isCharacter 
		ifFalse: [ isShift 
			ifTrue: [ aStream nextPutAll: '[Shift]' ] ].
	isControl 
		ifTrue: [ aStream nextPutAll: '[Control]' ].
	isAlt 
		ifTrue: [ aStream nextPutAll: '[Alt]' ].
	self isNumericFunction 
		ifTrue: 
			[ aStream nextPutAll: '[F'.
			self numericFunctionCode printOn: aStream.
			^aStream nextPut: $] ].
	code > 16rFF 
		ifTrue: 
			[ aStream nextPutAll: '['.
			aStream nextPutAll: self name.
			^aStream nextPut: $] ]. 
	
	self character == $[
		ifTrue: [ aStream nextPut: $[ ]. "escape"
	aStream nextPut: self character! !
	

!TerminalMacroAccessor publicMethods !

events
	^events!

events: anObject
	events := anObject!

scanLine: aString
	"Answer a TerminalKeyEvent instance representing aString"
	!

terminal
	^terminal!

terminal: anObject
	terminal := anObject! !
	

!TerminalMenu class publicMethods !

tagID
	^5! !

!TerminalMenu publicMethods !

items
	^items!

items: anObject
	items := anObject!

kindDo: aRequestor
	^aRequestor doTerminalMenu: self!

name
	^name!

name: anObject
	name := anObject! !
	

!WordWrapper class publicMethods !

on: aString columns: howMany hyphenations: hyphenationString tabSpacing: spacesForTab 
	^( self new ) 
		columns: howMany
		; hyphenationString: hyphenationString
		; tabSpacing: spacesForTab
		; computeLinesFrom: aString
		; lines! !

!WordWrapper publicMethods !

breakLines: aString from: start to: stop

	| index crIndex |
	aString isEmpty ifTrue: [ lines add: aString. ^self ].
	index := start + columns - 1.
	crIndex := aString indexOf: Character cr startingAt: start.
	(crIndex > 0 and:[crIndex < index])
		ifTrue:
			[ lines add:(aString copyFrom: start to: crIndex).
			^self breakLines: aString from: crIndex + 1 to: stop ].
	crIndex = 0 
		ifFalse:[index := index min: crIndex].	
	index >= stop
		ifTrue: [ ^lines add: (aString copyFrom: start to: stop) ].
	"find last hyphenchar"
	[index >= start]
		whileTrue:
			[ | char |
			char := aString at: index.
			 (self isHyphenationChar: char)
				ifTrue:
					[ lines add: (aString copyFrom: start to: index).
					^self breakLines: aString from: index + 1 to: stop ]
				ifFalse:
					[ index := index - 1]].
	"if not such char was found"
	index :=  start + columns - 1.
	lines add: (aString copyFrom: start to: index).
	self breakLines: aString from: index + 1 to: stop.!

columns: int
	columns := int!

computeLinesFrom: aString

	lines := OrderedCollection new.
	self breakLines: aString from: 1 to: aString size.	
	^self lines!

hyphenationString: aString
	hyphenationString := aString!

isHyphenationChar: char
	char == Character cr
		ifTrue: [ ^true ].
	char == Character lf
		ifTrue:[ ^true].
	^hyphenationString includes: char!

lines
	^lines!

tabSpacing: spacesForTab 
	tabSpacing := spacesForTab! !
	

!Rectangle publicMethods !

asBounds
	^Bounds origin: self origin corner: self corner!

asRectangle!

intersectOrLine: aRectangle
	^self intersect: aRectangle! !
	

!TObjectAppearance publicMethods !

asPixelValue: aColorOrNilOrInteger 
	aColorOrNilOrInteger isNil ifTrue: [^nil].
	aColorOrNilOrInteger isInteger ifTrue: [^aColorOrNilOrInteger].
	^TerminalGraphicalObject platformColorValueFor: aColorOrNilOrInteger! !

