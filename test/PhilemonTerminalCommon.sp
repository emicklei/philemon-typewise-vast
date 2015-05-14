"Philemon SmalltalkMT exporter has done all the work
Version PhilemonTerminalCommon EM 5-0507"
PROJECTNAME PhilemonTerminalCommon .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonColorSupport PhilemonEventTriggerSupport PhilemonObjectSerialization .
POOLS TerminalConstants .

PROFILE
BEGIN
END

CLASS TerminalWidgetBinaryAccessor
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	readObjectFrom:
	doIndexedMessageSend:
	writeObject:on:
	doTerminalWidgetAppearance:
	doTerminalDisplayTableItem:
	initialize
	doTerminalMenu:
	doTerminalCheckBox:
	doBounds:
	doTerminalCompositeWidget:
	doTerminalMenuSeparator:
	doTerminalDisplayItem:
	doTerminalGroupBox:
	doTerminalCounter:
	doTerminalBasicWidget:
	doTerminalDialog:
	doTerminalTableList:
	writeColor:
	doTerminalMenuList:
	doTerminalMenuBar:
	doTerminalLabel:
	writeTerminalClass:
	doTerminalText:
	isReading
	doEvaluationCollection:
	doTerminalInput:
	doTerminalTableColumn:
	doTerminalButton:
	readColor
	doTerminalMenuItem:
	doTerminalList:
	doTerminalDropDownList:
	doTerminalWindow:
	doTerminalObjectAppearance:
	doMessageSend:
	doTerminalRadioButton:
	doTerminalWidget:
	isReading:
	doTerminalImageLabel:
END

CLASS TWidgetAppearanceHolder
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	initialize
	widgetNamed:
	allClearDirty
	isDirty
	name:
	isDirty:
	clearDirty
	name
	widgetsDo:
	dispatchIndexedMessage:
	markDirty:
END

CLASS TTableColumn
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	labelClass:
	new
	tagID
END
INSTANCEMETHODS
BEGIN
	maxLength
	editable:
	editable
	kindDo:
	heading:
	maxLength:
	visible:
	alignment:
	appearance
	width:
	hasAppearance
	initializeHeader
	header:
	header
	phiWriteWith:
	clearDisplay:
	alignment
	initialize
	hasHeader
	width
	heading
	appearance:
	widgetUnderPoint:
	widgetUnderPoint
END

CLASS MessageSend
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	makeSelectorFrom:arguments:
	tagID
END
INSTANCEMETHODS
BEGIN
	asByteArray
	phiWriteWith:
	installForWidget:onEvent:
	sendUsingHandler:
	isIndexed
END

CLASS IndexedMessageSend
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	receiver:index:
	receiver:index:arguments:
	receiver:index:argument:
	tagID
END
INSTANCEMETHODS
BEGIN
	index
	isIndexed
	evaluate
	index:
	sendUsingHandler:
	kindDo:
END

CLASS Bounds
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	leftColumn
	bottomRow
	kindDo:
	topRow
	rows
	asRectangle
	rightColumn
	columns
	setTop:left:rows:columns:
	asBounds
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	with:dispatchIndexedMessage:
	dispatchIndexedMessage:
END

CLASS EvaluationCollection
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	asOrderedCollection
	phiWriteWith:
	asArray
	installForWidget:onEvent:
	printOn:
	asByteArray
	sendUsingHandler:
END

CLASS TWidgetEvent
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	losingFocus
	id:
	selectedItem
	gettingFocus
	F:
	fromID:
	initializeAfterLoad
	counterEnd
	selectionIsValid
	string
	clicked
	counterCount
	enter
	escape
	shiftF:
END
INSTANCEMETHODS
BEGIN
	printOn:
	isTriggeredWithArgument
	id
	id:
	asSymbol
END

CLASS TObjectAppearance
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	default
	umlDefinitionString
END
INSTANCEMETHODS
BEGIN
	disabledForeground:
	copyStateFrom:
	characterHeight
	selectionForeground
	selectionBackground:
	parentAppearance
	phiWriteWith:
	foreground:
	selectionBackground
	background
	isForCharacters
	foreground
	characterWidth
	printAttributesOn:
	selectionForeground:
	background:
	disabledForeground
	printOn:
END

CLASS TWidgetAppearance
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	umlDefinitionString
	tagID
	default
	release
	initializeDefault
	new
END
INSTANCEMETHODS
BEGIN
	focusColor:
	windowAppearance:
	copyStateFrom:
	windowAppearance
	parentAppearance
	kindDo:
	focusColor
	printAttributesOn:
	borderColor:
	parentAppearance:
	borderColor
	printOn:
END

CLASS TMenuSeparator
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	kindDo:
END

CLASS TMenuItem
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	enabled
	label
	event
	kindDo:
	subMenu:
	subMenu
	icon:
	label:
	help:
	help
	event:
	message:
	accellerator
	accellerator:
	enabled:
	message
	icon
END

CLASS TDisplayTableItem
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	tagID
END
INSTANCEMETHODS
BEGIN
	id
	doWithIndex:
	id:
	kindDo:
	items
	items:
	phiWriteWith:
END

CLASS TDisplayItem
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	id:string:
	tagID
END
INSTANCEMETHODS
BEGIN
	asString
	string:
	string
	printOn:
	kindDo:
	id:
	id
	phiWriteWith:
END

CLASS StyleSheet
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
END

CLASS Color
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
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
yourself).
!

BinaryObjectAccessor subclass: #TerminalWidgetBinaryAccessor
    instanceVariableNames: 'isReading '
    classVariableNames: 'ClientClassMap ServerClassMap TagToClassMap '
    poolDictionaries: ''
	comment: '' 
	category: '' !
EventModel subclass: #TWidgetAppearanceHolder
    instanceVariableNames: 'appearance dirty name '
    classVariableNames: ''
    poolDictionaries: 'TerminalConstants '
	comment: '' 
	category: '' !
TWidgetAppearanceHolder subclass: #TTableColumn
    instanceVariableNames: 'editable heading width maxLength header alignment '
    classVariableNames: 'LabelClass '
    poolDictionaries: ''
	comment: '' 
	category: '' !
MessageSend subclass: #IndexedMessageSend
    instanceVariableNames: 'index '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Rectangle subclass: #Bounds
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TWidgetEvent
    instanceVariableNames: 'id '
    classVariableNames: 'Events '
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TObjectAppearance
    instanceVariableNames: 'foreground background selectionForeground selectionBackground disabledForeground '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TObjectAppearance subclass: #TWidgetAppearance
    instanceVariableNames: 'windowAppearance focusColor borderColor '
    classVariableNames: 'Default '
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TMenuSeparator
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TMenuItem
    instanceVariableNames: 'label event enabled message subMenu icon accellerator help '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TDisplayTableItem
    instanceVariableNames: 'id items '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TDisplayItem
    instanceVariableNames: 'id string '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #StyleSheet
    instanceVariableNames: 'styles '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!TerminalWidgetBinaryAccessor publicMethods !

doBounds: aBounds

	self isReading
		ifTrue: 
			[ | topRow leftColumn rows columns | 
			topRow := self readNext.
			leftColumn := self readNext.
			rows := self readNext.
			columns := self readNext.
			aBounds setTop: topRow left: leftColumn rows: rows columns: columns]
		ifFalse: 
			[ self writeTerminalClass: aBounds class.
			aBounds topRow phiWriteWith: self.
			aBounds leftColumn phiWriteWith: self.
			aBounds rows phiWriteWith: self.
			aBounds columns phiWriteWith: self.]!

doEvaluationCollection: aEvaluationCollection 

	self isReading
		ifTrue: 
			[aEvaluationCollection collection: self readNext]
		ifFalse: 
			[ self writeTerminalClass: aEvaluationCollection class.
			"Convert the collection to an OrderedCollection ; not an Array"
			aEvaluationCollection asOrderedCollection phiWriteWith: self.]!

doIndexedMessageSend: anIndexedMessage

	self isReading
		ifTrue: 
			[anIndexedMessage index: self readNext.
			anIndexedMessage receiver: self readNext.
			"arguments must be array"
			anIndexedMessage arguments: (self readNext ifNil:[#()])] 
		ifFalse: 
			[ self writeTerminalClass: anIndexedMessage class.
			anIndexedMessage index phiWriteWith: self.
			anIndexedMessage receiver phiWriteWith: self.
			anIndexedMessage arguments phiWriteWith: self ]!

doMessageSend: aMessageSend 

	self isReading
		ifTrue: 
			[  | selectorString selectorSymbol |
			selectorString := self readNext.
			aMessageSend receiver: self readNext.
			"arguments must be array"
			aMessageSend arguments: (self readNext ifNil:[#()]).
			"server message selectors remain Strings"
			aMessageSend receiver ~= '_server'  "CONSTANT"
				ifTrue:
					[selectorSymbol := MessageSend
						makeSelectorFrom: selectorString
						arguments: aMessageSend arguments.
					aMessageSend selector: selectorSymbol]
				ifFalse:
					[aMessageSend selector: selectorString]]
		ifFalse: 
			[ self writeTerminalClass: aMessageSend class.
			aMessageSend selector phiWriteWith: self.
			aMessageSend receiver phiWriteWith: self.
			aMessageSend arguments phiWriteWith: self ]!

doTerminalBasicWidget: aBasicWidget

	self doTerminalWidget: aBasicWidget.
!

doTerminalButton: aButton 
	self doTerminalLabel: aButton.
	self isReading
		ifTrue: 
			[ aButton default: self readNext.
			aButton accelerator: self readNext.
			aButton help: self readNext]
		ifFalse: 
			[ aButton default phiWriteWith: self.
			aButton accelerator phiWriteWith: self.
			aButton help phiWriteWith: self]!

doTerminalCheckBox: aCheckbox 
	self doTerminalLabel: aCheckbox.
	self isReading
		ifTrue: [ aCheckbox tickAlignment: self readNext ]
		ifFalse: [ aCheckbox tickAlignment phiWriteWith: self ]	!

doTerminalCompositeWidget: aCompositeWidget 

	self doTerminalWidget: aCompositeWidget.
	self isReading
		ifTrue: [ self readNext do: [ :widg | aCompositeWidget add: widg ] ]
		ifFalse: [ aCompositeWidget widgets phiWriteWith: self ]!

doTerminalCounter: aCountDown 

	self doTerminalInput: aCountDown.
!

doTerminalDialog: aDialog
	self isReading
		ifTrue: 
			[ aDialog text: self readNext.
			aDialog type: self readNext.
			aDialog yes: self readNext.
			aDialog no: self readNext.
			aDialog cancel: self readNext.
			]
		ifFalse: 
			[ aDialog text phiWriteWith: self.
			aDialog type phiWriteWith: self.
			aDialog yes phiWriteWith: self.
			aDialog no phiWriteWith: self.
			aDialog cancel phiWriteWith: self.
			]!

doTerminalDisplayItem: aDisplayItem 

	self isReading
		ifTrue: 
			[ aDisplayItem id: self readNext.
			aDisplayItem string: self readNext ]
		ifFalse: 
			[ self writeTerminalClass: aDisplayItem class.
			aDisplayItem id phiWriteWith: self.
			aDisplayItem string phiWriteWith: self ]!

doTerminalDisplayTableItem: aDisplayItem 

	self isReading
		ifTrue: 
			[ aDisplayItem id: self readNext.
			aDisplayItem items: self readNext ]
		ifFalse: 
			[ self writeTerminalClass: aDisplayItem class.
			aDisplayItem id phiWriteWith: self.
			aDisplayItem items phiWriteWith: self ]!

doTerminalDropDownList: aDDList
	self doTerminalList: aDDList.
	self isReading
		ifTrue: [ aDDList maximumVisibleRows: self readNext ]
		ifFalse: [ aDDList maximumVisibleRows phiWriteWith: self ]		!

doTerminalGroupBox: aGroupBox

	self doTerminalCompositeWidget: aGroupBox.
	self isReading
		ifTrue: 
			[ aGroupBox label: self readNext ]
		ifFalse: 
			[ aGroupBox label phiWriteWith: self ]	!

doTerminalImageLabel: anImageLabel 

	self doTerminalBasicWidget: anImageLabel.
!

doTerminalInput: anInput 
	self doTerminalLabel: anInput.
	self isReading
		ifTrue: 
			[ anInput maxLength: self readNext.
			anInput type: self readNext ]
		ifFalse: 
			[ anInput maxLength phiWriteWith: self.
			anInput type phiWriteWith: self ]!

doTerminalLabel: aLabel 
	self doTerminalBasicWidget: aLabel.
!

doTerminalList: aList

	self doTerminalBasicWidget: aList!

doTerminalMenu: aMenu 
	self isReading
		ifTrue: 
			[ aMenu name: self readNext.
			aMenu items: self readNext ]
		ifFalse: 
			[ aMenu name phiWriteWith: self.
			aMenu items phiWriteWith: self ]!

doTerminalMenuBar: aMenuBar

	self doTerminalList: aMenuBar.
	self isReading
		ifTrue: 
			[aMenuBar characters: self readNext. 
			aMenuBar labels: self readNext. 
			aMenuBar menus: self readNext.
			aMenuBar useNative: self readNext]
		ifFalse: 
			[aMenuBar characters phiWriteWith: self.
			aMenuBar labels phiWriteWith: self.
			aMenuBar menus phiWriteWith: self.
			aMenuBar useNative phiWriteWith: self]	!

doTerminalMenuItem: anItem 

	self isReading
		ifTrue: 
			[anItem label: self readNext.
			anItem event: self readNext.
			anItem enabled: self readNext.
			anItem message: self readNext.
			anItem subMenu: self readNext.
			anItem icon: self readNext.
			anItem accellerator: self readNext.
			anItem help: self readNext ]
		ifFalse: 
			[ self writeTerminalClass: anItem class.
			self write: anItem label.
			self write: anItem event.
			self write: anItem enabled.
			self write: anItem message.
			self write: anItem subMenu.
			self write: anItem icon.
			self write: anItem accellerator.
			self write: anItem help ]!

doTerminalMenuList: aMenu 
	self doTerminalList: aMenu.
	self isReading
		ifTrue: [ aMenu items: self readNext ]
		ifFalse: [ aMenu items phiWriteWith: self ]!

doTerminalMenuSeparator: aSep

	self isReading
		ifFalse: 
			[ self writeTerminalClass: aSep class]!

doTerminalObjectAppearance: anAppearance 

	self isReading
		ifTrue: 
			[ anAppearance 
					foreground: self readNext 
					;background: self readNext 
					;selectionForeground: self readNext 
					;selectionBackground: self readNext
					;disabledForeground: self readNext ]
		ifFalse: 
			[ anAppearance = anAppearance class default 
				ifTrue: [ self writeNil. ^nil ].
			self writeTerminalClass: anAppearance class.
			anAppearance foreground phiWriteWith: self.
			anAppearance background phiWriteWith: self.
			anAppearance selectionForeground phiWriteWith: self.
			anAppearance selectionBackground phiWriteWith: self.
			anAppearance disabledForeground phiWriteWith: self ]!

doTerminalRadioButton: aButton 
	self doTerminalCheckBox: aButton.
	self isReading
		ifTrue: 
			[ aButton groupName: self readNext. ]
		ifFalse: 
			[ aButton groupName phiWriteWith: self. ]	!

doTerminalTableColumn: aTableColumn 
	self isReading
		ifTrue: 
			[ aTableColumn width: self readNext.
			aTableColumn maxLength: self readNext.
			aTableColumn header: self readNext.
			aTableColumn alignment: self readNext.
			aTableColumn appearance: self readNext. ]
		ifFalse: 
			[ self writeTerminalClass: aTableColumn class.
			aTableColumn width phiWriteWith: self.
			aTableColumn maxLength phiWriteWith: self.
			aTableColumn header phiWriteWith: self.
			aTableColumn alignment phiWriteWith: self.
			aTableColumn appearance phiWriteWith: self ]!

doTerminalTableList: aTableList 

	self doTerminalList: aTableList.
	self isReading
		ifTrue: [ self readNext do: [ :each | aTableList addColumn: each ] ]
		ifFalse: [ aTableList tableColumns phiWriteWith: self ]!

doTerminalText: anText 
	self doTerminalInput: anText!

doTerminalWidget: aWidget 

	self isReading
		ifTrue: 
			[ aWidget name: self readNext.
			aWidget bounds: self readNext.
			aWidget appearance: self readNext]
		ifFalse: 
			[ self writeTerminalClass: aWidget class.
			aWidget name phiWriteWith: self.
			aWidget bounds phiWriteWith: self.
			aWidget appearanceOrNil phiWriteWith: self ]!

doTerminalWidgetAppearance: anAppearance 

	| windowAppearance |
	self isReading
		ifTrue: 
			[self doTerminalObjectAppearance: anAppearance.
			anAppearance borderColor: self readNext.
			anAppearance focusColor: self readNext ]
		ifFalse: 
			[(self doTerminalObjectAppearance: anAppearance) isNil
				ifTrue: [ ^self ].
			anAppearance borderColor phiWriteWith: self.
			anAppearance focusColor phiWriteWith: self ]!

doTerminalWindow: aWindow 
	self doTerminalCompositeWidget: aWindow.
	self isReading
		ifTrue: 
			[aWindow menuBar: self readNext ]
		ifFalse: 
			[aWindow hasMenuBar
				ifTrue: [ aWindow menuBar phiWriteWith: self ]
				ifFalse: [ self writeNil ] ]!

initialize
	super initialize.
	version := 13. "10-11-2004"!

isReading
	^isReading!

isReading: anObject
	isReading := anObject!

readColor
	^Color redByte: stream next greenByte: stream next blueByte: stream next!

readObjectFrom: aStream

	self isReading: true.
	^super readObjectFrom: aStream!

writeColor: aColor 

	stream nextPut: TColor.
	stream nextPut: aColor redByte.
	stream nextPut: aColor greenByte.
	stream nextPut: aColor blueByte!

writeObject: anObject on: aStream
	self isReading: false.
	super writeObject: anObject on: aStream!

writeTerminalClass: aClass
	stream nextPut: 100 ;nextPut: aClass tagID! !
	

!TWidgetAppearanceHolder publicMethods !

allClearDirty
	self widgetsDo:[ :w | w clearDirty ]!

clearDirty
	dirty := 0!

dispatchIndexedMessage: anIndexedMessage 
	TMAppearanceSet = anIndexedMessage index 
		ifTrue: [ ^self appearance: anIndexedMessage argument ].
	TMAppearanceGet = anIndexedMessage index 
		ifTrue: [ ^self appearance  ].
	super dispatchIndexedMessage: anIndexedMessage !

initialize
		dirty := 0.!

isDirty
	^dirty > 0!

isDirty: attributeCode

	^dirty > 0 and:[ (dirty bitAnd: attributeCode) = attributeCode ]!

markDirty: attributeCode

	dirty := dirty bitOr: attributeCode!

name
	^name ifNil:[ name := 'tw' , self hash printString ].!

name: anObject
	name := anObject!

widgetNamed: aString
	^self name = aString
		ifTrue: [ self ]
		ifFalse:[ nil ]!

widgetsDo: oneArgBlock
	oneArgBlock value: self.! !
	

!TTableColumn class publicMethods !

labelClass: newLabelClass
	LabelClass := newLabelClass!

new
	^super new initialize!

tagID
	^18! !

!TTableColumn publicMethods !

alignment
	^alignment!

alignment: anObject
	alignment := anObject!

appearance
	"If my appearance is not set then get a copy preferrable"

	appearance isNil ifTrue: [appearance := TWidgetAppearance default copy].
	^appearance!

appearance: anObject
	appearance := anObject!

clearDisplay: aBoolean 
	"Enter the new method definition below and click 'Resume'."
!

editable
	^editable!

editable: anObject
	editable := anObject.
	self markDirty: TMEditableSet!

hasAppearance
	^appearance notNil!

hasHeader
	^header notNil!

header
	self initializeHeader.
	^header!

header: aLabel 
	header := aLabel!

heading
	^self header string!

heading: aString
	^self header string: aString.!

initialize
	super initialize.
	self editable: false.
	self width: 4.
	self alignment: 1 "LEFT"!

initializeHeader
	header isNil 
		ifTrue: 
			[ header := LabelClass in: ( 1 @ 1 extent: 0 @ 0 ).
			header alignment: self alignment ]!

kindDo: aRequestor 

	^aRequestor doTerminalTableColumn: self!

maxLength
	^maxLength!

maxLength: anObject
	maxLength := anObject!

phiWriteWith: aBinaryObjectAccessor
	"Make sure the receiver uses the special binary accessor"
	self kindDo: aBinaryObjectAccessor!

visible: aBoolean 
	"Enter the new method definition below and click 'Resume'."!

widgetUnderPoint
	" TableList should handle this "
	^nil!

widgetUnderPoint: aPoint
	^nil!

width
	^width!

width: anObject
	width := anObject! !
	

!MessageSend class publicMethods !

makeSelectorFrom: operation arguments: anArray

	| nameStream |
	anArray isEmpty
		ifTrue: [ ^operation asSymbol ].
	anArray size = 1
		ifTrue: [ ^operation last = $:
			ifTrue:[operation asSymbol]
			ifFalse:[(operation copyWith: $:) asSymbol ]].
	(operation occurrencesOf: $:) = anArray size
		ifTrue:[^operation asSymbol].
	nameStream := WriteStream on: String new.
	nameStream nextPutAll: operation ;nextPut: $:.
	anArray size - 1 timesRepeat:[ nameStream nextPutAll: 'with:'].
	^nameStream contents asSymbol!

tagID
	^9! !

!MessageSend publicMethods !

asByteArray
	^TerminalWidgetBinaryAccessor byteArrayFromObject: self!

installForWidget: aTWidget onEvent: eventID
	
	^aTWidget installMessageSend: self onEvent: eventID
	!

isIndexed
	^false!

phiWriteWith: aBOA
	self kindDo: aBOA!

sendUsingHandler: aMessageHandler 

	| target stSelector |
	target := self receiver isString
		ifTrue: [ aMessageHandler receiverAt: self receiver ]
		ifFalse: [ self receiver ].
	target isNil ifTrue: [ ^'[Terminal] could not find widget named' echo: self receiver].
	stSelector := self class makeSelectorFrom: self selector arguments: self arguments.
	^target perform: stSelector withArguments: self arguments! !
	

!IndexedMessageSend class publicMethods !

receiver: anObject index: index
	^self new 
		receiver: anObject
		; index: index!

receiver: anObject index: index argument: anArgument
	^self new 
		receiver: anObject
		; index: index 
		; arguments: (Array with: anArgument)!

receiver: anObject index: index arguments: anArray 
	^self new  receiver: anObject ; index: index ; arguments: anArray!

tagID
	^4! !

!IndexedMessageSend publicMethods !

evaluate
	"Dispatch the message using the receiver of the message"
	self receiver dispatchIndexedMessage: self!

index
	^index!

index: anObject
	index := anObject!

isIndexed
	^true!

kindDo: aRequestor
	^aRequestor doIndexedMessageSend: self!

sendUsingHandler: aMessageHandler 

	| target stSelector |
	target := self receiver isString
		ifTrue: [ aMessageHandler receiverAt: self receiver ]
		ifFalse: [ self receiver ].
	target isNil ifTrue: [ ^'[Terminal] could not find widget named' echo: self receiver].
	^target dispatchIndexedMessage: self! !
	

!Bounds class publicMethods !

tagID
	^26! !

!Bounds publicMethods !

asBounds
	^self!

asRectangle
	^Rectangle origin: self origin corner: self corner!

bottomRow
	^self right!

columns
	^self height + 1!

kindDo: aRequestor
	^aRequestor doBounds: self!

leftColumn
	^self origin y!

rightColumn
	^self bottom!

rows
	^self width + 1!

setTop: newTop left: newLeft rows: newRows columns: newColumns
	self origin: newTop @ newLeft.
	self corner: ( newTop + newRows - 1) @ ( newLeft + newColumns - 1).!

topRow
	^self origin x! !
	

!Object publicMethods !

dispatchIndexedMessage: anIndexedMessage
	anIndexedMessage index = 27 "TMYourself" ifTrue: [ ^self ].
	self error: self printString, ' does not understand indexed message:' , anIndexedMessage asString!

with: argument dispatchIndexedMessage: anIndexedMessage
	"Insert the argument into the message and dispatch it,
	unless the message is unary"

	anIndexedMessage arguments isEmpty
		ifFalse:[anIndexedMessage arguments at:1 put: argument].
	^self dispatchIndexedMessage: anIndexedMessage! !
	

!EvaluationCollection class publicMethods !

tagID
	^10! !

!EvaluationCollection publicMethods !

asArray
	^collection asArray!

asByteArray
	^TerminalWidgetBinaryAccessor byteArrayFromObject: self!

asOrderedCollection
	^collection asOrderedCollection!

installForWidget: aTWidget onEvent: eventID
	
	collection do:[ :each | each installForWidget: aTWidget onEvent: eventID ].
	^self!

phiWriteWith: aBOAccessor
	^self kindDo: aBOAccessor
	!

printOn: aStream
	collection doWithIndex:[ :each :i |
		aStream cr tab.
		i printOn: aStream.
		aStream nextPut: $: .
		each printOn: aStream ]!

sendUsingHandler: aMessageHandler
	self do:[ :each | each sendUsingHandler: aMessageHandler ]! !
	

!TWidgetEvent class publicMethods !

clicked
	^Events at: 4 ifAbsentPutUsing:[self id: 4]!

counterCount
	^Events at: 8 ifAbsentPutUsing:[self id: 8]!

counterEnd
	^Events at: 6 ifAbsentPutUsing:[self id: 6]!

enter
	^Events at: 11 ifAbsentPutUsing:[self id: 11]!

escape
	^Events at: 10 ifAbsentPutUsing:[self id: 10]!

F: oneToTwelve
	| id |
	id := 42 + (oneToTwelve*2).
	^Events at: id ifAbsentPutUsing:[self id: id]!

fromID: key
	^Events at: key!

gettingFocus
	^Events at: 2 ifAbsentPutUsing:[self id: 2]!

id: anInteger
	^self new id: anInteger!

initializeAfterLoad
	Events := IdentityDictionary new.
	self losingFocus.  "0"
	self selectedItem.  "1"
	self gettingFocus.  "2"
	self selectionIsValid.  "3"
	self clicked.  "4" 	
	self counterEnd. "6"
	self counterCount. "8"
	self escape. "10"
	"F1..F12"
	1 to: 12 do: [ :f | self F: f ].
	"Shift-F1..Shift-F12"
	1 to: 12 do: [ :f | self shiftF: f ]. 	
	!

losingFocus
	^Events at: 0 ifAbsentPutUsing:[self id: 0]!

selectedItem
	^Events at: 1 ifAbsentPutUsing:[self id: 1]!

selectionIsValid
	^Events at: 3 ifAbsentPutUsing:[self id: 3]!

shiftF: oneToTwelve
	| id |
	id := 66 + (oneToTwelve*2).
	^Events at: id ifAbsentPutUsing:[self id: id]!

string
	^Events at: 12 ifAbsentPutUsing:[self id: 12]! !

!TWidgetEvent publicMethods !

asSymbol
	"self"!

id
	^id!

id: anObject
	id := anObject!

isTriggeredWithArgument
	^id \\ 2 = 1!

printOn: aStream
	super printOn: aStream.
	aStream nextPut: $:.
	id printOn: aStream! !
	

!TObjectAppearance class publicMethods !

default
	self subclassResponsibility!

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalObjectAppearance">
	<attribute name="flags" private="true" default="0" type="Integer" />
	<attribute name="background"/>
	<attribute name="foreground"/>
	<attribute name="selectionBackground"/>
	<attribute name="selectionForeground"/>
	<operation name="copyStateFrom:">
		<parameter type="TerminalObjectAppearance"/>
	</operation>
	<operation name="isForCharacters" return="Boolean"/>
	<operation name="parentAppearance" return="TerminalObjectAppearance"/>
</class>'! !

!TObjectAppearance publicMethods !

background
	^background isNil
		ifTrue:[self parentAppearance isNil
			ifTrue:[nil]
			ifFalse:[self parentAppearance background]]
		ifFalse:[background]!

background: anObject
	background := self asPixelValue: anObject!

characterHeight
	^self currentFontMetrics height!

characterWidth
	^self currentFontMetrics width!

copyStateFrom: anAppearance 

	foreground notNil 
		ifTrue: [ self foreground: anAppearance foreground ].
	background notNil 
		ifTrue: [ self background: anAppearance background ].
	selectionForeground notNil 
		ifTrue: [ self selectionForeground: anAppearance selectionForeground ].
	selectionBackground notNil 
		ifTrue: [ self selectionBackground: anAppearance selectionBackground ]!

disabledForeground
	^disabledForeground!

disabledForeground: anObject
	disabledForeground := self asPixelValue: anObject!

foreground
	^foreground isNil
		ifTrue:[self parentAppearance isNil
			ifTrue:[nil]
			ifFalse:[self parentAppearance foreground]]
		ifFalse:[foreground]!

foreground: anObject
	foreground := self asPixelValue: anObject!

isForCharacters
	^false!

parentAppearance
	self subclassResponsibility!

phiWriteWith: aBWA
	self kindDo: aBWA!

printAttributesOn: aStream
	aStream
		nextPutAll: 'fore='
		;print: foreground
		;nextPutAll: ',back='
		;print: background
		;nextPutAll: ',selfore='
		;print: selectionForeground		
		;nextPutAll: ',selback='
		;print: selectionBackground				
		;nextPutAll: ',dis='
		;print: disabledForeground				!

printOn: aStream
	aStream 
		nextPutAll: 'appearance@'
		;print: self hash
		;nextPut: ${.
	self printAttributesOn: aStream.
	aStream nextPut: $}!

selectionBackground
	^selectionBackground isNil
		ifTrue:[self parentAppearance isNil
			ifTrue:[nil]
			ifFalse:[self parentAppearance selectionBackground]]
		ifFalse:[selectionBackground]!

selectionBackground: anObject
	selectionBackground := self asPixelValue: anObject!

selectionForeground
	^selectionForeground isNil
		ifTrue:[self parentAppearance isNil
			ifTrue:[nil]
			ifFalse:[self parentAppearance selectionForeground]]
		ifFalse:[selectionForeground]!

selectionForeground: anObject
	selectionForeground := self asPixelValue: anObject! !
	

!TWidgetAppearance class publicMethods !

default
	Default isNil ifTrue: [self initializeDefault].
	^Default!

initializeDefault
	Default := super new 
		foreground: Color white
		; background: Color black
		; selectionForeground: Color black
		; selectionBackground: Color grey
		; disabledForeground: Color grey
		; borderColor: nil
		; focusColor: Color grey.!

new
	^super new parentAppearance: self default!

release
	Default := nil!

tagID
	^12!

umlDefinitionString
"self exportUML"
^'<?xml version="1.0"?>
<class name="TerminalWidgetAppearance" superclass="TerminalObjectAppearance">
	<attribute name="border" type="Color"/>
	<attribute name="windowAppearance" type="TerminalObjectAppearance"/>
	<operation name="characterAppearance" return="TerminalCharacterAppearance"/>
	<operation name="characterHeight" return="Integer"/>
	<operation name="characterWidth" return="Integer"/>
	<operation name="font" return="Font"/>
	<operation name="parentAppearance" return="TerminalObjectAppearance"/>
</class>'! !

!TWidgetAppearance publicMethods !

borderColor
	^borderColor isNil 
		ifTrue: 
			[self parentAppearance isNil 
				ifTrue: [nil]
				ifFalse: [self parentAppearance borderColor]]
		ifFalse: [borderColor]!

borderColor: anObject 
	borderColor := self asPixelValue: anObject!

copyStateFrom: anAppearance 
	super copyStateFrom: anAppearance.
	borderColor notNil ifTrue: [self border: anAppearance border]!

focusColor
	^focusColor isNil 
		ifTrue: 
			[self parentAppearance isNil 
				ifTrue: [nil]
				ifFalse: [self parentAppearance focusColor]]
		ifFalse: [focusColor]!

focusColor: anObject 
	focusColor := self asPixelValue: anObject!

kindDo: aRequestor
	^aRequestor doTerminalWidgetAppearance: self!

parentAppearance
	^windowAppearance!

parentAppearance: anAppearance
	windowAppearance := anAppearance!

printAttributesOn: aStream
	super printAttributesOn: aStream.
	aStream
		nextPutAll: ',bor='
		;print: borderColor
		;nextPutAll: ',foc='
		;print: focusColor!

printOn: aStream

	super printOn: aStream.
	self = Default
		ifTrue: [aStream nextPutAll: '(default)']!

windowAppearance
	^windowAppearance!

windowAppearance: anAppearance

	windowAppearance := anAppearance! !
	

!TMenuSeparator class publicMethods !

tagID
	^28! !

!TMenuSeparator publicMethods !

kindDo: aRequestor
	^aRequestor doTerminalMenuSeparator: self! !
	

!TMenuItem class publicMethods !

tagID
	^27! !

!TMenuItem publicMethods !

accellerator
	^accellerator!

accellerator: anObject
	accellerator := anObject!

enabled
	^enabled!

enabled: anObject
	enabled := anObject!

event
	^event!

event: anObject
	event := anObject!

help
	help isNil ifTrue: [ ^'' ].
	^help!

help: anObject
	help := anObject!

icon
	^icon!

icon: anObject
	icon := anObject!

kindDo: aRequestor
	^aRequestor doTerminalMenuItem: self!

label
	^label!

label: anObject
	label := anObject!

message
	^message!

message: anObject
	message := anObject!

subMenu
	^subMenu!

subMenu: anObject
	subMenu := anObject! !
	

!TDisplayTableItem class publicMethods !

tagID
	^23! !

!TDisplayTableItem publicMethods !

doWithIndex: twoArgBlock
	self items doWithIndex: twoArgBlock!

id
	^id!

id: anID
	id := anID!

items
	^items!

items: aCollection
	items := aCollection!

kindDo: aRequestor
	^aRequestor doTerminalDisplayTableItem: self!

phiWriteWith: aWriter
	^self kindDo: aWriter! !
	

!TDisplayItem class publicMethods !

id: anObject string: aPrintable
	^self new id: anObject ;string: aPrintable!

tagID
	^22! !

!TDisplayItem publicMethods !

asString
	^self string!

id
	^id!

id: anObject
	id := anObject!

kindDo: aRequestor
	^aRequestor doTerminalDisplayItem: self!

phiWriteWith: aWriter
	self kindDo: aWriter!

printOn: aStream 

	"Printing shows id, asString returns string"

	self id isString
		ifTrue: [ aStream nextPutAll: self id ]
		ifFalse: [ self id printOn: aStream ]!

string
	^string!

string: anObject
	string := anObject! !
	
	

!Color publicMethods !

phiWriteWith: aWriter

	aWriter writeColor: self! !

