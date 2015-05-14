"Philemon SmalltalkMT exporter has done all the work
Version PhilemonTerminalClient EM 5-1024"
PROJECTNAME PhilemonTerminalClient .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonTerminalView .
POOLS TerminalConstants .

PROFILE
BEGIN
END

CLASS TerminalClient
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	openURL:
	new
	model:
END
INSTANCEMETHODS
BEGIN
	terminal
	with:request:
	initialize
	postMessage:
	aboutToClose
	open
	show:
	addCentered:
	indexedRequest:withArguments:
	openHelp
	dispatchIndexedMessage:
	showDialog:
	handleByteArray:
	terminal:
	openURL:
	showErrorFor:
	model:
	safelySendMessage:
	with:indexedRequest:
	primPost:
	request:with:
	showErrorString:
	request:withArguments:
	remove:
	add:
	request:
	close
	receiverAt:
	application
	contextURL
END

CLASS TerminalWidget
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	asByteArray
	phiWriteWith:
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalCompositeWidget
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalWindow
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalGroupBox
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalBasicWidget
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalImageLabel
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalLabel
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalInput
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalButton
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
END

CLASS TTableColumn
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalWidgetBinaryAccessor
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	setupClientAccessors
	setupCommonAccessors
	registerClassName:for:
END
INSTANCEMETHODS
BEGIN
	readTerminalObject
END

CLASS TerminalList
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalTableList
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
END

CLASS TerminalMenuList
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalCheckBox
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

CLASS TerminalRadioButton
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	addStateMessagesTo:
	dispatchIndexedMessage:
END

! !
Compiler poolAdd: 'TerminalConstants' value: (StringDictionary new 
 at: 'TELosingFocus' put: 2;
 at: 'TMURLGet' put: 16384;
 at: 'TMItemsSet' put: 65;
 at: 'TEClicked' put: 4;
 at: 'TMTickAlignmentGet' put: 2097152;
 at: 'TMTitleGet' put: 65536;
 at: 'TMDelaySet' put: 513;
 at: 'TMEditableGet' put: 8192;
 at: 'TMMessageForAddingCenteredWidgetAccessedBy' put: 72;
 at: 'TESelectionIsValid' put: 3;
 at: 'TMBoundsGet' put: 8;
 at: 'TMRequest' put: 79;
 at: 'CR' put: $;
 at: 'TMOpenHelp' put: 77;
 at: 'TMRepeatSet' put: 1025;
 at: 'TMURLSet' put: 16385;
 at: 'TMSelectedItemGet' put: 128;
 at: 'TMStringSet' put: 2049;
 at: 'TMHandleError' put: 73;
 at: 'TMSubmit' put: 66;
 at: 'TMClearAllDirty' put: 80;
 at: 'TMSelectedItemSet' put: 129;
 at: 'TMTitleSet' put: 65537;
 at: 'TMEnabledSet' put: 17;
 at: 'TMItemSet' put: 262145;
 at: 'TMSelectionGet' put: 32;
 at: 'TEGettingFocus' put: 0;
 at: 'TMSetFocus' put: 82;
 at: 'TMRemove' put: 70;
 at: 'TMClose' put: 74;
 at: 'TMMessageForAddingWidgetAccessedBy' put: 71;
 at: 'TMInstallMessageOnEvent' put: 75;
 at: 'TMAppearanceGet' put: 2;
 at: 'TECounterEnd' put: 6;
 at: 'TMMessageForShowingScreenAccessedBy' put: 67;
 at: 'TESelectedItem' put: 1;
 at: 'TMAlignmentGet' put: 4096;
 at: 'TMItemsGet' put: 64;
 at: 'TMSelectionIndexSet' put: 1048577;
 at: 'TMStringGet' put: 2048;
 at: 'TMCountSet' put: 257;
 at: 'TECounted' put: 8;
 at: 'TMVisibleGet' put: 4;
 at: 'LF' put: $
;
 at: 'TMCountGet' put: 256;
 at: 'SPACE' put: $ ;
 at: 'TMItemGet' put: 262144;
 at: 'TMSelectionIndexGet' put: 1048576;
 at: 'TMIntervalSet' put: 524289;
 at: 'TMTickAlignmentSet' put: 2097153;
 at: 'TMVisibleSet' put: 5;
 at: 'TMEditableSet' put: 8193;
 at: 'TMEnabledGet' put: 16;
 at: 'TMBoundsSet' put: 9;
 at: 'TMAppearanceSet' put: 3;
 at: 'TMDelayGet' put: 512;
 at: 'TMOpenURL' put: 78;
 at: 'TMShowDialog' put: 81;
 at: 'TMRepeatGet' put: 1024;
 at: 'TMSelectionSet' put: 33;
 at: 'TMScaleToFitSet' put: 32769;
 at: 'TMScaleToFitGet' put: 32768;
 at: 'TMAlignmentSet' put: 4097;
 at: 'TMShow' put: 69;
 at: 'TAB' put: $	;
 at: 'TMMessageForShowingScreenAccessedByWith' put: 68;
 at: 'TMYourself' put: 76;
yourself).
!

Object subclass: #TerminalClient
    instanceVariableNames: 'terminal application '
    classVariableNames: ''
    poolDictionaries: 'TerminalConstants '
	comment: '' 
	category: '' !	

!TerminalClient class publicMethods !

model: anApplicationModel
	| client |
	client := self new.
	client model: anApplicationModel.
	anApplicationModel client: client.
	^client!

new
	^super new initialize!

openURL: urlString
	| url  serviceUrl  map model client methodName|
	url := urlString sstAsUrl.
	url isNil ifTrue: [ " Error cracking URL !! " ^nil ].
	serviceUrl := url transport , '://' , url address , (url path copyFrom: 1 to: (url path lastIndexOf: $/) - 1).
	methodName := url path copyFrom: (url path lastIndexOf: $/) + 1 to: url path size.
	map := Dictionary new.
	model := (TerminalClientHttpService at: serviceUrl).
	client := self new model: model.
	( client request: methodName withArguments: (Array with: map)) isNil 
		ifTrue: [ ^nil ].
	client open.
	^client! !

!TerminalClient publicMethods !

aboutToClose
	[ self primPost: (IndexedMessageSend receiver: '_server' index: TMClose) ]
		whenErrorDo:[ :sig | sig exitWith: nil ].
	self application shutDown!

add: aWidget
	"Add the widget to the current screen"
	terminal screen add: aWidget.
	aWidget setInputFocus!

addCentered: aWidget
	"Add the widget to the current screen at the center"
	
	| screenWidth  screenHeight offset|
	screenWidth := terminal screen columns.
	screenHeight := terminal screen rows.
	offset := ((screenHeight - aWidget rows)
		@(screenWidth - aWidget columns)) // 2.
	aWidget translateBy: offset.
	terminal screen add: aWidget.
	aWidget setInputFocus!

application
	^application!

close
	"This can be sent from the server.
	The terminal has a reference to the client and will callback the aboutToClose"
	
	terminal close.!

contextURL
	"Return the absolute URL of the context of the service"
	^self application contextURL!

dispatchIndexedMessage: anIndexedMessage 

	TMClose = anIndexedMessage index
		ifTrue: [ ^self close ].
	TMShow = anIndexedMessage index
		ifTrue: [ ^self show: anIndexedMessage argument ].	
	TMShowDialog = anIndexedMessage index
		ifTrue: [ ^self showDialog: anIndexedMessage argument ].		
	TMRemove = anIndexedMessage index
		ifTrue: [ ^self remove: anIndexedMessage argument ].		
	TMHandleError = anIndexedMessage index
		ifTrue: [ ^self showErrorString: anIndexedMessage argument ].		
	TMOpenHelp = anIndexedMessage index
		ifTrue: [ ^self openHelp].		
	TMOpenURL = anIndexedMessage index
		ifTrue: [ ^self openURL: anIndexedMessage argument].	
	TMClearAllDirty = anIndexedMessage index
		ifTrue: [ ^self terminal screen allClearDirty ].	

	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage 	!

handleByteArray: bin 

	bin isEmpty 
		ifTrue: [ ^nil ].
	^TerminalWidgetBinaryAccessor objectFromByteArray: bin!

indexedRequest: index withArguments: anArray
	"Send the operation to the application"

	^self postMessage:(IndexedMessageSend receiver: '_server' index: index arguments: anArray)!

initialize
	terminal := Terminal new messageHandler: self.!

model: anObject 
	application := anObject.
	anObject isNil ifFalse: [anObject startUp]!

open
	terminal open.!

openHelp
	self openURL: 'help.html'!

openURL: aString
	"Should check for protocol, if protocol is twtp then start new client"
	
	self startProgramWith: aString!

postMessage: aMessage 

	| sequence |
	sequence := OrderedCollection new.
	self terminal notify: 'Waiting...'.
	terminal screen notNil 
		ifTrue: 
			[ terminal screen addStateMessagesTo: sequence. 	
			"server must clear all dirty after handling statemessages (if any)"
			sequence isEmpty 
				ifFalse: [ sequence add: ( IndexedMessageSend receiver: '_server' index: TMClearAllDirty ) ] ].
	sequence add: aMessage.
	^self primPost: ( EvaluationCollection withAll: sequence ) asMinimalRepresentation!

primPost: aMessage

	| reply  message|
	'POST' echo: aMessage.
	reply := self application postMessage: aMessage.
	self terminal notify: self application connectionState.
	reply isNil
		ifTrue: [ 'NOREPLY' echo: aMessage. ^nil ].
	message := self handleByteArray: reply.
	'REPLY' echo: message.
	message isNil
		ifTrue: [ ^nil ].
	self safelySendMessage: message!

receiverAt: key
	'_client' = key ifTrue: [ ^self ].
	^terminal screen receiverAt: key
	!

remove: panelName
	| panel |
	panel := terminal screen widgetNamed: panelName.
	panel isNil
		ifTrue: [ ^self showErrorString: '[Client] unable to remove:' , panelName asString].
	terminal screen remove: panel!

request: operationName 
	^self request: operationName withArguments: #()!

request: operationName with: argument
	"Send the operation to the service on the server"

	^self request: operationName withArguments: (Array with: argument)!

request: operationName withArguments: anArray

	^self postMessage: (MessageSend receiver: '_server' selector: operationName arguments: anArray)!

safelySendMessage: aTMessage 

	[ aTMessage sendUsingHandler: self ] 
		when: ExError 
		do: [ :sig | 
			self showErrorFor: sig.
			sig exitWith: nil ]!

show: aWidget
	terminal show: aWidget!

showDialog: aDialog
	aDialog open.!

showErrorFor: aSignal 	
	self showErrorString: aSignal arguments asString!

showErrorString: aString 
	| win txt winSize close |
	self isRuntime ifFalse: [1 halt].
	winSize := terminal screen isNil 
				ifTrue: [20 @ 40]
				ifFalse: [terminal screen bounds extent].
	win := TerminalWidget windowIn: (1 @ 1 extent: winSize).
	win background: Color blue.
	win foreground: Color yellow.
	txt := TerminalWidget textIn: (1 @ 1 extent: (winSize x - 1) @ winSize y).
	txt editable: false.
	txt string: 'Error:' , aString.
	close := TerminalWidget 
				buttonIn: ((winSize x + 1) @ 2 extent: 0 @ (winSize y - 2)).
	close label: 'Runtime Error Occurred: Press Enter To Exit'.
	close 
		when: TWidgetEvent clicked
		send: #close
		to: self.
	win add: close.
	win add: txt.
	self show: win!

terminal
	^terminal!

terminal: aTerminal
	terminal := aTerminal!

with: anArgument indexedRequest: operationName 

	"Pre: the operationName is expecting an argument.
	This method exists because of the way EventTriggerSupport handles arguments 
	passed when triggering an event"

	^self indexedRequest: operationName withArguments: ( Array with: anArgument )!

with: anArgument request: operationName
	"Pre: the operationName is expecting an argument.
	This method exists because of the way EventTriggerSupport handles arguments 
	passed when triggering an event"
	
	^self request: operationName with: anArgument! !
	

!TerminalWidget publicMethods !

addStateMessagesTo: aCollection 
	"Subclasses should redefine this"
	
	(self isDirty: TMBoundsSet)
		ifTrue: [ aCollection add: (IndexedMessageSend
			receiver: self name
			index: TMBoundsSet 
			argument: self bounds) ].
	(self isDirty: TMVisibleSet)
		ifTrue: [ aCollection add: (IndexedMessageSend
			receiver: self name
			index: TMVisibleSet 
			argument: self visible) ].		
	(self isDirty: TMAppearanceSet)
		ifTrue: [ aCollection add: (IndexedMessageSend
			receiver: self name
			index: TMAppearanceSet 
			argument: self appearanceOrNil) ].				
	!

asByteArray

	^TerminalWidgetBinaryAccessor byteArrayFromObject: self!

dispatchIndexedMessage: anIndexedMessage 
	TMBoundsSet = anIndexedMessage index 
		ifTrue: [ ^self bounds: anIndexedMessage argument ].
	TMVisibleSet = anIndexedMessage index 
		ifTrue: [ ^self visible: anIndexedMessage argument ].
	TMVisibleGet = anIndexedMessage index 
		ifTrue: [ ^self visible ].
	TMInstallMessageOnEvent = anIndexedMessage index 
		ifTrue: [ ^self installMessage: anIndexedMessage arguments first onEvent: anIndexedMessage arguments last ]. 	
	TMSetFocus = anIndexedMessage index
		ifTrue: [ ^self haveFocus ]. 
	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage!

phiWriteWith: aBinaryObjectAccessor
	"Make sure the receiver uses the special binary accessor"
	self kindDo: aBinaryObjectAccessor
	! !
	

!TerminalCompositeWidget publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	self widgets do: [ :each | each addStateMessagesTo: aCollection ]!

dispatchIndexedMessage: anIndexedMessage 
	TMSubmit = anIndexedMessage index
		ifTrue: [ ^self submitInputNamesAndValuesTo: anIndexedMessage argument ].
	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage		! !
	

!TerminalWindow publicMethods !

addStateMessagesTo: aCollection 
	"Subclasses should redefine this"

	super addStateMessagesTo: aCollection.	
	(self isDirty: TMTitleSet)
		ifTrue: [ aCollection add: (IndexedMessageSend
			receiver: self name
			index: TMTitleSet 
			argument: self title) ].!

dispatchIndexedMessage: anIndexedMessage 

	TMTitleSet = anIndexedMessage index
		ifTrue: [ ^self title: anIndexedMessage argument ].
	TMTitleGet = anIndexedMessage index
		ifTrue: [ ^self title ].
	^super dispatchIndexedMessage: anIndexedMessage.! !
	

!TerminalGroupBox publicMethods !

addStateMessagesTo: aCollection
		super addStateMessagesTo: aCollection.
		( self isDirty: TMStringSet ) 
			ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMStringSet argument: self label ) ].
		( self isDirty: TMAlignmentSet ) 
			ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMAlignmentSet argument: self alignment ) ].
!

dispatchIndexedMessage: anIndexedMessage 
	TMStringSet = anIndexedMessage index
		ifTrue: [ ^self string: anIndexedMessage argument ].
	TMStringGet = anIndexedMessage index
		ifTrue: [ ^self string ].
	TMAlignmentSet = anIndexedMessage index
		ifTrue: [ ^self alignment: anIndexedMessage argument ].
	TMAlignmentGet = anIndexedMessage index
		ifTrue: [ ^self alignment ].
	^super dispatchIndexedMessage: anIndexedMessage.! !
	

!TerminalBasicWidget publicMethods !

addStateMessagesTo: aCollection 
	"Subclasses should redefine this and call super (me)"
	super addStateMessagesTo: aCollection.
	(self isDirty: TMEnabledSet)
		ifTrue: [ aCollection add: (IndexedMessageSend
			receiver: self name
			index: TMEnabledSet 
			argument: self isEnabled) ].!

dispatchIndexedMessage: anIndexedMessage 

	TMEnabledSet = anIndexedMessage index
		ifTrue: [ ^self enabled: anIndexedMessage argument ].
	TMEnabledGet = anIndexedMessage index
		ifTrue: [ ^self enabled ].		
	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage 		! !
	

!TerminalImageLabel publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	( self isDirty: TMScaleToFitSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMScaleToFitSet argument: self scaleToFit ) ].
	( self isDirty: TMURLSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMURLSet argument: self url ) ]		!

dispatchIndexedMessage: anIndexedMessage 

	TMURLSet = anIndexedMessage index
		ifTrue: [ ^self url: anIndexedMessage argument ].
	TMURLGet = anIndexedMessage index
		ifTrue: [ ^self url ].
	TMScaleToFitSet = anIndexedMessage index
		ifTrue: [ ^self scaleToFit: anIndexedMessage argument ].
	TMScaleToFitGet = anIndexedMessage index
		ifTrue: [ ^self scaleToFit ].
	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage ! !
	

!TerminalLabel publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	( self isDirty: TMStringSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMStringSet argument: self string ) ].
	(self isDirty: TMAlignmentSet)
		ifTrue: [ aCollection add: (IndexedMessageSend
			receiver: self name
			index: TMAlignmentSet 
			argument: self alignment) ].!

dispatchIndexedMessage: anIndexedMessage 

	TMStringSet = anIndexedMessage index
		ifTrue: [ ^self string: anIndexedMessage argument ].
	TMStringGet = anIndexedMessage index
		ifTrue: [ ^self string ].
	TMAlignmentSet = anIndexedMessage index
		ifTrue: [ ^self alignment: anIndexedMessage argument ].
	TMAlignmentGet = anIndexedMessage index
		ifTrue: [ ^self alignment ].

	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage 		! !
	

!TerminalInput publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	( self isDirty: TMEditableSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMEditableSet argument: self readOnly not ) ]!

dispatchIndexedMessage: anIndexedMessage 

	TMEditableSet = anIndexedMessage index
		ifTrue: [ ^self editable: anIndexedMessage argument ].
	TMEditableSet = anIndexedMessage index
		ifTrue: [ ^self editable ].
	^super dispatchIndexedMessage: anIndexedMessage ! !
	

!TerminalButton publicMethods !

addStateMessagesTo: aCollection 
	"Subclasses should redefine this"
	super addStateMessagesTo: aCollection .
! !
	

!TTableColumn publicMethods !

addStateMessagesTo: aCollection 
		(self isDirty: TMEditableSet)
		ifTrue: [ aCollection add: (IndexedMessageSend
			receiver: self name
			index: TMEditableSet 
			argument: self editable) ].!

dispatchIndexedMessage: anIndexedMessage 
	TMEditableSet = anIndexedMessage index 
		ifTrue: [ ^self editable: anIndexedMessage argument ].
	TMEditableGet = anIndexedMessage index 
		ifTrue: [ ^self editable ].! !
	

!TerminalWidgetBinaryAccessor class publicMethods !

registerClassName: aClassName for: aMap 

	| theClass |
	theClass := aClassName asClass.
	( aMap at: theClass tagID ) notNil 
		ifTrue: [ ^self error: 'tag id conflict' ].
	aMap at: theClass tagID put: theClass!

setupClientAccessors
	" TerminalWidgetBinaryAccessor setupClientAccessors
	"

	self setupCommonAccessors.
	ClientClassMap := Array new: 256.
	#(#TerminalLabel #TerminalInput #TerminalText #TerminalTableList #TTableColumn #TerminalButton #TerminalList #TerminalMenuBar #TerminalDropDownList #TerminalCompositeWidget #TerminalWindow #TerminalGroupBox #TerminalImageLabel #MessageSend #EvaluationCollection #TWidgetAppearance #TDisplayItem #TDisplayTableItem #TerminalCheckBox #IndexedMessageSend #TerminalRadioButton #Bounds #TMenuItem #TMenuSeparator #TerminalMenuList #TerminalMenu #TerminalDialog) 
		do: [:each | self registerClassName: each for: ClientClassMap].
	#(#TerminalTimer) 
		do: [:each | self registerClassName: each for: ClientClassMap]!

setupCommonAccessors

	self readSelectors at: 99 put: #readColor.
	self readSelectors at: 100 put: #readTerminalObject! !

!TerminalWidgetBinaryAccessor publicMethods !

readTerminalObject
	"100"

	| behavior instance|
	behavior := ClientClassMap at: stream next.
	instance := behavior new.
	instance kindDo: self.
	^instance! !
	

!TerminalList publicMethods !

addStateMessagesTo: aCollection 
	"Note: TMSelectionIndexSet is not inspected"
	
	super addStateMessagesTo: aCollection.
	( self isDirty: TMItemsSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMItemsSet argument: self items ) ].
	( self isDirty: TMSelectionIndexSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMSelectionIndexSet argument: self selectionIndex ) ].
!

dispatchIndexedMessage: anIndexedMessage 

	TMItemsSet = anIndexedMessage index
		ifTrue: [ ^self primItems: anIndexedMessage argument ].
	TMItemsGet = anIndexedMessage index
		ifTrue: [ ^self items].	
	TMSelectionIndexSet = anIndexedMessage index
		ifTrue: [ ^self primSelectionIndex: anIndexedMessage argument ]	.
	TMSelectionIndexGet = anIndexedMessage index
		ifTrue: [ ^self selectionIndex ]	.		

		
	" These messages give additional support, item can be retreived using items and index "
	TMSelectedItemSet = anIndexedMessage index
		ifTrue: [ ^self selectedItem: anIndexedMessage argument ]	.
	TMSelectedItemGet = anIndexedMessage index
		ifTrue: [ ^self selectedItem ]	.
	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage 		! !
	

!TerminalTableList publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	

	self tableColumns do:  [ :each | each addStateMessagesTo: aCollection ].
	" Should inputWidgets send state messages? Or is it 'implementation'? "
	" Maybe inpotWidgets send state to their non-editable counterparts "
"	self inputWidgets do: [ :each | each addStateMessagesTo: aCollection ]."
" 	self headingWidgets do: [ :each | each addStateMessagesTo: aCollection ]. "! !
	

!TerminalMenuList publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
!

dispatchIndexedMessage: anIndexedMessage 

	TMItemsSet = anIndexedMessage index
		ifTrue: [ ^self error: 'Trying to set Menu contents dynamically!! This is not allowed, menu contents is static.' ].
	super dispatchIndexedMessage: anIndexedMessage ! !
	

!TerminalCheckBox publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	( self isDirty: TMSelectionSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMSelectionSet argument: self selection ) ].
	( self isDirty: TMAlignmentSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMAlignmentSet argument: self tickAlignment ) ]!

dispatchIndexedMessage: anIndexedMessage 

	TMSelectionSet = anIndexedMessage index 
		ifTrue: [ ^self selection: anIndexedMessage argument ].
	TMSelectionGet = anIndexedMessage index
		ifTrue: [ ^self selection ].		
	TMTickAlignmentSet = anIndexedMessage index 
		ifTrue: [ ^self tickAlignment: anIndexedMessage argument ].
	TMTickAlignmentGet = anIndexedMessage index 
		ifTrue: [ ^self tickAlignment ].		
	"super handles unknown index"
	^super dispatchIndexedMessage: anIndexedMessage 		! !
	

!TerminalRadioButton publicMethods !

addStateMessagesTo: aCollection 

	super addStateMessagesTo: aCollection.
	( self isDirty: TMItemSet ) 
		ifTrue: [ aCollection add: ( IndexedMessageSend receiver: self name index: TMItemSet argument: self item ) ]!

dispatchIndexedMessage: anIndexedMessage 

	TMItemSet = anIndexedMessage index 
		ifTrue: [ ^self item: anIndexedMessage argument ].
	TMItemGet = anIndexedMessage index
		ifTrue: [ ^self item ].		
	^super dispatchIndexedMessage: anIndexedMessage .! !

