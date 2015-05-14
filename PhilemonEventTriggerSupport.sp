"Philemon SmalltalkMT exporter has done all the work
Version PhilemonEventTriggerSupport EM 4-0410"
PROJECTNAME PhilemonEventTriggerSupport .
PROJECTCATEGORY Philemon .
PREREQUISITES .
POOLS .

PROFILE
BEGIN
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	canTriggerEvent:
END
INSTANCEMETHODS
BEGIN
	triggerEvent:withArguments:ifNotHandled:
	actionListForEvent:
	actionForEvent:
	removeActionsSatisfying:forEvent:
	removeActionsForEvent:
	removeActionsWithReceiver:forEvent:
	triggerEvent:withArguments:
	removeAllActionsWithReceiver:
	hasEventTable
	when:send:to:with:
	evaluate
	evaluateWithArguments:
	when:evaluate:
	asValue
	eventTable
	whenAny:send:to:
	triggerEvent:ifNotHandled:
	triggerEvent:
	canTriggerEvent:
	triggerEvent:with:with:
	releaseEventTable
	messageSendClass
	triggerEvent:with:
	when:send:to:withArguments:
	setActionList:forEvent:
END

CLASS MessageSend
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	selector:arguments:receiver:
	receiver:selector:arguments:
	numberOfArgumentsFor:
	receiver:selector:
	receiver:selector:argument:
END
INSTANCEMETHODS
BEGIN
	selector
	argumentCount
	receiver
	arguments:
	evaluate
	receiver:
	,
	arguments
	selector:
	kindDo:
	argument
	evaluateWithArguments:
	collectArguments:
	asEvaluationCollection
	printOn:
END

CLASS EventTableManager
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	default
	release
END
INSTANCEMETHODS
BEGIN
	removeTableFor:
	initialize
	hasEventTableFor:
	eventTableFor:
END

CLASS EventModel
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	releaseEventTable
	eventTable
	hasEventTable
END

CLASS InterfaceAdaptor
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	on:accessWith:assignWith:
	on:accessWith:assignWith:event:
END
INSTANCEMETHODS
BEGIN
	event
	putSelector:
	getSelector:
	event:
	value
	value:
	getSelector
	changedValue
	putSelector
	subject
	subject:
END

CLASS EventValueHolder
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	on:
END
INSTANCEMETHODS
BEGIN
	compareSetValue:
	value
	valueChanged
	value:
	setValue:
	printOn:
END

CLASS EvaluationCollection
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	withAll:
	with:
END
INSTANCEMETHODS
BEGIN
	evaluateWithArguments:
	do:
	copyWith:
	asMinimalRepresentation
	kindDo:
	includes:
	collection:
	asEvaluationCollection
	evaluate
	reject:
	,
	size
END

! !

Object subclass: #MessageSend
    instanceVariableNames: 'selector arguments receiver '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #EventTableManager
    instanceVariableNames: 'dictionary '
    classVariableNames: 'SoleInstance '
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #EventModel
    instanceVariableNames: 'eventTable '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
EventModel subclass: #InterfaceAdaptor
    instanceVariableNames: 'subject getSelector putSelector event '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
EventModel subclass: #EventValueHolder
    instanceVariableNames: 'value '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #EvaluationCollection
    instanceVariableNames: 'collection '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!Object class publicMethods !

canTriggerEvent: eventName
	"Answer whether instances of the receiver can trigger an event named <eventName>.
	By default, subclasses of Object are 'ambivalent'."

	^true! !

!Object publicMethods !

actionForEvent: anEventNameSymbol
	"Answer the action to evaluate when the event
	named <anEventNameSymbol> is triggered by the receiver."

	^self eventTable
		at: anEventNameSymbol asSymbol
		ifAbsent: [nil]!

actionListForEvent: anEventNameSymbol
	"Answer an editable list of actions that get evaluated
	when the event named <anEventNameSymbol> is triggered."

	^(self eventTable
		at: anEventNameSymbol asSymbol
		ifAbsent: [EvaluationCollection withAll: #()]) asEvaluationCollection!

asValue
	^EventValueHolder on: self!

canTriggerEvent: anEventNameSymbol
	"Answer <true> if the receiver can trigger an event named <anEventNameSymbol>."

	^self class canTriggerEvent: anEventNameSymbol!

evaluate
	"Answer the result of evaluating the receiver, or the receiver itself if
	it does not have any execution semantics."

	^self evaluateWithArguments: #()!

evaluateWithArguments: anArray
	"Answer the result of evaluating the receiver, or the receiver itself if
	it does not have any execution semantics."

	^self!

eventTable
	"Subclasses may store the table locally"
	
	^EventTableManager default eventTableFor: self!

hasEventTable
	
	^EventTableManager default hasEventTableFor: self!

messageSendClass
	^MessageSend!

releaseEventTable
	"Subclasses may store the table locally"
	
	^EventTableManager default removeTableFor: self!

removeActionsForEvent: anEventNameSymbol
	"Remove all actions for the event named <anEventNameSymbol>."

	| table |
	table := self eventTable.
	table removeKey: anEventNameSymbol asSymbol ifAbsent: [].
	table isEmpty
		ifTrue: [EventTableManager default removeTableFor: self ]!

removeActionsSatisfying: aOneArgBlock forEvent: aSymbol
		"Remove all actions for the event <aSymbol> that satisfy <aOneArgBlock>."
	self
		setActionList:
			((self actionListForEvent: aSymbol)
				reject: [:anAction | aOneArgBlock value: anAction])
		forEvent: aSymbol!

removeActionsWithReceiver: anObject forEvent: event
	"Remove all actions for <event> in the receiver's event table
	which have <anObject> as their receiver."
	
	self 
		setActionList: ((self actionListForEvent: event)
			reject:[ :each | each receiver = anObject ])
		forEvent: event!

removeAllActionsWithReceiver: anObject
	"Remove all actions for all events in the receiver's event table
	which have <anObject> as their receiver."
		
	| table | 
	self hasEventTable ifFalse:[^self].
	table := self eventTable.
	table keys do:[ :event | self removeActionsWithReceiver: anObject forEvent: event ].
	table isEmpty
		ifTrue: [EventTableManager default removeTableFor: self ]
!

setActionList: actionSequence forEvent: eventName
	"Set the list of actions that get evaluated when
	the event named <eventName> is triggered by
	the receiver to <actionSequence>."
	
	| sequence |
	sequence := actionSequence asMinimalRepresentation.
	sequence isNil
		ifTrue: [self eventTable removeKey: eventName ifAbsent:[]]
		ifFalse: [self eventTable at: eventName asSymbol put: sequence ]!

triggerEvent: anEventNameSymbol
	"Trigger the event named <anEventNameSymbol>. The Answer is undefined"

	(self actionForEvent: anEventNameSymbol) evaluate!

triggerEvent: anEventNameSymbol ifNotHandled: exceptionBlock
	"Trigger the event named <anEventNameSymbol>.  If the event is
	not handled, execute the <exceptionBlock>, The Answer is undefined"

	(self eventTable
		at: anEventNameSymbol asSymbol
		ifAbsent: [^exceptionBlock value])
			evaluate!

triggerEvent: anEventNameSymbol with: anArgumentObject
	"Trigger the event <anEventNameSymbol> using the given
	<anArgumentObject> as the argument.  The Answer is undefined"

	self
		triggerEvent: anEventNameSymbol
		withArguments: (Array with: anArgumentObject)!

triggerEvent: anEventNameSymbol with: firstArgumentObject with: secondArgumentObject 
	"Trigger the event <anEventNameSymbol> using the <firstArgumentObject>
	and <secondArgumentObject> as the arguments.  The Answer is undefined."

	self
		triggerEvent: anEventNameSymbol
		withArguments: (Array
			with: firstArgumentObject
			with: secondArgumentObject)!

triggerEvent: anEventNameSymbol withArguments: anArgumentCollection
	"Trigger the event <anEventNameSymbol> using the
	elements of the <anArgumentCollection> as the arguments. The Answer is undefined"

	(self actionForEvent: anEventNameSymbol)
		evaluateWithArguments: anArgumentCollection!

triggerEvent: anEventNameSymbol withArguments: anArgumentCollection ifNotHandled: exceptionBlock
	"Trigger the event <anEventNameSymbol> using the elements of
	the <anArgumentCollection> as the arguments.  If the event is
	not handled, execute the <exceptionBlock>, The Answer is undefined"

	(self eventTable
		at: anEventNameSymbol asSymbol
		ifAbsent: [^exceptionBlock value])
			evaluateWithArguments: anArgumentCollection!

when: anEventNameSymbol evaluate: anAction
	"Append <anAction> to the list of actions to evaluate
	when the receiver triggers the event named <anEventNameSymbol>."

	| actions |
	(self canTriggerEvent: anEventNameSymbol)
		ifFalse: [^self error: 'Bad event' , anEventNameSymbol].
	actions := self actionListForEvent: anEventNameSymbol.
	(actions includes: anAction)
		ifTrue: [^self].
	self
		setActionList: (actions copyWith: anAction)
		forEvent: anEventNameSymbol!

when: anEventNameSymbol send: aSelectorSymbol to: anObject with: anArgumentObject 
	"Form an action with <anObject> as the receiver, 
	a <aSelectorSymbol> as the message selector, and <anArgumentObject> 
	as the argument and append it to the actions list for the event named <anEventNameSymbol>."

	self
		when: anEventNameSymbol
		send: aSelectorSymbol
		to: anObject
		withArguments: (Array with: anArgumentObject)!

when: anEventNameSymbol send: aSelectorSymbol to: anObject withArguments: anArgumentCollection
	"Form an action with <anObject> as the receiver,
	a <aSelectorSymbol> as the message selector, and the elements of the <anArgumentCollection> as the arguments
	and append it to the actions list for the event named <anEventNameSymbol>."

	"simple version"
	self
		when: anEventNameSymbol
		evaluate: (self messageSendClass new
			receiver: anObject 
			;selector: aSelectorSymbol 
			;arguments: anArgumentCollection)!

whenAny: eventNameCollection send: aSelector to: aReceiver

	eventNameCollection do:[:each | self when: each send: aSelector to: aReceiver]! !
	

!MessageSend class publicMethods !

numberOfArgumentsFor: aSelector
		"Answer the number of arguments required by a message whose
		selector is <aSelector>, assuming <aSelector> is a legal
		method selector."
	aSelector first isLetter
		ifTrue: [ ^aSelector occurrencesOf: $: ].
   ( #(
		#= #~= #== #~~ #> #>= #< #<=   " comparison operations "
		#+ #- #*  #/ #\\  #//                    " arithmetic operations "
	   #& #|                                 " logical operations "
	   #@ #,                                " miscellaneous "
		) includes: aSelector asSymbol)
			ifTrue: [^1].
	^aSelector occurrencesOf: $:!

receiver: r selector:s 
	^self new selector: s ;arguments: #();receiver: r!

receiver: r selector: s argument: oneA
	^self 
		selector: s 
		arguments: (Array with: oneA) 
		receiver: r!

receiver: r selector: s arguments: a 
	^self 
		selector: s 
		arguments: a 
		receiver: r!

selector:s arguments:a receiver: r
	^self new selector: s ;arguments: a;receiver: r! !

!MessageSend publicMethods !

, aMessageSend

	"Answer a new collection with the receiver and the arguments.
	Note: the order of messages must be invariant"

	| collection |
	collection := OrderedCollection new.
	collection add: self.
	aMessageSend asEvaluationCollection do: [ :each | collection add: each ].
	^EvaluationCollection withAll: collection!

argument
	"Pre: arguments not empty"
	^arguments first!

argumentCount
	^arguments isNil ifTrue: [0] ifFalse: [arguments size]!

arguments
	^arguments!

arguments: anObject
	arguments := anObject!

asEvaluationCollection
	^EvaluationCollection with: self!

collectArguments: evaluationArguments
		"Private - answer the action arguments with which to evaluate
		the receiver, collecting from the <evaluationArguments>
		and the predefined arguments in the receiver. "
	| predefinedArgs |
	predefinedArgs := self arguments.
	^(evaluationArguments size = predefinedArgs size)
		ifTrue: [evaluationArguments]
		ifFalse:
			[( ( predefinedArgs isNil or: [predefinedArgs isEmpty])
				ifTrue: [ predefinedArgs := Array new: (self class numberOfArgumentsFor: selector) ]
				ifFalse: [predefinedArgs copy] )
					replaceFrom: 1
					to: (evaluationArguments size min: predefinedArgs size)
					with: evaluationArguments
					startingAt: 1]!

evaluate
	^self receiver perform: self selector withArguments: self arguments!

evaluateWithArguments: anArray
		"Answer the result of sending the message represented by
		the receiver."
	^self receiver
		perform: self selector
		withArguments: (self collectArguments: anArray)!

kindDo: aRequestor
	^aRequestor doMessageSend: self!

printOn: aStream
	aStream nextPutAll: self class name ;nextPut: $( ;print: receiver ;nextPut:$, ;print: selector ;nextPut: $)!

receiver
	^receiver!

receiver: anObject
	receiver := anObject!

selector
	^selector!

selector: anObject
	selector := anObject! !
	

!EventTableManager class publicMethods !

default
	SoleInstance isNil
		ifTrue: [SoleInstance := self new initialize].
	^SoleInstance!

release
	SoleInstance := nil! !

!EventTableManager publicMethods !

eventTableFor: anObject

	^dictionary at: anObject ifAbsentPutUsing:[IdentityDictionary new]!

hasEventTableFor: anObject
	^dictionary includesKey: anObject!

initialize
	dictionary := IdentityDictionary new!

removeTableFor: anObject
	^dictionary removeKey: anObject ifAbsent:[]! !
	

!EventModel publicMethods !

eventTable
	^eventTable ifNil:[eventTable := IdentityDictionary new]!

hasEventTable
	^eventTable notNil!

releaseEventTable
	"Forget about all dependents"
	
	eventTable := nil! !
	

!InterfaceAdaptor class publicMethods !

on: anObject accessWith: getSelector assignWith: putSelector

	^self on: anObject accessWith: getSelector assignWith: putSelector event: #changed!

on: anObject accessWith: getSelector assignWith: putSelector event: eventSymbol

	| adaptor |
	adaptor := self new.
	anObject when: #changed send: #changedValue to: adaptor.
	adaptor 
			subject: anObject
			;getSelector: getSelector
			;putSelector: putSelector
			;event: eventSymbol.
	^adaptor
		! !

!InterfaceAdaptor publicMethods !

changedValue
	self triggerEvent: event!

event
	^event!

event: anEventSymbol
	event := anEventSymbol.!

getSelector
	^getSelector!

getSelector: anObject
	getSelector := anObject!

putSelector
	^putSelector!

putSelector: anObject
	putSelector := anObject!

subject
	^subject!

subject: anObject
	subject := anObject!

value
	^self subject perform: self getSelector!

value: anObject
	self subject perform: self putSelector with: anObject! !
	

!EventValueHolder class publicMethods !

on: anObject
	^self new setValue: anObject! !

!EventValueHolder publicMethods !

compareSetValue: aValue
	"First compare to the current
	before changing to @aValue"
	
	value = aValue	
		ifFalse: [self value: aValue]!

printOn: aStream
	aStream nextPut: $[.
	value printOn: aStream.
	aStream nextPut: $]!

setValue: anObject
	"no event trigger"
	
	value := anObject!

value
	^value!

value: anObject

	value := anObject.
	self valueChanged!

valueChanged
	"Trigger the appropriate event with arguments if needed"
	
	self hasEventTable ifTrue: [self triggerEvent: #changed]! !
	

!EvaluationCollection class publicMethods !

with: anEvaluatable
	^self new collection: (Array with: anEvaluatable)!

withAll: aCollection
	^self new collection: aCollection asArray! !

!EvaluationCollection publicMethods !

, anObject
	| messages |
	messages := OrderedCollection new.
	self do: [ :each | messages add: each ].
	anObject asEvaluationCollection do: [ :each | messages add: each ].
	^self class withAll: messages!

asEvaluationCollection
	"That's what I am"!

asMinimalRepresentation

	^collection isEmpty
		ifTrue: [nil]
		ifFalse: [collection size = 1
			ifTrue: [collection at: 1]
			ifFalse: [self]]!

collection: aSequenceable
	collection := aSequenceable!

copyWith: anAction
	^self class withAll: (collection copyWith: anAction)!

do: oneArgBlock
	collection do: oneArgBlock !

evaluate
		"Answer the result of evaluating the elements of the receiver."
	| answer |
	answer := nil.
	collection do: [:each| answer := each evaluate].
	^answer!

evaluateWithArguments: anArray
		"Answer the result of evaluating the elements of the receiver
		with <anArray> as the arguments."
	| answer |
	answer := nil.
	collection do: [:each| answer := each evaluateWithArguments: anArray].
	^answer!

includes: anAction
	^collection includes: anAction!

kindDo: aRequestor
	^aRequestor doEvaluationCollection: self!

reject: oneArgBlock
	^self class withAll:(collection reject: oneArgBlock)!

size
	^collection size! !

