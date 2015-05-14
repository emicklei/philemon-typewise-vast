"Philemon SmalltalkMT exporter has done all the work
Version PhilemonEventTriggerSupportTest [552] EM 4-0129"
PROJECTNAME PhilemonEventTriggerSupportTest .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonEventTriggerSupport .
POOLS .

PROFILE
BEGIN
END

CLASS LoggingEventTableManager
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	unInstall
	install
END
INSTANCEMETHODS
BEGIN
	eventTableFor:
END

CLASS EventTriggerTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	setUp
	testValueChangeTrigger
	testTriggerWithArgument2
	testTriggerWithArgument1
	tearDown
	testInterfaceAdaptor
	eventValue
	eventValue:
END

! !

EventTableManager subclass: #LoggingEventTableManager
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TestCase subclass: #EventTriggerTest
    instanceVariableNames: 'eventValue valueChangeTrigger '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!LoggingEventTableManager class publicMethods !

install
	"self install"
	SoleInstance := self new initialize!

unInstall
	"self unInstall"
	SoleInstance := super new initialize! !

!LoggingEventTableManager publicMethods !

eventTableFor: anObject
	'eventTable request for ' echo: anObject.
	^super eventTableFor: anObject! !
	

!EventTriggerTest publicMethods !

eventValue
	^eventValue!

eventValue: anObject
	eventValue := anObject!

setUp
	valueChangeTrigger := EventValueHolder new!

tearDown
	valueChangeTrigger removeActionsForEvent: #changed!

testInterfaceAdaptor

	| p  a|
	p := Point x:0 y:0.
	a := InterfaceAdaptor on: p accessWith: #x assignWith: #x:.
	self assert: a value = 0.
	a value:10.
	self assert: a value = 10.
	p removeAllActionsWithReceiver: a!

testTriggerWithArgument1

	| value1  value2 twoArgBlock object|
	value1 := nil.
	value2 := nil.
	twoArgBlock := [ :val1 :val2 |  value1 := val1. value2 := val2 ].
	object := Object new.
	[object when: #event send: #value:value: to: twoArgBlock withArguments: #( nil 2 ).
	
	object triggerEvent: #event with: 1.
	self assert: value1 = 1.
	] ensure: [object removeAllActionsWithReceiver: twoArgBlock]!

testTriggerWithArgument2

	| value1  value2 twoArgBlock object|
	value1 := nil.
	value2 := nil.
	twoArgBlock := [ :val1 :val2 |  value1 := val1. value2 := val2 ].
	object := Object new.
	[object when: #event send: #value:value: to: twoArgBlock withArguments: #( nil nil ).
	
	object triggerEvent: #event with: 1 with: 2.
	self assert: value1 = 1.
	self assert: value2 = 2.
	] ensure: [object removeAllActionsWithReceiver: twoArgBlock]!

testValueChangeTrigger
	
		valueChangeTrigger when: #changed send: #eventValue: to: self with: #changed.
		valueChangeTrigger value: nil.
		self should:[ eventValue == #changed ].
		eventValue := nil.
		valueChangeTrigger removeAllActionsWithReceiver: self.
		valueChangeTrigger value: nil.
		self should:[ eventValue isNil ].
		
		! !

