"Philemon SmalltalkMT exporter has done all the work
Version PhilemonObjectSerialization EM 4-0502a"
PROJECTNAME PhilemonObjectSerialization .
PROJECTCATEGORY Philemon .
PREREQUISITES .
POOLS SerializationConstants .

PROFILE
BEGIN
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	phiReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS UndefinedObject
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Point
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Rectangle
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Block
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Boolean
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	phiReadWith:
	elcReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Behavior
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Signal
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS BinaryObjectAccessor
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	initializeReadSelectors
	initialize
	readSelectors
	readSelectorFor:
	objectFromByteArray:
	release
	on:
	new
	readSelectorEntries
	byteArrayFromObject:
	serializationException
END
INSTANCEMETHODS
BEGIN
	atEnd
	writeSymbol:
	writeCollection:ofType:
	writeNumber:
	storeLong:
	readByteString
	storeInt:
	readFloat
	storeFloat:
	next
	storeByte:
	writeObject:on:
	writeFraction:
	writeKeyedCollection:
	storeShort:
	readHeader
	writePoint:
	writeByte:
	readDictionary
	initialize
	readByteArray
	writeTime:
	storeDouble:
	writeBehavior:
	writeArray:
	writeSet:
	writeInteger:
	storeByteString:
	writeObjectByReference:
	readBoolean
	readSymbol
	readObject
	readRectangle
	readFraction
	readUTF
	writeZero
	readDouble
	readTrue
	readByte
	readFalse
	writeOrderedCollection:
	readNewObject
	readLong
	storeSymbol:
	nextPut:
	writeSortedCollection:
	readInteger
	storeUTF:
	readTime
	writeRectangle:
	readKeyedCollection
	stream
	readInt
	readObjectByReference
	writeObjectByValue:
	dispatch:
	readSortedCollection
	readPoint
	readNil
	readArray
	readCharacter
	writeByteString:
	readObjectStateFor:
	writeSequenceableCollection:
	readBehavior
	writeNil
	writeIEEEFloat:
	storeBoolean:
	readIEEEFloat
	close
	contents
	writeHeaderOn:
	newReferenceIndex
	storeDate:
	writeBoolean:
	stream:
	readObjectFrom:
	writeFloat:
	readDate
	storeArray:
	writeCharacter:
	writeByteArray:
	readHeaderFrom:
	readShort
	readZero
	reset
	readSet
	writeDate:
	readNext
	readOrderedCollection
END

CLASS OrderedCollection
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Set
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Number
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	phiReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Integer
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	elcReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
	fasterFactorial
END

CLASS Float
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	elcReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Fraction
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS SortedCollection
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	fromSortedArray:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS KeyedCollection
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS ByteArray
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS SequenceableCollection
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	phiReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Time
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS String
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	phiReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Symbol
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	phiReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Date
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

CLASS Character
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	phiReadWith:
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
END

! !
Compiler poolAdd: 'SerializationConstants' value: (StringDictionary new 
 at: 'TTime' put: 18;
 at: 'TBehavior' put: 16;
 at: 'TFalse' put: 3;
 at: 'TInt' put: 6;
 at: 'TTrue' put: 2;
 at: 'TSortedCollection' put: 34;
 at: 'TArray' put: 14;
 at: 'TSet' put: 15;
 at: 'TDate' put: 19;
 at: 'TObjectByReference' put: 36;
 at: 'TNull' put: 1;
 at: 'TByteString' put: 11;
 at: 'TByteArray' put: 31;
 at: 'TLong' put: 7;
 at: 'TZero' put: 39;
 at: 'MaxFloat' put: 3.402823e38;
 at: 'TTwoByteString' put: 33;
 at: 'TUTF8String' put: 12;
 at: 'TShort' put: 5;
 at: 'TByte' put: 4;
 at: 'TChar' put: 10;
 at: 'TObject' put: 17;
 at: 'TOrderedCollection' put: 27;
 at: 'TRectangle' put: 37;
 at: 'TDictionary' put: 32;
 at: 'TException' put: 38;
 at: 'TDouble' put: 9;
 at: 'TFraction' put: 29;
 at: 'TKeyedCollection' put: 28;
 at: 'TFloat' put: 8;
 at: 'TSymbol' put: 13;
 at: 'TPoint' put: 35;
 at: 'TColor' put: 99;
 at: 'TBlock' put: 26;
 at: 'MinFloat' put: 1.157494e-38;
yourself).
!

Object subclass: #BinaryObjectAccessor
    instanceVariableNames: 'stream dispatchSelectors referenced referenceCount version '
    classVariableNames: 'ReadSelectors SerializationError '
    poolDictionaries: 'SerializationConstants '
	comment: '' 
	category: '' !	

!Object class publicMethods !

phiReadWith: aReader

	^aReader readObjectWithClass: self

	! !

!Object publicMethods !

phiWriteWith: aWriter

	aWriter writeObject: self! !
	

!UndefinedObject publicMethods !

phiWriteWith: aWriter

	aWriter writeNil! !
	

!Point publicMethods !

phiWriteWith: anAccessor
	anAccessor writePoint: self! !
	

!Rectangle publicMethods !

phiWriteWith: anAccessor
	anAccessor writeRectangle: self! !
	

!Block publicMethods !

phiWriteWith: aWriter
	
	aWriter writeBlock: self! !
	

!Boolean class publicMethods !

elcReadWith: aReader
	
	^aReader readBoolean!

phiReadWith: aReader
	
	^aReader readBoolean! !

!Boolean publicMethods !

phiWriteWith: aStream
	aStream writeBoolean: self! !
	

!Behavior publicMethods !

phiWriteWith: aWriter

	aWriter writeBehavior: self! !
	

!Signal publicMethods !

phiWriteWith: aWriter
	aWriter writeSignal: self! !
	

!BinaryObjectAccessor class publicMethods !

byteArrayFromObject: anObject 

	| outStream |
	outStream := WriteStream on: ( ByteArray new: 256 ).
	self new writeHeaderOn: outStream; writeObject: anObject on: outStream.
	^outStream contents!

initialize
	"self initialize"

	SerializationError := Error.
	self initializeReadSelectors
!

initializeReadSelectors

	| inStream |
	inStream := ReadStream on: self readSelectorEntries.
	[ inStream atEnd ]
		whileFalse: 
			[  | constant | 
			constant := self constantAt: inStream next asString.
			self readSelectors at: constant put: inStream next ]!

new

	^super new initialize!

objectFromByteArray: anArray 

	| inStream |
	inStream := ReadStream on: anArray.
	^self new  readHeaderFrom: inStream ; readObjectFrom: inStream!

on: aStream

	^self new stream: aStream!

readSelectorEntries

	^#(
		TNull readNil
		TTrue readTrue 
		TFalse readFalse 

		TZero readZero
		TByte readByte
		TShort readShort
		TInt readInt
		TLong readLong

		TFraction readFraction
		TFloat readFloat 
		TDouble readDouble

		TChar readCharacter
		TByteString readByteString 
		TTwoByteString readTwoByteString
		TUTF8String readUTF
		TSymbol readSymbol

		TArray readArray
		TOrderedCollection readOrderedCollection
		TSortedCollection readSortedCollection
		TKeyedCollection readKeyedCollection
		TSet readSet
		TBehavior readBehavior
		TObject readObject
		TObjectByReference readObjectByReference
		TByteArray readByteArray
		TDictionary readDictionary

		TPoint readPoint
		TRectangle readRectangle
		TTime readTime
		TDate readDate
		TBlock readBlock

		TException readException
 	)  !

readSelectorFor: aCode

	^self readSelectors at: aCode!

readSelectors

	ReadSelectors isNil ifTrue: [ReadSelectors := Array new: 256].
	^ReadSelectors!

release

	ReadSelectors := nil!

serializationException

	^SerializationError! !

!BinaryObjectAccessor publicMethods !

atEnd

	^self stream atEnd!

close

	self stream close!

contents

	| result |
	result := (Array new: 100) writeStream.
	self do: [:each | result nextPut: each].
	^result contents!

dispatch: code

	| selector |
	(selector := dispatchSelectors at: code) isNil
		ifTrue: [ self class serializationException signalWith: 'unknown code: ' , code printString ]
		ifFalse: [  ^self perform: selector ]!

initialize
	dispatchSelectors := self class readSelectors.
	referenced := IdentityDictionary new: 128.
	referenceCount := 0.
	version := 2.!

newReferenceIndex
	referenceCount := referenceCount + 1.
	^referenceCount!

next

	^self readNext!

nextPut: anObject

	anObject phiWriteWith: self !

readArray

	| newArray |
	newArray := Array new: self readInteger.
	1 to: newArray size do: [:index | newArray at: index put: self readNext].
	^newArray!

readBehavior

	^Smalltalk classAt: self readSymbol ifAbsent: [ nil ]!

readBoolean

	^self readNext!

readByte

	^stream next!

readByteArray

	| newArray |
	newArray := ByteArray new: self readInteger.
	1 to: newArray size do:[ :i | newArray at: i put: stream next ].
	^newArray!

readByteString

	^String fromBytes: (stream next: self readInteger)!

readCharacter

	^Character value: self readInteger!

readDate
	"	^Date fromDays: (( self readInteger / 1000 ) / Time secondsPerDay) asInteger
"

	| year month day |
	year := self readInt.
	month := stream next.
	day := stream next.
	^Date 
		newDay: day
		monthIndex: month
		year: year!

readDictionary

	| theClass |
	theClass := self readNext.
	^theClass withKeysAndValues: self readArray!

readDouble
	"Read IEEE representation of a double"

	"hack"
	| newDouble |
	newDouble := Float basicNew: 8.
	1 to: newDouble basicSize do:[ :i |
		newDouble basicAt: 9 - i put: stream next ].
	^newDouble!

readFalse
	"implicit tag"

	^false!

readFloat

	"Read VA representation of Float"

	| aFloat |
	aFloat := Float basicNew: stream next.
	aFloat basicSize 
		to: 1 
		by: -1 
		do: [ :i | aFloat basicAt: i put: stream next ].
	^aFloat!

readFraction

	^Fraction
		numerator: self readInteger
		denominator: self readInteger!

readHeader
	self readHeaderFrom: stream!

readHeaderFrom: inStream

	| itsVersion |
	( inStream next: 3) asString = 'BOA'
		ifFalse:[self error: '[BOA] Non-BOA formatted contents: ' , inStream contents asString].
	(itsVersion := inStream next) = version
		ifFalse:[self error: '[BOA] version conflict, read: ', itsVersion asString , ' expected: ' , version asString]!

readIEEEFloat

	"Read IEEE representation of a float"
	
	| aByteArray exponent mantissa newFloat sign |
	aByteArray := ByteArray new: 4.
	4 to: 1 by: -1 do: [ :i | aByteArray at: i put: stream next ]. 
	
	sign := ( aByteArray at: 4 ) bitAnd: 128.
	exponent := ( ( ( aByteArray at: 4 ) bitAnd: 127 ) * 2 ) + ( ( ( aByteArray at: 3 ) bitAnd: 128 ) bitShift: -7 ) + 896.
	mantissa := ( ( aByteArray at: 1 ) + ( ( aByteArray at: 2 ) bitShift: 8 ) + ( ( ( ( aByteArray at: 3 ) bitAnd: 127 ) + 128 ) bitShift: 16 ) ) bitShift: 5. 
	
	newFloat := Float basicNew: 8.
	1 to: 3 by: 1 do: [ :i | newFloat basicAt: i put: 0 ].
	4 to: 6 by: 1 do: [ :i | 
		newFloat basicAt: i put: ( mantissa bitAnd: 255 ).
		mantissa := mantissa bitShift: -8 ].
	newFloat basicAt: 7 put: ( mantissa bitAnd: 15 ) + ( ( exponent bitAnd: 15 ) bitShift: 4 ).
	newFloat basicAt: 8 put: sign + ( exponent bitShift: -4 ).
	^newFloat!

readInt

	"Use two's complement notation"

	| result |
	result := ( ( stream next ) << 24 ) + ( ( stream next ) << 16 ) + ( ( stream next ) << 8 ) + ( ( stream next ) << 0 ).
	result > 16r7FFFFFFF
		ifTrue: [ ^result - 16r100000000 ]
		ifFalse: [ ^result ]!

readInteger
	"
	The next byte will tell whether it is a byte or a long.
	This message should only be send when an integer is expected
	"

	| tag |
	tag := stream next.
	tag = TZero ifTrue: [ ^0 ].
	tag = TByte ifTrue: [^stream next].
	tag = TShort ifTrue: [^self readShort].
	tag = TInt ifTrue: [^self readInt].
	tag = TLong ifTrue: [^self readLong].
	^self error: 'int tag expected'!

readKeyedCollection

	| className howMany newCollection |
	className := self readSymbol.
	howMany := self readInteger.
	newCollection := (Smalltalk classAt: className) new: howMany.
	howMany timesRepeat:[ newCollection at: self readNext put: self readNext ].
	^newCollection!

readLong

	"Use two's complement notation"

	| result |
	result := ( ( stream next ) << 56 ) + ( ( stream next ) << 48 ) + ( ( stream next ) << 40 ) + ( ( stream next ) << 32 ) + ( ( stream next ) << 24 ) + ( ( stream next ) << 16 ) + ( ( stream next ) << 8 ) + ( ( stream next ) << 0 ).
	result > 16r7FFFFFFFFFFFFFFF
		ifTrue: [ ^result - 16r10000000000000000 ]
		ifFalse: [ ^result ]!

readNewObject

	| className newObject basicSize |
	className := self readSymbol.
	basicSize := stream next.
	newObject := basicSize = 0
		ifTrue: [ (Smalltalk classAt: className) basicNew ]
		ifFalse: [ (Smalltalk classAt: className) basicNew: basicSize ].
	^newObject!

readNext

	^self dispatch: stream next
		!

readNil
	"implicit tag"

	^nil!

readObject

	| newObject |
	newObject := self readNewObject.
	self readObjectStateFor: newObject.
	^newObject!

readObjectByReference

	| referencedObject |
	(stream peekFor: TObject)
		ifTrue: 
			[referencedObject := self readNewObject.
			referenced at: self newReferenceIndex put: referencedObject.
			self readObjectStateFor: referencedObject.
			^referencedObject ]
		ifFalse: 
			[ ^referenced at: self readNext ]!

readObjectFrom: aStream

	stream := aStream.
	^self readNext!

readObjectStateFor: newObject

	1 to: newObject basicSize do:[ :i |
		newObject basicAt: i put: self readNext ].	
	1 to: newObject class instSize do:[ :i |
		newObject instVarAt: i put: self readNext ].!

readOrderedCollection

	| newOrderedCollection size |
	size := self readInteger.
	newOrderedCollection := OrderedCollection new: size.
	size timesRepeat: [newOrderedCollection add: self readNext ].
	^newOrderedCollection!

readPoint

	^Point x: self readNext y: self readNext!

readRectangle

	^Rectangle 
		left: self readNext 
		right: self readNext 
		top: self readNext 
		bottom: self readNext!

readSet

	^self readArray asSet!

readShort

	^stream next << 8 + stream next!

readSortedCollection

	^SortedCollection fromSortedArray: self readArray!

readSymbol

	^self readByteString asSymbol!

readTime

	^Time fromMilliseconds: self readInteger!

readTrue
	"implicit tag"

	^true!

readUTF
	"It's not clear if this is useful."
	
	^self readUTFOfClass: nil!

readZero
	"implicit tag"
	^0!

reset

	self stream reset!

storeArray: anArray

	self writeInteger: anArray size.
	anArray do:[ :each | each phiWriteWith: self ]!

storeBoolean: aBoolean

	aBoolean
		ifTrue: [ stream nextPut: TTrue ]  
		ifFalse: [ stream nextPut: TFalse ]!

storeByte: aByteInteger

	stream nextPut: aByteInteger!

storeByteString: aString

	self writeInteger: aString size.
	aString do:[ :ch | stream nextPut: ch asInteger ].!

storeDate: aDate
	"<year 1901..><month 1..12><day 1..31>"

	self storeInt: aDate year.
	stream nextPut: aDate monthIndex.
	stream nextPut: aDate dayOfMonth!

storeDouble: aDouble

	aDouble basicSize to: 1 by: -1 do:[ :i |
		stream nextPut: (aDouble basicAt: i) ].
!

storeFloat: aFloat 
	
	|  exponent mantissa sign b1 b2 b3 b4 |

	sign := ( aFloat basicAt: 8 ) bitAnd: 128.
	exponent := ( ( ( aFloat basicAt: 8 ) bitAnd: 127 ) bitShift: 4 ) + 
		( ( ( aFloat basicAt: 7 ) bitAnd: 16rF0 ) bitShift: -4 ) - 896.
	mantissa := ( (aFloat basicAt: 7 ) bitAnd: 16r0F ) + 16.
	3 to: 8 do: [ :i | mantissa := ( mantissa bitShift: 8 ) + ( aFloat basicAt: 9-i ) ]. 
	
	mantissa := mantissa bitShift: -29.
	b1 := mantissa bitAnd: 255.
	mantissa := mantissa bitShift: -8.
	b2 := mantissa bitAnd: 255.
	mantissa := ( mantissa bitShift: -8 ) bitAnd: 127.
	b3 := mantissa + ( ( exponent bitAnd: 1 ) bitShift: 7 ).
	b4 := sign + ( exponent bitShift: -1 ). 
	
	stream nextPut: b4;
		nextPut: b3;
		nextPut: b2;
		nextPut: b1!

storeInt: anInteger 
	
	stream nextPut: ( anInteger >> 24 ) & 16rFF.
	stream nextPut: ( anInteger >> 16 ) & 16rFF.
	stream nextPut: ( anInteger >> 8 ) & 16rFF.
	stream nextPut: ( anInteger >> 0 ) & 16rFF!

storeLong: anInteger 
	
	stream nextPut: ( anInteger >> 56 ) & 16rFF.
	stream nextPut: ( anInteger >> 48 ) & 16rFF.
	stream nextPut: ( anInteger >> 40 ) & 16rFF.
	stream nextPut: ( anInteger >> 32 ) & 16rFF.
	stream nextPut: ( anInteger >> 24 ) & 16rFF.
	stream nextPut: ( anInteger >> 16 ) & 16rFF.
	stream nextPut: ( anInteger >> 8 ) & 16rFF.
	stream nextPut: ( anInteger >> 0 ) & 16rFF.!

storeShort: aShortInteger 

	stream	nextPut: ( aShortInteger >> 8 ) & 16rFF.
	stream	nextPut: ( aShortInteger >> 0 ) & 16rFF!

storeSymbol: aSymbol
	"Pre: aSymbol can be represented by a sequence of bytes"

	self storeByteString: aSymbol!

storeUTF: aString
	"Implementation taken from DataOutputStream>>writeUTF(String) in JDK 1.0"
	
	| utflength |
	stream nextPut: 0; nextPut: 0.
	utflength := 0.
	aString do: [:each | | cv bytes |
		cv := each asInteger.
		bytes :=
			( ( cv >= 16r0001 ) and: [ cv <= 16r007F ] )		"why is 16r0000 excluded here?"
				ifTrue: [ Array with: cv ]	"1 byte:  0ddddddd"
				ifFalse: [ cv > 16r07FF
					ifFalse: 
						[ Array	"110ddddd 10dddddd"
							with: ( 16rC0 bitOr: ( ( cv >> 6 ) bitAnd: 16r1F ) )
							with: ( 16r80 bitOr: ( ( cv ">> 0" ) bitAnd: 16r3F ) ) ]
					ifTrue: 
						[ Array	"1110dddd 10dddddd 10dddddd"
							with: ( 16rE0 bitOr: ( ( cv >> 12 ) bitAnd: 16r0F ) )
							with: ( 16r80 bitOr: ( ( cv >> 6 ) bitAnd: 16r3F ) )
							with: ( 16r80 bitOr: ( ( cv ">> 0" ) bitAnd: 16r3F ) ) ] ].
		( utflength := utflength + bytes size ) > 65535
			ifTrue: [ self error: 'string too long' ].
		stream nextPutAll: bytes ].
	stream skip: (utflength + 2) negated.
	self storeShort: utflength.
	stream skip: utflength!

stream

	"Answer the value of stream"

	^stream!

stream: aValue

	"Set the value of stream"

	stream := aValue!

writeArray: anArray 

	self writeCollection: anArray ofType: TArray!

writeBehavior: aClass

	stream nextPut: TBehavior.  
	self storeSymbol: aClass name.!

writeBoolean: aBoolean

	self storeBoolean: aBoolean!

writeByte: aByteInteger

	stream nextPut: TByte.
	stream nextPut: aByteInteger!

writeByteArray: aBArray
	stream nextPut: TByteArray.
	self writeInteger: aBArray size.
	stream nextPutAll: aBArray!

writeByteString: aString 

	stream nextPut: TByteString.
	self writeInteger: aString size.
	aString do:[ :ch | stream nextPut: ch asInteger ].!

writeCharacter: aCharacter

	stream nextPut: TChar. 
	self writeInteger: aCharacter asInteger!

writeCollection: aCollection ofType: aCode

	stream nextPut: aCode.
	self writeInteger: aCollection size.
	aCollection do: [:each | each phiWriteWith: self]!

writeDate: aDate

	stream nextPut: TDate. 
	self storeDate: aDate.!

writeFloat: aFloat 

	stream nextPut: TFloat.
	stream nextPut: aFloat basicSize.
	aFloat basicSize 
		to: 1 
		by: -1 
		do: [ :i | stream nextPut: ( aFloat basicAt: i ) ]!

writeFraction: aFraction

	stream nextPut: TFraction. 
	self writeInteger: aFraction numerator.
	self writeInteger: aFraction denominator!

writeHeaderOn: aStream
	aStream 
		nextPutAll: 'BOA'
		; nextPut: version!

writeIEEEFloat: aFloat

	"The largest positive number that can be stored with single-precision is 
	 1.11111111111111111111111 * (2**(127)) = 3.402823...*(10**(38)).
	The smallest positive numer that can be stored with single-precision is
	1.00000000000000000000000 * (2**(-126)) = 1.157494...*(10**(-38))."

	| absFloat |
	aFloat = 0 "special case"
		ifTrue: 
			[ stream nextPut:TFloat. 
			^stream nextPutAll: #[0 0 0 0] ].
	absFloat := aFloat abs.
	( absFloat < MinFloat or:[absFloat > MaxFloat] )
		ifTrue: 
			[ stream nextPut: TDouble. 
			self storeDouble: aFloat ]
		ifFalse: 
			[ stream nextPut:TFloat.
			self storeFloat: aFloat ].!

writeInteger: anInteger 

	anInteger = 0 ifTrue: [ ^self writeZero ].
	anInteger > 0
		ifTrue:[	anInteger <= 16rFF
						ifTrue: 
							[stream nextPut: TByte.
							 ^stream nextPut: anInteger ].
					anInteger <= 16rFFFF
						ifTrue: 
							[stream nextPut: TShort.
							 ^self storeShort: anInteger ].
					anInteger <= 16r7FFFFFFF
						ifTrue: 
							[stream nextPut: TInt.
							^self storeInt: anInteger ].
					anInteger <= 16r7FFFFFFFFFFFFFFF
						ifTrue: 
							[stream nextPut: TLong. 
							^self storeLong: anInteger ]
					]
		ifFalse:[anInteger <= 16r80000000
						ifTrue: [stream nextPut: TInt.
									^self storeInt: (16r100000000+anInteger) ]
						ifFalse: [stream nextPut: TLong. 
										^self storeLong: (16r10000000000000000+anInteger) ]
					].!

writeKeyedCollection: aKeyedCollection

	stream nextPut: TKeyedCollection.
	self storeSymbol: aKeyedCollection class name.
	self writeInteger: aKeyedCollection size.
	aKeyedCollection keysAndValuesDo:[ :k :v |
		k phiWriteWith: self.
		v phiWriteWith: self ]!

writeNil

	stream nextPut: TNull !

writeNumber: aNumber

	"On default, first convert it to a float"

	aNumber = 0 ifTrue: [ ^self writeZero ].
	self writeFloat: aNumber asFloat!

writeObject: anObject on: aStream

	stream := aStream.
	anObject phiWriteWith: self!

writeObjectByReference: anObject

	| index |
	stream nextPut: TObjectByReference.
	index := referenced at: anObject ifAbsent:[].
	index isNil
		ifTrue: 
			[referenced at: anObject put: self newReferenceIndex.
			self writeObjectByValue: anObject]
		ifFalse:
			[self writeInteger: index]!

writeObjectByValue: anObject

	|  instVarSize |
	stream nextPut: TObject.
	instVarSize := anObject class instSize.
	self storeSymbol: anObject class name.
	stream nextPut: anObject basicSize.
	1 to: anObject basicSize do:[ :index |
		(anObject basicAt: index) phiWriteWith: self ].
	1 to: instVarSize do:[ :index |
		(anObject instVarAt: index) phiWriteWith: self ].!

writeOrderedCollection: anOrderedCollection

	self writeCollection: anOrderedCollection ofType: TOrderedCollection!

writePoint: aPoint

	stream nextPut: TPoint.
	aPoint x phiWriteWith: self.
	aPoint y phiWriteWith: self.!

writeRectangle: aRectangle

	stream nextPut: TRectangle.
	aRectangle left phiWriteWith: self.
	aRectangle right phiWriteWith: self.
	aRectangle top phiWriteWith: self.
	aRectangle bottom phiWriteWith: self.	!

writeSequenceableCollection: aCollection

	self writeArray: aCollection!

writeSet: aSet

	stream nextPut: TSet.
	self storeArray: aSet asArray!

writeSortedCollection: anSortedCollection

	self writeCollection: anSortedCollection ofType: TSortedCollection!

writeSymbol: aSymbol

	stream nextPut: TSymbol. 
	self storeSymbol: aSymbol!

writeTime: aTime

	stream nextPut: TTime. 
	self writeInteger: aTime asMilliseconds!

writeZero
	stream nextPut: TZero! !
	

!OrderedCollection publicMethods !

phiWriteWith: aWriter

	aWriter writeOrderedCollection: self! !
	

!Set publicMethods !

phiWriteWith: aWriter
	aWriter writeSet: self! !
	

!Number class publicMethods !

phiReadWith: aReader

	^aReader readNumber! !

!Number publicMethods !

phiWriteWith: aWriter

	aWriter writeNumber: self! !
	

!Integer class publicMethods !

elcReadWith: aReader
	^aReader readInteger
	! !

!Integer publicMethods !

fasterFactorial

	| factor product |
	factor := self - 1.
	product := self.
	[factor ~= 1]
		whileTrue:
			[ product := factor * product .
			factor := factor - 1].
	^product
	!

phiWriteWith: aWriter

	aWriter writeInteger: self! !
	

!Float class publicMethods !

elcReadWith: aReader
	^aReader readFloat! !

!Float publicMethods !

phiWriteWith: aWriter
	aWriter writeFloat: self! !
	

!Fraction publicMethods !

phiWriteWith: aWriter
	aWriter writeFraction: self! !
	

!SortedCollection class publicMethods !

fromSortedArray: aCollection
	"Added by RUTK -- see #representBinaryOn:."

	| newCollection |
	newCollection := self new: aCollection size.
	newCollection addAll: aCollection.
	^newCollection! !

!SortedCollection publicMethods !

phiWriteWith: aWriter

	aWriter writeSortedCollection: self! !
	

!KeyedCollection publicMethods !

phiWriteWith: aWriter
	aWriter writeKeyedCollection: self! !
	

!ByteArray publicMethods !

phiWriteWith: aWriter
	aWriter writeByteArray: self! !
	

!SequenceableCollection class publicMethods !

phiReadWith: aReader

	^aReader readSequenceableCollectionWithClass: self! !

!SequenceableCollection publicMethods !

phiWriteWith: aWriter

	aWriter writeSequenceableCollection: self! !
	

!Time publicMethods !

phiWriteWith: aWriter
	aWriter writeTime: self! !
	

!String class publicMethods !

phiReadWith: aWriter

	^aWriter readByteString! !

!String publicMethods !

phiWriteWith: aWriter

	aWriter writeByteString: self! !
	

!Symbol class publicMethods !

phiReadWith: aReader

	^aReader readSymbol
		! !

!Symbol publicMethods !

phiWriteWith: aWriter

	aWriter writeSymbol: self! !
	

!Date publicMethods !

phiWriteWith: aWriter
	aWriter writeDate: self! !
	

!Character class publicMethods !

phiReadWith: aReader
	^aReader readCharacter! !

!Character publicMethods !

phiWriteWith: aWriter

	aWriter writeCharacter: self! !

