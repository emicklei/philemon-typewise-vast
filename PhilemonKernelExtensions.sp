"Philemon SmalltalkMT exporter has done all the work
Version PhilemonKernelExtensions EM 40409a"
PROJECTNAME PhilemonKernelExtensions .
PROJECTCATEGORY Philemon .
PREREQUISITES .
POOLS .

PROFILE
BEGIN
END

CLASS Collection
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	contains:
	isSequenceable
	noCheckAt:put:
	noCheckAt:
END

CLASS SequenceableCollection
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	new:withAll:
END
INSTANCEMETHODS
BEGIN
	permutationsDo:
	identityIndexOf:startingAt:ifAbsent:
	writeStream
	readStream
	keysAndValuesDo:
	swap:with:
	do:separatedBy:
	identityIndexOf:
	project:where:
	identityIndexOf:startingAt:
	identityIndexOf:ifAbsent:
	lastIndexOf:
	permutationsStartingAt:do:
END

CLASS Set
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	copyWithout:
END

CLASS Number
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	to:steps:
	arcTanWithDenominator:
	asPoint
	rad
	deg
	isZero
END

CLASS Integer
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	newFromString:
	fromHex:
	fromBytes:
END
INSTANCEMETHODS
BEGIN
	**
	asHex
	asBinary
END

CLASS Float
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	newFromString:
END
INSTANCEMETHODS
BEGIN
END

CLASS String
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	tokensBasedOn:
END

CLASS Symbol
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	newFromString:
END
INSTANCEMETHODS
BEGIN
	value:
END

CLASS ReadStream
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	upToWhile:
	skipUpTo:
END

CLASS WriteStream
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	tab:
	print:
	crtab
	crtab:
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	newFromString:
	fromString:
END
INSTANCEMETHODS
BEGIN
	printOn:indent:
	isSequenceable
END

CLASS Point
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	r:theta:
	zero
END
INSTANCEMETHODS
BEGIN
	theta
	incY:
	centerExtent:
	decY:
	r
	rotatedBy:
	decX:
	incX:
	ceiling
END

CLASS Rectangle
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	areaIntersects:
	includesPoint:
	scaledBy:
	areaExcludesPoint:
	areaIncludesPoint:
END

CLASS ByteArray
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	fromPrintableString:
	decodeFrom:from:startingAt:into:
END
INSTANCEMETHODS
BEGIN
	encodeFrom:into:startingAt:
	asPrintableString
	compressedStoreOn:
END

! !
	

!Collection publicMethods !

contains: aBlock
	"Answer whether at least one elements evaluates <aBlock> to true"

	^(self detect: aBlock ifNone:[nil]) notNil!

isSequenceable
	^true!

noCheckAt: anIndex

	^self at: anIndex!

noCheckAt: anIndex put: anObject

	self at: anIndex put: anObject! !
	

!SequenceableCollection class publicMethods !

new: howMany withAll: anObject

	| newCol |
	newCol := self new: howMany.
	1 to: howMany do:[ :i | newCol at: i put: anObject ].
	^newCol! !

!SequenceableCollection publicMethods !

do: aBlock separatedBy: betweenBlock

	| mySize |
	mySize := self size.
	1 to: mySize do:[ :index |
		aBlock value: (self at: index).
		index < mySize
			ifTrue: [betweenBlock value]]!

identityIndexOf: anElement

	"Answer an Integer which is the first index of within the receiver
	 that is equivalent to the Object anElement.
	 If the receiver does not contain an element that is identical to
	 anElement, answer 0."

	^self identityIndexOf: anElement startingAt: 1!

identityIndexOf: anElement ifAbsent: aBlock

	"Answer an Integer which is the first index of within the receiver
	 that is equivalent to the Object anElement.
	 If the receiver does not contain an element that is identical to
	 anElement, answer 0."

	^self identityIndexOf: anElement startingAt: 1 ifAbsent: aBlock!

identityIndexOf: anElement startingAt: anIndex

	"Answer an Integer which is the first index of within the receiver
	 that is equivalent to the Object anElement.
	 Start the search in the receiver beginning at the Integer anIndex.
	 If the receiver does not contain an element that is identical to
	 anElement, answer 0."

	^self identityIndexOf: anElement startingAt: anIndex ifAbsent: [0]!

identityIndexOf: anElement startingAt: anIndex ifAbsent: exceptionBlock

	"Answer an Integer which is the first index of within the receiver
	 that is equivalent to the Object anElement.
	 Start the search in the receiver beginning at the Integer anIndex.
	 If the receiver does not contain an element that is identical to
	 anElement, answer the result of evaluating the zero argument block,
	 exceptionBlock.

	 Fail if the receiver does not contain anElement and exceptionBlock
	 is not a zero-argument Block."

	self from: anIndex to: self size doWithIndex: [:element :index |
		element == anElement ifTrue: [^index]].

	^exceptionBlock value!

keysAndValuesDo: aBlock
	
	1 to: self size do:[ :index | aBlock value: index value: (self at: index)]!

lastIndexOf: anObject

	| previous next |
	self isEmpty
		ifTrue: [^0].
	previous := self indexOf: anObject startingAt: 1.
	previous = 0
		ifTrue: [^0].
	[next := self indexOf: anObject startingAt: previous + 1.
	next = 0]
		whileFalse:
			[previous := next].
	^previous!

permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."
	"(1 to: 4) asArray permutationsDo: [:each | Transcript cr; show: each printString]"

	self shallowCopy permutationsStartingAt: 1 do: aBlock!

permutationsStartingAt: anInteger do: aBlock
	"#(1 2 3 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	anInteger > self size ifTrue: [^self].
	anInteger = self size ifTrue: [^aBlock value: self].
	anInteger to: self size do:
		[:i | self swap: anInteger with: i.
		self permutationsStartingAt: anInteger + 1 do: aBlock.
		self swap: anInteger with: i]!

project: collectBlock where: selectBlock

	^(self select: selectBlock) collect: collectBlock!

readStream

	^ReadStream on: self!

swap: index1 with:  index2
	"Exchange the objects"
	
	| obj |
	obj := self at: index1.
	self at: index1 put:(self at: index2).
	self at:index2 put: obj!

writeStream

	^WriteStream on: self! !
	

!Set publicMethods !

copyWithout: anObject
	"Answer a new Set without @anObject even if it was not there"
	
	^self copy remove: anObject ifAbsent:[] ; yourself! !
	

!Number publicMethods !

arcTanWithDenominator: denominator 

	"Answer the arctan of the receiver within a range of -pi to pi dependant of the sign of denominator"
	
	| signOfDenominator signOfNominator |
	signOfDenominator := denominator sign.
	signOfNominator := signOfDenominator * ( self sign ). 
	
	signOfDenominator = 0 
		ifTrue:
			[ signOfNominator = 1 
				ifTrue:[ ^Float pi / 2 ].
			^ Float pi / 2 negated ]. 
	
	signOfDenominator = 1
		ifTrue:
			[ ^self arcTan]
		ifFalse:
			[ signOfNominator = -1
				ifTrue:[ ^ self arcTan - Float pi ]
				ifFalse:[ ^ self arcTan + Float pi ] ]!

asPoint

	^self @ self!

deg

	^self radiansToDegrees!

isZero

	^self = 0!

rad

	^self degreesToRadians!

to: aNumber steps: howMany 
	
	^ howMany = 0
		ifTrue:
			[ Array with: self ]
		ifFalse:
			[ howMany = 1
				ifTrue:[ Array with: aNumber ]
				ifFalse:[ self to: aNumber by: aNumber - self / ( howMany - 1 ) ] ]! !
	

!Integer class publicMethods !

fromBytes: aByteArray
	
	"self fromBytes: #[ 255 0 0 255]"
	
	^aByteArray inject: 0 into:[ :int : each | (int bitShift: 8 ) + each ]!

fromHex: hexString

	| value int |
	value := 0.
	hexString asLowercase do:[ :char |
		int := char isDigit
			ifTrue: [ char asInteger - $0 asInteger ]
			ifFalse: [ char asInteger - $a asInteger + 10].
		value := value * 16 + int ].
	^value!

newFromString: aString 

	"pre: no leading separators"

	| input negative num from |
	input := aString.
	( ( negative := aString first == $- )
		or: [ aString first == $+ ] ) 
		ifTrue: [ from := 2 ]
		ifFalse: [from := 1].
	num := 0.
	from to: input size do: [ :i | num := num * 10 + ( input at: i ) asInteger - 48 ].
	^negative
		ifTrue: [ num negated ]
		ifFalse: [ num ]! !

!Integer publicMethods !

** anInteger
	^anInteger power: self!

asBinary
	^self printStringRadix: 2!

asHex
	^self printStringRadix: 16! !
	

!Float class publicMethods !

newFromString: aString 

	"self newFromString: '+1.3e-2'
	self newFromString: '12345'
	self newFromString: '0.01'
	self newFromString: '+9.09'
	"

	| input eIndex factor negative dotIndex num intpart fractionpart fractionfactor |
	input := aString.
	eIndex := ( aString indexOf: $e ) max: ( aString indexOf: $E ).
	eIndex = 0 
		ifFalse: 
			[ input := aString copyFrom: 1 to: eIndex - 1.
			factor := 10 raisedToInteger: ( Integer newFromString: ( aString copyFrom: eIndex + 1 to: aString size ) ) ].
	( ( negative := aString first == $- )
		or: [ aString first == $+ ] ) 
		ifTrue: [ input := input copyFrom: 2 to: input size ].
	( dotIndex := input indexOf: $. ) = 0
		ifTrue: 
			[ num := 0.
			input do: [ :ch | num := num * 10 + ch asInteger - 48 ] ]
		ifFalse: 
			[ intpart := 0.
			1 to: dotIndex - 1 do: [ :i | intpart := intpart * 10 + ( input at: i ) asInteger - 48 ].
			fractionfactor := 1.
			fractionpart := 0.0.
			dotIndex + 1 to: input size do: [ :i | 
				fractionfactor := fractionfactor / 10.0.
				fractionpart := fractionpart + ( ( ( input at: i ) asInteger - 48 ) * fractionfactor ) ].
			num := intpart + fractionpart ].
	eIndex = 0 
		ifFalse: [ num := num * factor ].
	^negative
		ifTrue: [ num negated ]
		ifFalse: [ num ]! !
	

!String publicMethods !

tokensBasedOn: aChar 
	
	"Answer a collection of elements read from the receiver that are separated by 
	<anElement>"

	" 'it is     gonna be a fine day ' tokensBasedOn: (Character value: 32) "
	
	| inStream tokenStream tokens next lastChar |
	inStream := ReadStream on: self.
	tokenStream := WriteStream on: String new.
	tokens := OrderedCollection new.
	lastChar := nil.
	[ inStream atEnd ]
		whileFalse: 
			[ next := inStream next.
			next == aChar
				ifTrue: 
					[ lastChar == aChar
						ifFalse: 
								[tokens add: tokenStream contents.
								tokenStream reset ]]
				ifFalse: [ tokenStream nextPut: next ].
			lastChar := next ].
	lastChar == aChar
		ifFalse: [tokens add: tokenStream contents].
	^tokens! !
	

!Symbol class publicMethods !

newFromString: aString
	^aString asSymbol! !

!Symbol publicMethods !

value: anObject
	^anObject perform: self! !
	

!ReadStream publicMethods !

skipUpTo: anObject
	"Set the receiver's position reference past the next accessible
	 occurence of the argument anObject.  Answer a Boolean which is
	 true if an occurence of the argument anObject is accessible, and
	 false otherwise.  If anObject is not accessible, place the position
	 reference past the last element of the underlying collection."
	
	self skipTo: anObject!

upToWhile: aBlock 

	"answer the next subcollection with all elements
	for which aBlock evaluates to true"

	| out |
	out := WriteStream on: self contentSpecies new.
	[ self atEnd not
		and: [ aBlock value: self peek ] ]
		whileTrue: [ out nextPut: self next ].
	^out contents! !
	

!WriteStream publicMethods !

crtab

	self cr; tab!

crtab: howMany

	self cr ;tab: howMany!

print: anObject
	anObject printOn: self!

tab: howMany

	howMany timesRepeat:[self tab]! !
	

!Object class publicMethods !

fromString: aString
	"Create an instance by reading its state from @aString"
	^self newFromString: aString!

newFromString: aString
	^self subclassResponsibility! !

!Object publicMethods !

isSequenceable
	^false!

printOn: aStream indent: level
	"On default, the receiver ignores indentation"

	^self printOn: aStream! !
	

!Point class publicMethods !

r: radius theta: angleInRadians
	"Create and return an instance representing the point at the given
	radius and angle."

	^(radius * angleInRadians cos) @ (radius * angleInRadians sin)!

zero

	^self x: 0 y: 0! !

!Point publicMethods !

ceiling
	^self class x: self x ceiling y: self y ceiling!

centerExtent: aPoint
	"No Line2D supported yet"

	^self - (aPoint / 2) corner: self + (aPoint / 2)!

decX: delta 

	self x: self x - delta!

decY: delta 

	self y: self y - delta!

incX: delta 

	self x: self x + delta!

incY: delta 

	self y: self y + delta!

r

	"The length of the receiver as a vector"

	^( ( self x ) * ( self x ) + ( ( self y ) * ( self y ) ) ) sqrt!

rotatedBy: radians 

	| sinVal cosVal |
	sinVal := radians negated sin.
	cosVal := radians negated cos.
	^self class x: cosVal * ( self x ) + ( sinVal * ( self y ) ) y: ( cosVal * ( self y ) - ( sinVal * ( self x ) ) )!

theta

	"Answer the angle the receiver makes with origin in radians.   
	right is 0; up is (Pi / 2) radians (90 degrees).  Result will be
	between 0 and (2 * Pi) radians"

	| tan theta pi |
	^self x = 0
		ifTrue: 
			[ pi := Float pi.
			self y >= 0
				ifTrue: [ pi * 0.5 ]
				ifFalse: [ pi * 1.5 ] ]
		ifFalse: 
			[ tan := self y asFloat / self x asFloat.
			theta := tan arcTan.
			self x < 0
				ifTrue: 
					[ theta + theta class pi ]
				ifFalse: 
					[ theta < 0
						ifTrue: [ theta + ( theta class pi * 2 ) ]
						ifFalse: [ theta ] ] ]! !
	

!Rectangle publicMethods !

areaExcludesPoint: aPoint

	"Public - Answer true if aPoint is excluded from the 
	 receiver or any bounding edges, else answer false"

	^aPoint < self origin or: [aPoint > self corner]!

areaIncludesPoint: aPoint

	"Public - Answer true if aPoint is included within the 
	 receiver or all bounding edges, else answer false.
	  
	 Note: Contrast with #containsPoint: .  For the rectangle
		0@0 corner: 10@10 the following points are excluded
		by #containsPoint:  but they are included by areaIncludesPoint: "

	^aPoint >= self origin and: [aPoint <= self corner]!

areaIntersects: operand 

	"Answer true if the receiver intersects the rectangle operand. Otherwise answer false.
	Points on the border are considered on the area"

	| operandCorner operandOrigin |
	operandCorner := operand corner.
	operandOrigin := operand origin.
	^( self corner x min: operandCorner x ) >= ( self origin x max: operandOrigin x )
		and: [ ( self corner y min: operandCorner y ) >= ( self origin y max: operandOrigin y ) ]!

includesPoint: aPoint 

	^aPoint >= self origin
		and: [ aPoint <= self corner ]!

scaledBy: amount 
	"Answer a new object scaled by the argument amount, which can be
	a Point or a scalar value."

	^ self species origin: self origin * amount corner: self corner * amount! !
	

!ByteArray class publicMethods !

fromPrintableString: packedString
	"Decode the receiver from a ByteArray that was a printable String.
	See ByteArray>>asPrintableString for algorithm details. "

	"ByteArray halt; fromPrintableString: '^24FS @a' 
	"

	| size result last resultSize |
	size := packedString size.
	size = 0 ifTrue: [^self new].
	last := packedString last asInteger.
	resultSize := size // 4 * 3.
	last >= 96
		ifTrue:  "not a multiple of 3"
			[resultSize := resultSize - 3 + last - 96].
	result := self new: resultSize.
	self decodeFrom: 1 from: packedString startingAt: 1 into: result.
	^result! !

!ByteArray class privateMethods !

decodeFrom: start from: source startingAt: index into: dest

	| from to w stop |
	to := start.
	from := index.
	stop := dest size.
	" Map groups of 4 source bytes into groups of 3 destination bytes. "
	[to <= stop]
		whileTrue:
			[w := (((source at: from) asInteger bitAnd: 63) bitShift: 18) +
				(((source at: from + 1) asInteger bitAnd: 63) bitShift: 12) +
				(((source at: from + 2) asInteger bitAnd: 63) bitShift: 6) +
				((source at: from + 3) asInteger bitAnd: 63).
			from := from + 4.
			dest at: to put: (w bitShift: -16).
			to < stop ifTrue:
				[dest at: to + 1 put: ((w bitShift: -8) bitAnd: 255).
				to + 1 < stop ifTrue:
					[dest at: to + 2 put: (w bitAnd: 255)]].
			to := to + 3]! !

!ByteArray publicMethods !

asPrintableString
	" Encode the receiver into a printable String.
	We divide up the receiver into 6-bit chunks,
	and turn each one into a character whose code
	is in the range [32..95] (The terminating character
	of the string is in the range [32..98]).   Note
	that the result size is always a multiple of 4."

	"ByteArray fromPrintableString: #[12 34 56 78 90 123] asPrintableString"

	| result |
	result := self class new: self size + 2 // 3 * 4.
	self encodeFrom: 1 into: result startingAt: 1.
	^String fromBytes: result.!

compressedStoreOn: aStream 

	"Write a reduced storeString of the receiver using the #[] notation for small
	byteArray and a compressed string representation for larger ones"

	self size < 50
		ifTrue: 
			[aStream nextPutAll: '#[ '.
			self do: [ :each | 
				aStream print: each ; space].
			aStream nextPut: $]]
		ifFalse: 
			[aStream 
				nextPutAll: '(ByteArray fromPrintableString:' 
				; print: self asPrintableString 
				; nextPut: $)]! !

!ByteArray privateMethods !

encodeFrom: start into: dest startingAt: index

	| from to w stop |
	from := start.
	to := index.
	stop := self size.
	" Map groups of 3 source bytes into groups of 4 destination bytes."
	[from <= stop]
		whileTrue:
			[w := (self at: from) bitShift: 16.
			from < stop ifTrue:
				[w := w + ((self at: from + 1) bitShift: 8).
				from + 1 < stop ifTrue:
					[w := w + (self at: from + 2)]].
			w := w bitXor: 16r820820.  "flip the high bit of each group of 8"
			from := from + 3.
			dest at: to put: (w bitShift: -18) + 32.
			dest at: to + 1 put: ((w bitShift: -12) bitAnd: 63) + 32.
			dest at: to + 2 put: ((w bitShift: -6) bitAnd: 63) + 32.
			dest at: to + 3 put: (w bitAnd: 63) + 32.
			to := to + 4].
	"If the size is not a multiple of 3 bytes, encode the remainder in the last character
	of the result, which is unused in this case. Use a code that is not in the range [32..95]."
	stop \\ 3 = 0 ifFalse:
		[dest at: to - 1 put: stop \\ 3 + 96]! !

