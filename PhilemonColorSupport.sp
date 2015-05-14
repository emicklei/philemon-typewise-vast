"Philemon SmalltalkMT exporter has done all the work
Version PhilemonColorSupport [552] EM 3-0807 + JB 3-0902"
PROJECTNAME PhilemonColorSupport .
PROJECTCATEGORY Philemon .
PREREQUISITES .
POOLS .

PROFILE
BEGIN
END

CLASS Color
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	red
	green
	navy
	modulo16At:
	grey:
	orange
	preDefined
	beige
	coral
	brown
	darkGreen
	darkRed
	nameOrNilFor:
	fromHex:
	distanceSortBlock
	magenta
	blue
	yellow
	integerFromHex:
	fromHTML:
	purple
	nearestNameFor:
	darkgreen
	violet
	rgbSortBlock
	grey
	random
	darkBlue
	red01:green01:blue01:
	cyan
	pink
END
INSTANCEMETHODS
BEGIN
	darker
	transparency
	blue01
	green01
	inspectActions
	uniqueName
	withTransparency:
	mixed:with:
	negated
	lighter
	asHexString
	red01
	kindDo:
	asHTML
	printOn:
END

CLASS TransparentColor
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	transparency
	kindDo:
	uniqueName
	transparency:
END

CLASS ColorCache
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	preDefined
	compileFromRGBFile
	initializePalette16
	release
	palette16
	initialize
	nextWord:
END
INSTANCEMETHODS
BEGIN
END

CLASS RandomColor
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	new
END
INSTANCEMETHODS
BEGIN
	uniqueName
	redByte
	green01
	blue01
	kindDo:
	nextColorComponent255
	blueByte
	name
	transparency
	greenByte
	red01
	next
	nextColorComponent01
	initialize
END

! !

Color subclass: #TransparentColor
    instanceVariableNames: 'transparency '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #ColorCache
    instanceVariableNames: ''
    classVariableNames: 'Palette16 PreDefined '
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #RandomColor
    instanceVariableNames: 'random '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!Color class publicMethods !

beige

	^self preDefined at: #beige ifAbsentPutUsing: [self redByte: 245 greenByte: 245  blueByte: 220]!

blue

	^self preDefined at: #blue ifAbsentPutUsing: [self redByte: 0 greenByte: 0  blueByte: 255]!

brown

	^self preDefined at: #brown ifAbsentPut: [self redByte: 165 greenByte: 42  blueByte: 42]!

coral

	^self preDefined at: #coral ifAbsentPutUsing: [self redByte: 255 greenByte: 127  blueByte: 80]!

cyan

	^self preDefined at: #cyan ifAbsentPutUsing: [self redByte: 0 greenByte: 255  blueByte: 255]!

darkBlue

	^self preDefined at: #darkBlue ifAbsentPutUsing: [self redByte: 0 greenByte: 0  blueByte: 127]!

darkgreen

	^self preDefined at: #darkgreen ifAbsentPutUsing: [self redByte: 0 greenByte: 100  blueByte: 0]!

darkGreen

	^self preDefined at: #darkGreen ifAbsentPutUsing: [self redByte: 0 greenByte: 127  blueByte: 0]!

darkRed

	^self preDefined at: #darkRed ifAbsentPutUsing: [self redByte: 127 greenByte: 0  blueByte: 0]!

distanceSortBlock

	^ [ :c1 :c2 |
		( c1 red squared +  c1 green squared + c1 blue squared ) <
			( c2 red squared + c2 green squared + c2 blue squared) ]!

fromHex: hexString

	"self fromHex:  '986665'
	"
	| red green blue |
	hexString size < 6
		ifTrue: [^self white].
	red := self integerFromHex: (hexString copyFrom: 1 to: 2).
	green := self integerFromHex: (hexString copyFrom: 3 to: 4).
	blue := self integerFromHex: (hexString copyFrom: 5 to: 6).
	^self redByte: red greenByte: green blueByte: blue!

fromHTML: hexString

	"self fromHTML:  '#986665'
	"
	^ hexString first = $#
		ifTrue:[ self fromHex: (hexString copyFrom: 2 to: hexString size)]
		ifFalse:[ self fromHex: hexString ]!

green

	^self preDefined at: #green ifAbsentPutUsing: [self redByte: 0 greenByte: 255  blueByte: 0]!

grey

	^self preDefined at: #grey ifAbsentPutUsing: [self redByte: 192 greenByte: 192  blueByte: 192]!

grey: percentage
	"0 <= percentage <= 100"

	| byte |
	byte := (percentage / 100 * 255) rounded.
	^self redByte: byte  greenByte: byte  blueByte: byte !

integerFromHex: hexString
	"Utility copied from Integer class>>fromHex to prevent dependency"
	"PRE: foreach c in hexString: (c in [$0..$9]) | (c in [$a..$f]) | (c in [$A..$F])"

	^ hexString asUppercase inject: 0 into:[ :hex :char |
		| charInt |
		charInt := char asInteger.
		(hex * 16) + charInt -  (charInt >= $A asInteger
			ifTrue:[ $A asInteger - 10 ]
			ifFalse:[ $0 asInteger ])]!

magenta

	^self preDefined at: #magenta ifAbsentPutUsing: [self redByte: 255 greenByte: 0  blueByte: 255]!

modulo16At: index

	^ColorCache palette16 at: index - 1 \\ 16 + 1!

nameOrNilFor: aColor

	^ColorCache preDefined keyAtValue: aColor!

navy

	^self preDefined at: #navy ifAbsentPutUsing: [self redByte: 0 greenByte: 0  blueByte: 128]!

nearestNameFor: aColor

	| colorDistance colorPoint protoPoint nearest newDistance |
	colorDistance := 65535.
	colorPoint := aColor red @ aColor green @ aColor blue.
	protoPoint := 0@0@0.
	nearest := #white.
	ColorCache preDefined keysAndValuesDo:[ :cName :cColor |
		protoPoint x: cColor red.
		protoPoint y: cColor green.
		protoPoint z: cColor blue.
		(newDistance := (colorPoint dist: protoPoint)) < colorDistance 
			ifTrue:
				[ nearest := cName.
				colorDistance := newDistance ]].
	^nearest
		
		 !

orange

	^self preDefined at: #orange ifAbsentPutUsing: [self redByte: 255 greenByte: 165  blueByte: 0]!

pink

	^self preDefined at: #pink ifAbsentPutUsing: [self red: 65535 green:49344 blue:52171]!

preDefined

	^ColorCache preDefined!

purple
	
	^self preDefined at: #purple ifAbsentPutUsing: [self redByte: 160 greenByte: 32  blueByte: 240]!

random

	^RandomColor new!

red

	^self preDefined at: #red ifAbsentPutUsing: [self redByte: 255 greenByte: 0  blueByte: 0]!

red01: red green01: green blue01: blue

	^self redByte: (red * 256) rounded greenByte: (green * 256) rounded blueByte: (blue * 256) rounded!

rgbSortBlock

	^ [ :c1 :c2 |
		c1 red < c2 red
			and:[ c1 green < c2 green
				and:[ c1 blue < c1 blue ]]]!

violet

	^self preDefined at: #violet ifAbsentPutUsing: [self redByte: 238 greenByte: 130  blueByte: 238]!

yellow

	^self preDefined at: #yellow ifAbsentPutUsing: [self redByte: 255 greenByte: 255  blueByte: 0]! !

!Color publicMethods !

asHexString
	| hexStream |
	hexStream := WriteStream on: (String new: 6).
	hexStream nextPutAll: (self redByte printStringRadix: 16 padTo: 2).
	hexStream nextPutAll: (self greenByte printStringRadix: 16 padTo: 2).
	hexStream nextPutAll: (self blueByte printStringRadix: 16 padTo: 2).
	^hexStream contents!

asHTML
	^'#' , self asHexString!

blue01

	"Answer the blue value scaled to be in range [0..1]"

	^self blueByte / 255.0!

darker
	"Answer a darker shade of this color."

	^ self mixed: 0.8333 with: Color black
!

green01

	"Answer the green value scaled to be in range [0..1]"

	^self greenByte / 255.0!

inspectActions

	^#( asHexString redByte greenByte blueByte uniqueName hue saturation luminance)!

lighter
	"Answer a lighter shade of this color."

	^ self mixed: 0.8333 with: Color white
!

mixed: proportion with: aColor 

	"Answer this color mixed with the given color.
	 The proportion, a number between 0.0 and 1.0, determines what what fraction of the receiver to use in the mix.
	 For example, 0.9 would yield a color close to the receiver.
	"
	"Details: This method uses RGB interpolation; HSV interpolation can lead to surprises.
	"
	
	| frac1 frac2 |
	frac1 := (proportion asFloat min: 1.0) max: 0.0.
	frac2 := 1.0 - frac1.
	^ Color
		redByte: (( self redByte * frac1 ) + ( aColor redByte * frac2 )) rounded
		greenByte: (( self greenByte * frac1 ) + ( aColor greenByte * frac2 )) rounded 
		blueByte: (( self blueByte * frac1 ) + ( aColor blueByte * frac2 )) rounded!

negated
	"Return an RGB inverted color"

	^Color
		redByte: 16rFF - self redByte
		greenByte: 16rFF - self greenByte
		blueByte: 16rFF - self blueByte!

red01

	"Answer the red value scaled to be in range [0..1]"

	^self redByte / 255.0!

transparency
	"For colors, the transparency is none so answer 0"
	
	^0!

uniqueName

	^self asHexString!

withTransparency: zero2one

	^(TransparentColor redByte:self redByte greenByte: self greenByte blueByte: self blueByte) transparency: zero2one! !

!Color privateMethods !

kindDo: aRequestor
	^aRequestor doColor: self!

printOn: aStream 
	
	| nameOrNil |
	nameOrNil := self class nameOrNilFor: self.
	nameOrNil isNil
		ifTrue: 
			[ aStream nextPutAll: 'Color redByte: '
				; print: self redByte
				; nextPutAll: ' greenByte: '
				; print: self greenByte
				; nextPutAll: ' blueByte: '
				; print: self blueByte]
		ifFalse: 
			[ aStream nextPutAll: 'Color ' ; nextPutAll: nameOrNil ]! !
	

!TransparentColor publicMethods !

kindDo: aRequestor

	^ aRequestor doTransparentColor: self!

transparency

	"Answer the value of transparency"

	^transparency!

transparency: aValue

	"Set the value of transparency"

	transparency := aValue!

uniqueName

	^super uniqueName , '_' , (self transparency printString copyWithout: $.)! !
	

!ColorCache class publicMethods !

compileFromRGBFile
"
	|inStream colorName outStream r g b|
	inStream := CfsReadFileStream open: './rgb.txt'.
	[inStream atEnd]
		whileFalse:
			[ r := self nextWord: inStream.
			g := self nextWord: inStream.
			b := self nextWord: inStream.
			colorName := (( inStream nextLine trimSeparators) copyWithout: $ ) asLowercase.
			(self class includesSelector: name asSymbol)
				ifFalse: 
					[ outStream := WriteStream on: (String new: 64).
					outStream nextPutAll: (
'%1

	^PreDefined at: #%1 ifAbsentPut: [self redByte: %2 greenByte: %3  blueByte: %4]' 
			bindWith: colorName with: r with: g with: b ).
					self class
						compile: outStream contents 
						notifying: nil 
						ifNewAddTo: self applications first 
						categorizeIn: #( 'predefined' )
			]]
"!

initialize

	PreDefined := IdentityDictionary new.
	self initializePalette16!

initializePalette16

	"self initializePalette16"
	
	Palette16 := #(
	#blue #red #green #yellow #white #black 	#orange #magenta 
	#grey #brown #darkgreen #cyan #beige #purple #violet #coral) collect: [ :each | Color perform: each ]!

nextWord: aStream
		"Private - Answer a String that is the next sequence of
		alphanumeric characters from aStream.  If none are found,
		answer an empty String."
	| startPosition |
	[aStream atEnd or: [aStream peek isAlphaNumeric]]
		whileFalse: [aStream next].
	startPosition := aStream position.
	[aStream atEnd not and: [aStream peek isAlphaNumeric]]
		whileTrue: [aStream skip: 1].
	^aStream copyFrom: startPosition + 1 to: aStream position!

palette16

	Palette16 isNil
		ifTrue: [ self initializePalette16 ].
	^Palette16!

preDefined

	^PreDefined!

release
	"release the receiver."

	PreDefined := Palette16 := nil! !
	

!RandomColor class publicMethods !

new

	"Answer a new instance of the receiver and initialize it"

	^super new initialize! !

!RandomColor publicMethods !

blue01

	^self nextColorComponent01!

blueByte

	^self nextColorComponent255!

green01

	^self nextColorComponent01!

greenByte

	^self nextColorComponent255!

initialize
	random := Random new!

kindDo: aRequestor
	^aRequestor doColor: self!

name

	^self uniqueName!

next

	^Color
		redByte: self nextColorComponent255
		greenByte: self nextColorComponent255
		blueByte: self nextColorComponent255!

nextColorComponent01

	^random next!

nextColorComponent255

	^(random next * 255) rounded!

red01

	^self nextColorComponent01!

redByte

	^self nextColorComponent255!

transparency
	^0!

uniqueName

	^self blueByte printString! !

