"Philemon SmalltalkMT exporter has done all the work
Version PhilemonTextEmphasisSupport EM 3-0914 + MvH 4-0315"
PROJECTNAME PhilemonTextEmphasisSupport .
PROJECTCATEGORY Philemon .
PREREQUISITES .
POOLS .

PROFILE
BEGIN
END

CLASS CharacterEmphasis
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	forRemoving
END
INSTANCEMETHODS
BEGIN
	removeEmphasisEncoding:
	decodeFromArray:
	addEmphasisEncoding:
	style:
	=
	addCharacterEmphasis:
	foreground:
	removeCharacterEmphasis:
	hyperlink:
	background
	foreground
	hyperlink
	style
	background:
	arrayEncoding
	hasChange
	hash
	printOn:
END

CLASS EmphasizedText
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	string:emphasis:
	new
	fromString:
END
INSTANCEMETHODS
BEGIN
	inspectActions
	emphasisReport
	hyperlinkFrom:to:anchor:
	copyWith:
	emphasis:
	deleteEmphasisFrom:to:
	printOn:
	removeEmphasis:from:to:
	foreground:
	insertString:at:
	checkMergeableAt:
	includes:
	from:to:setBackground:
	copyFrom:to:
	offsetEmphasisEntriesFrom:by:
	background:
	subStringsFrom:emphasisStartingAt:withEmphasisDo:
	removeAllEmphasis
	,
	at:
	string
	asString
	readStream
	emphasis
	isString
	string:
	last
	string:emphasis:
	characterEmphasisAt:
	size
	emphasisAt:
	removeEmphasisFrom:to:
	addEmphasis:from:to:
	asEmphasizedText
	isEmpty
	first
	isSBString
	deleteStringFrom:count:
	removeHyperlinkFrom:to:
	checkInBoundsFrom:to:
	emphasizeFrom:to:with:
	from:to:setForeground:
END

CLASS EmphasizedTextStream
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	appendOn:
END
INSTANCEMETHODS
BEGIN
	space
	nextPut:
	nextPutAll:
	reset
	emphasis:
	emphasis
	cr
	text:
	contents
	text
END

CLASS Array
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	decodeAsEmphasis
END

CLASS String
INCLUDEDEF=0
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	emphasis
	asEmphasizedText
	emphasisAt:
END

! !

Object subclass: #CharacterEmphasis
    instanceVariableNames: 'style hyperlink background foreground '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #EmphasizedText
    instanceVariableNames: 'string emphasis '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #EmphasizedTextStream
    instanceVariableNames: 'emphasis text '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!CharacterEmphasis class publicMethods !

forRemoving
	^self new foreground: 0 ;background: 0 ;style: 0 ;hyperlink: 0! !

!CharacterEmphasis publicMethods !

= aCharacterEmphasis
	^aCharacterEmphasis class = self class
		and:[aCharacterEmphasis style = self style
			and:[aCharacterEmphasis foreground = self foreground
				and:[aCharacterEmphasis background = self background			
					and:[aCharacterEmphasis hyperlink = self hyperlink]]]]!

addCharacterEmphasis: aCharacterEmphasis 
	"overwrite any"

	aCharacterEmphasis foreground ifNotNil: [:c | foreground := c].
	aCharacterEmphasis background ifNotNil: [:c | background := c].
	aCharacterEmphasis style ifNotNil: [:s | style := s].
	aCharacterEmphasis hyperlink ifNotNil: [:h | hyperlink := h]!

addEmphasisEncoding: anArray

	| emphasis |
	emphasis := self class new decodeFromArray: anArray.
	self addCharacterEmphasis: emphasis.
	^anArray!

arrayEncoding
	| out |
	out := WriteStream on: #().
	foreground isNil 
		ifFalse: 
			[out
				nextPut: #foreground;
				nextPut: foreground].
	background isNil 
		ifFalse: 
			[out
				nextPut: #background;
				nextPut: background].
	style isNil 
		ifFalse: 
			[out
				nextPut: #style;
				nextPut: style].
	hyperlink isNil 
		ifFalse: 
			[out
				nextPut: #hyperlink;
				nextPut: hyperlink].
	^out contents!

background
	^background!

background: anObject
	^background := anObject!

decodeFromArray: anArray 
	| in |
	in := anArray readStream.
	[in atEnd] whileFalse: 
			[| token value |
			token := in next.
			value := in next.
			token = #foreground ifTrue: [foreground := value].
			token = #background ifTrue: [background := value].
			token = #style ifTrue: [style := value].
			token = #hyperlink ifTrue: [hyperlink := value]]!

foreground
	^foreground!

foreground: anObject
	foreground := anObject!

hasChange
	^foreground notNil | background notNil | style notNil | hyperlink notNil!

hash
	^(self style hash bitOr: self foreground hash) bitOr: self hyperlink hash!

hyperlink
	^hyperlink!

hyperlink: anObject
	hyperlink := anObject!

printOn: aStream
	aStream nextPutAll: self class name ;nextPutAll: ' new decodeFromArray: '.
	self arrayEncoding printOn: aStream!

removeCharacterEmphasis: aCharacterEmphasis 
	aCharacterEmphasis foreground ifNotNil: [:c | foreground := nil].
	aCharacterEmphasis background ifNotNil: [:c |background := nil].	
	aCharacterEmphasis style ifNotNil: [:s | style := nil].
	aCharacterEmphasis hyperlink ifNotNil: [:h | hyperlink := nil]!

removeEmphasisEncoding: anArray

	| emphasis |
	emphasis := self class new decodeFromArray: anArray.
	self removeCharacterEmphasis: emphasis.
	^anArray!

style
	^style!

style: anObject
	style := anObject! !
	

!EmphasizedText class publicMethods !

fromString: aString
	^self string: aString emphasis: OrderedCollection new!

new
	^self fromString: ''!

string: aString emphasis: anOrderedCollection 
	"Answer an instance of the receiver whose characters are those of the argument, aString"

	^super new string: aString asString emphasis: anOrderedCollection! !

!EmphasizedText publicMethods !

, aString
	^self class string: string , aString asString emphasis: emphasis copy!

addEmphasis: aCharacterEmphasis from: start to: stop 

	| here  entry before overlap after|
	emphasis isEmpty
		ifTrue: [^emphasis add: (Array with: start with: aCharacterEmphasis with: stop )].
	here := 1.
	[entry := emphasis at: here.
	"before"
	(start < entry first and:[stop < entry first]) 
		ifTrue:
			[emphasis 
				insert: (Array with: start  with: aCharacterEmphasis with: stop) 
				atIndex: here.
			^self checkMergeableAt: here ].
	"overlap at end"		
	(start < entry first and:[ stop < entry last]) 
		ifTrue:
			[overlap := Array with: entry first with: ((entry at: 2) copy addCharacterEmphasis: aCharacterEmphasis; yourself) with: stop.
			emphasis at: here put: overlap.
			self addEmphasis: aCharacterEmphasis from:start to: entry first - 1.
			self addEmphasis: (entry at: 2) from: stop + 1 to: entry last.
			^self checkMergeableAt: here - 1].
	"included"
	(start >= entry first and:[ stop <= entry last]) 
		ifTrue: 
			[overlap := Array with: start with: ((entry at: 2) copy addCharacterEmphasis: aCharacterEmphasis; yourself) with: stop.
			emphasis at: here put: overlap.
			start > entry first
				ifTrue: [self addEmphasis: (entry at: 2) from:entry first to: start - 1].
			stop < entry last
				ifTrue:[self addEmphasis: (entry at: 2) from: stop + 1 to: entry last ].
			start = entry first
				ifTrue:[self checkMergeableAt: here - 1].
			stop = entry last
				ifTrue:[self checkMergeableAt: here + 1].
			^self].
	"overlap at begin"
	(start < entry last and:[ stop > entry last])
		ifTrue:
			[ overlap := Array with: start with: ((entry at: 2) copy addCharacterEmphasis: aCharacterEmphasis; yourself) with: entry last.
			emphasis at: here put: overlap.
			self addEmphasis: (entry at: 2) from:entry first to: start - 1.
			^self addEmphasis: aCharacterEmphasis from: entry last + 1 to: stop].
	"after"
	here = emphasis size
		ifTrue:
			[emphasis add: (Array with: start with: aCharacterEmphasis with: stop ).
			^self checkMergeableAt: emphasis size].
	"check next entry"
	here := here + 1] repeat.
		!

asEmphasizedText
	"That's me"!

asString
	"For OSWidget"
	^string!

at: index
	^string at: index!

background: colorName
	self emphasizeFrom: 1 to: self size with: (Array with: #background with: colorName)!

characterEmphasisAt: index

	| here  entry|
	emphasis isEmpty
		ifTrue: [^nil].
	here := 1.
	[ here > emphasis size
		ifTrue: [^nil].
	entry := emphasis at: here.
	index < entry first
		ifTrue: [^nil].
	index <= entry last
		ifTrue: [^entry at: 2].
	here := here + 1] repeat.
	!

checkInBoundsFrom: stop to: start 
	(start between: 1 and: self string size) 
		ifFalse: [self error: 'emphasis start out of bounds'].
	(stop between: 1 and: self string size) 
		ifFalse: [self error: 'emphasis stop out of bounds'].
	start <= stop ifFalse: [self error: 'emphasis start greater than stop']!

checkMergeableAt: index 

	"See if the emphasis at @index matches that
	of (index-1) or (index+1) and try to merge them"

	| here previous next |
	  "allow index out of range"( index between: 1 and: emphasis size ) 
		ifFalse: [ ^self ].
	here := emphasis at: index.
	index > 1 
		ifTrue: 
			[ previous := emphasis at: index - 1. 	"no gap?"
			( previous last + 1 ) = here first 
				ifTrue: [ ( previous at: 2 ) = ( here at: 2 ) 
					ifTrue: 
						[ previous at: 3 put: ( here at: 3 ).
						emphasis removeAtIndex: index.
						^self checkMergeableAt: index - 1 ] ] ].
	index < emphasis size 
		ifTrue: 
			[ next := emphasis at: index + 1. 	"no gap?"
			next first = ( here last + 1 ) 
				ifTrue: [ ( next at: 2 ) = ( here at: 2 ) 
					ifTrue: 
						[ next at: 1 put: ( here at: 1 ).
						emphasis removeAtIndex: index.
						^self checkMergeableAt: index ] ] ]!

copyFrom: start to: stop

	"slow version"
	| text |
	text := (self string copyFrom: start to: stop) asEmphasizedText.
	start to: stop do:[ :i |
		text emphasizeFrom: (i - start + 1) to: (i - start + 1) with: (self emphasisAt: i)].
	^text!

copyWith: aChar
	^self class string: (string asString copyWith: aChar) emphasis: emphasis copy!

deleteEmphasisFrom: start to: stop 

	| state diff |
	state := false.
	diff := stop - start + 1.
	emphasis copy do: [ :each | 
		stop < each first
			ifTrue: 
				[ each at: 1 put: ( each first - diff).
				each at: 3 put: ( each last - diff) ]
			ifFalse: 
				[ start < each first
					ifTrue: 
						[ stop < each last
							ifTrue: 
								[ each at: 1 put: stop + 1.
								each at: 1 put: ( each first - diff ).
								each at: 3 put: each last - diff ]
							ifFalse: 
								[ state := true.
								emphasis remove: each ] ]
					ifFalse: 
						[ stop < each last
							ifTrue: 
								[ state := true.
								each at: 3 put: each last - diff.
								start = each first 
									ifTrue: [ each at: 1 put: stop - diff ] ]
							ifFalse: 
								[ start < each last
									ifTrue: 
										[ state := true.
										start = each first
											ifTrue: [emphasis remove: each]
											ifFalse:[each at: 3 put: start - 1]]
									ifFalse: 
										[ ( start = each first
											and: [ stop >= each last ] ) 
											ifTrue: 
												[ state := true.
												emphasis remove: each ] ] ] ] ] ]!

deleteStringFrom: start count: count
	"Send from OSWidget"
	
	"create new string"
	| here  emp|
	string := (string copyFrom: 1 to: start - 1) ,
		(string copyFrom: start + count  to: string size).
		
	"now update emphasis"
	self deleteEmphasisFrom: start to: start + count - 1!

emphasis
	^emphasis!

emphasis: anObject
	emphasis := anObject!

emphasisAt: index
	"Answer the emphasis or nil for the character at: @index.
	See emphasizeFrom:to:with: for the format of the return value
	"
	| ce |
	(index between: 1 and: self string size)
		ifFalse:[self error: 'emphasis index out of bounds'].
	ce := self characterEmphasisAt: index.
	ce isNil ifTrue: [^nil].
	^ce arrayEncoding
	!

emphasisReport
	| out |
	out := WriteStream on: ''.
	emphasis do: [ :entry | 
		entry first printOn: out.
		out nextPutAll: '..'.
		entry last printOn: out.
		out nextPut: $:.
		( entry at: 2 ) arrayEncoding printOn: out.
		out cr].
	^out contents!

emphasizeFrom: start to: stop with: emphasisEncoding 
	"@emphasisEncoding is an array with key value pairs.
	Key must be one of { foreground, style, hyperlink }
	Value must be { <name of color>, normal | bold | underline, <name of anchor>
	Example:  #(foreground blue style underline hyperlink home)
	"

	| ce |
	emphasisEncoding isNil ifTrue: [^self].
	self checkInBoundsFrom: stop to: start.
	ce := CharacterEmphasis new decodeFromArray: emphasisEncoding.
	self 
		addEmphasis: ce
		from: start
		to: stop!

first
	^string first!

foreground: colorName
	self emphasizeFrom: 1 to: self size with: (Array with: #foreground with: colorName)!

from: start to: stop setBackground: colorName
	self emphasizeFrom: start to: stop with: (Array with: #background with: colorName)!

from: start to: stop setForeground: colorName
	self emphasizeFrom: start to: stop with: (Array with: #foreground with: colorName)!

hyperlinkFrom: start to: stop anchor: anchorName 

	| ce |
	ce := CharacterEmphasis new hyperlink: anchorName.
	self addEmphasis: ce from: start to: stop!

includes: char
	^self string includes: char!

insertString: aString at: index
	"Send from OSWidget"
	
	"create new string"
	| here  emp|
	string := (string copyFrom: 1 to: index - 1),
		aString , (string copyFrom: index to: string size).
		
	"now update emphasis accordingly"
	here := 1.
	[here <= emphasis size]
		whileTrue:
			[ emp := emphasis at: here.
			index < emp first "index before any emphasis"
				ifTrue: [^self].
			index <= emp last "index within here emphasis"
				ifTrue: 
					[ emp at: 3 put: (emp last + aString size).
					"increase indices for remaining emphasis entries"
					self offsetEmphasisEntriesFrom: here + 1 by: aString size.
					^self]
				ifFalse: [ here := here + 1]].
	"append next to last emphasis"
	(emp notNil and:[emp last = (index - 1)])
		ifTrue: [ emp at: 3 put: (emp last + aString size) ].
				
			!

inspectActions
	^#(emphasisReport)!

isEmpty
	^string isEmpty!

isSBString
	"VA specific"
	
	"Answer true if the receiver is a single byte string."
	
	^true
!

isString
	^true!

last
	^self string last!

offsetEmphasisEntriesFrom: start by: howMany 

	start to: emphasis size do: [ :i | 
		( emphasis at: i ) at: 1 put: ( ( emphasis at: i ) at: 1 ) + howMany.
		( emphasis at: i ) at: 3 put: ( ( emphasis at: i ) at: 3 ) + howMany ]!

printOn: aStream
	self string printOn: aStream!

readStream
	^ReadStream on: self!

removeAllEmphasis
	self emphasis: OrderedCollection new!

removeEmphasis: aCharacterEmphasis from: start to: stop 

	| here entry newEmphasis |
	emphasis isEmpty 
		ifTrue: [ ^self ].
	here := 1.
	[ 
		entry := emphasis at: here. 	"before"
		( start < entry first
			and: [ stop < entry first ] ) 
			ifTrue: [ ^self ]. 	"overlap at end"
		( start < entry first
			and: [ stop <= entry last ] ) 
			ifTrue: [ ^self removeEmphasis: aCharacterEmphasis from: entry first to: stop ]. 	"included"
		( start >= entry first
			and: [ stop <= entry last ] ) 
			ifTrue: 
				[ start = entry first 
					ifTrue: 
						[ stop = entry last
							ifTrue: 
								[ newEmphasis := ( entry at: 2 ) copy  removeCharacterEmphasis: aCharacterEmphasis ; yourself.
								newEmphasis hasChange
									ifTrue: 
										[ emphasis at: here put: newEmphasis.
										^self checkMergeableAt: here ]
									ifFalse: 
										[ ^emphasis removeAtIndex: here ] ]
							ifFalse: 
								[ ^entry at: 1 put: stop + 1 ] ]. 	"start > entry first"
				stop < entry last 
					ifTrue: 
						[  | entryStop | 
						entryStop := entry at: 3.
						entry at: 3 put: start - 1.
						newEmphasis := ( entry at: 2 ) copy  removeCharacterEmphasis: aCharacterEmphasis ; yourself.
						newEmphasis hasChange 
							ifTrue: [ self addEmphasis: newEmphasis from: start to: stop ].
						^self addEmphasis: ( entry at: 2 ) from: stop + 1 to: entryStop ].
				stop = entry last 
					ifTrue: [ ^entry at: 3 put: start - 1 ] ]. 
		 	"overlap at begin"
		( start < entry last
			and: [ stop > entry last ] ) 
			ifTrue: 
				[  | entryStop | 
				entryStop := entry at: 3.
				newEmphasis := ( entry at: 2 ) copy  removeCharacterEmphasis: aCharacterEmphasis ; yourself.
				entry at: 3 put: start - 1.
				newEmphasis hasChange 
					ifTrue: [ self addEmphasis: newEmphasis from: start to: entry last ].
				^self removeEmphasis: aCharacterEmphasis from: entryStop + 1 to: stop ]. 
		 	"after"
		here = emphasis size 
			ifTrue: [ ^self ]. 
		 	"check next entry"
		here := here + 1 ] repeat!

removeEmphasisFrom: start to: stop 
	"Removes any emphasis from @start to @stop"
	
	self checkInBoundsFrom: stop to: start.	
	self removeEmphasis: CharacterEmphasis forRemoving from: start to: stop 		!

removeHyperlinkFrom: start to: stop 

	| ce |
	ce := CharacterEmphasis new hyperlink: ''.
	self removeEmphasis: ce from: start to: stop!

size
	^self string size!

string
	^string!

string: aString
	string := aString.
	emphasis := OrderedCollection new!

string: aString emphasis: anOrderedCollection
	string := aString.
	emphasis := anOrderedCollection!

subStringsFrom: aString emphasisStartingAt: startIndex withEmphasisDo: twoArgBlock 
	"Send from OSWidget"
	
	| stopIndex index |
	stopIndex := startIndex + aString size - 1.
	index := startIndex.
	emphasis do: 
			[:entry | 
			entry first <= stopIndex 
				ifTrue: 
					[entry last > index 
						ifTrue: 
							[entry first > index 
								ifTrue: 
									["gap"
									twoArgBlock value: (aString copyFrom: index - startIndex + 1
												to: entry first - startIndex)
										value: nil.
									index := entry first.
									twoArgBlock value: (aString copyFrom: index - startIndex + 1
												to: (entry last min: stopIndex) - startIndex + 1)
										value: (entry at: 2).
									index := entry last + 1]
								ifFalse: 
									[twoArgBlock value: (aString copyFrom: index - startIndex + 1
												to: (entry last min: stopIndex) - startIndex + 1)
										value: (entry at: 2).
									index := entry last + 1]]]].
	index <= stopIndex 
		ifTrue: 
			["after gap?"
			twoArgBlock value: (aString copyFrom: index - startIndex + 1
						to: stopIndex - startIndex + 1)
				value: nil]! !
	

!EmphasizedTextStream class publicMethods !

appendOn: aText
	^self new text: aText asEmphasizedText! !

!EmphasizedTextStream publicMethods !

contents
	^text!

cr
	self nextPut: Character cr ;nextPut: Character lf!

emphasis
	^emphasis!

emphasis: anObject
	emphasis := anObject!

nextPut: char

	text := text , (String with: char).
	text emphasizeFrom: text size to: text size with: emphasis!

nextPutAll: aString

	text := text , aString.
	text emphasizeFrom: text size - aString size + 1 to: text size with: emphasis!

reset
	self text: EmphasizedText new.
	self emphasis: nil!

space
	self nextPut: $ !

text
	^text!

text: anObject
	text := anObject! !
	

!Array publicMethods !

decodeAsEmphasis
	^CharacterEmphasis new decodeFromArray:self! !
	

!String publicMethods !

asEmphasizedText
	^EmphasizedText fromString: self!

emphasis
	"For interface compatibility with EmphasizedText"
	^OrderedCollection new!

emphasisAt: index
	^nil! !

