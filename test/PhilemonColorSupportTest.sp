"Philemon SmalltalkMT exporter has done all the work
Version PhilemonColorSupportTest [552] EM 2-0819"
PROJECTNAME PhilemonColorSupportTest .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonColorSupport .
POOLS .

PROFILE
BEGIN
END

CLASS ColorSupportTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	testColorConstants
	testModulo16At
	testFromHTML
END

! !

TestCase subclass: #ColorSupportTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!ColorSupportTest publicMethods !

testColorConstants
	#(blue black beige coral) do:[:e | Color perform: e ]!

testFromHTML

	self assert: (Color fromHTML: '#FF0000') = Color red.
	self assert: (Color fromHTML: '#0000FF') = Color blue.	!

testModulo16At

	self assert: (Color modulo16At: 1) = Color blue! !

