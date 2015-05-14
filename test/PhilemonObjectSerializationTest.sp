"Philemon SmalltalkMT exporter has done all the work
Version PhilemonObjectSerializationTest EM 4-0410"
PROJECTNAME PhilemonObjectSerializationTest .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonObjectSerialization .
POOLS .

PROFILE
BEGIN
END

CLASS ExampleObjectStoredByReference
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	phiWriteWith:
	name:
	other
	name
	other:
END

CLASS BinaryObjectAccessorTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	testFraction
	testNil
	testBehavior
	testPoint
	testSet2
	testByteArray
	testMessage
	doBasicTestOn:
	testDictionary
	testSorted
	testBoolean2
	testSortedCollection
	testTime
	testStoreByReference
	testDate
	testZero
	testDictionary2
	testArray
	testAll
	withSerialized:do:
	testSymbol
	testFloat
	testRectangle
	testSet
	testBoolean
	performanceManyPoints
END

CLASS BOAInterchangeTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	testZero_java
	testInteger
	javaClassFile
	testNull_java
	testDictionary_java
	testFalse_java
	testChar
	testNull
	inputDir
	testZero
	testRectangle_java
	testArray_java
	outputDir
	testFalse
	testFloat_java
	testChar_java
	testTrue_java
	testDictionary
	testRectangle
	testInteger_java
	javaHeader
	testArray
	testString
	testTrue
	makeJava
	testFloat
	doObject:name:
	testString_java
END

CLASS VASTWriteJavaReadTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
END

CLASS VASTReadJavaWriteTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
END

! !

Object subclass: #ExampleObjectStoredByReference
    instanceVariableNames: 'other name '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TestCase subclass: #BinaryObjectAccessorTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TestCase subclass: #BOAInterchangeTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
BOAInterchangeTest subclass: #VASTWriteJavaReadTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
BOAInterchangeTest subclass: #VASTReadJavaWriteTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!ExampleObjectStoredByReference publicMethods !

name
	^name!

name: anObject
	name := anObject!

other
	^other!

other: anObject
	other := anObject!

phiWriteWith: aWriter
	aWriter writeObjectByReference: self! !
	

!BinaryObjectAccessorTest publicMethods !

doBasicTestOn: anObject 
	self withSerialized: anObject
		do: 
			[:result | 
			self assert: result class == anObject class.
			self assert: result = anObject]!

performanceManyPoints
	"TimeProfiler profile:[self new performanceManyPoints]"

	| points |
	points := (1 to: 10000) collect:[ :i | i @ i ].
	self doBasicTestOn: points!

testAll
	"self new testAll"

	| d copy | 
	(d := Dictionary new: 100)
		at: 'point' put: 1@$a;
		at: 'behavior' put: Class;
		at: 'bytearray' put: #[1 2 3 4];
		at: 'string' put: (String abtFromBytes: (0 to: 255) asByteArray );
		at: 'boolean' put: true;
		at: 'character' put: $$;
		at: 'date' put: Date today;
		at: 'float' put: 0.0;
		at: 'fraction' put: 1 / 2;
		at: 'integer' put: 123456789;
		at: 'message' put: (Message new selector: #halt);
		at: 'ordered' put: (OrderedCollection with: 1 with: 'abc' with: nil);
		at: 'sorted' put: (SortedCollection with: 7 with: 3 with: 99);
		at: 'array' put: #(1 2 3 4);
		at: 'set'  put: (Set with: Object);
		at: 'symbol' put: #help;
		at: 'time' put: Time now;
		at: 'nil' put: #(nil).
	copy := BinaryObjectAccessor objectFromByteArray: (BinaryObjectAccessor byteArrayFromObject: d).
	copy keysAndValuesDo: [ :k :v |	
		self assert: (d at: k ifAbsent: []) class = v class]!

testArray

	self doBasicTestOn: #(1).!

testBehavior

	self doBasicTestOn: Class.!

testBoolean

	self doBasicTestOn: true.!

testBoolean2

	self doBasicTestOn: false.!

testByteArray

	self doBasicTestOn: #[1 2 3 4].!

testDate

	self doBasicTestOn: Date today.!

testDictionary

	self withSerialized: Dictionary new do: [ :r | r class = Dictionary & r isEmpty ]!

testDictionary2

	| d |
	d := Dictionary new.
	d at: 1 put: 2.
	self withSerialized: d do: [ :r | r class = Dictionary & (r at: 1) = 2]!

testFloat

	self doBasicTestOn: 0.2.!

testFraction

	self doBasicTestOn: 1/800.!

testMessage

	| m |
	m := (Message selector: #at: arguments: #(12) ).
	self withSerialized: m do: [ :r | r class = Message & (r selector = #at:) ]!

testNil

	self doBasicTestOn: nil.!

testPoint

	self doBasicTestOn: 1@$a.!

testRectangle

	self doBasicTestOn: (1@2 corner: 3@4).!

testSet

	self withSerialized: Set new do: [ :r | r class = Set & r isEmpty ]!

testSet2

	self withSerialized: (Set with: Object) do: [ :r | r class = Set & (r includes: Object) ]!

testSorted

	self doBasicTestOn: (SortedCollection with: 7 with: 3 with: 99).!

testSortedCollection
	"self new testSortedCollection"

	| fix |
	fix := SortedCollection new.
	self withSerialized: fix do: [:result | 
			self assert: result class == fix class.
			self assert: result asArray = fix asArray].!

testStoreByReference

	| ex1 ex2 boa1 boa2 |
	ex1 := ExampleObjectStoredByReference new.
	ex1 name: 'ex1'.
	ex2 := ExampleObjectStoredByReference new.
	ex2 name: 'ex2'.
	ex1 other: ex2.
	ex2 other: ex1.
	boa1 := BinaryObjectAccessor 
		objectFromByteArray: (BinaryObjectAccessor byteArrayFromObject: ex1).
	boa2 := boa1 other.
	self assert: boa1 name = 'ex1'.
	self assert: boa2 name = 'ex2'.
	self assert: boa2 other = boa1.
	self assert: boa1 other = boa2.!

testSymbol

	self doBasicTestOn: #help.!

testTime

	self doBasicTestOn: Time now!

testZero

	self doBasicTestOn: 0.!

withSerialized: anObject do: oneArgBlock

	| result |
	result := BinaryObjectAccessor objectFromByteArray: (BinaryObjectAccessor byteArrayFromObject: anObject).
	oneArgBlock value: result! !
	

!BOAInterchangeTest publicMethods !

doObject: anObject name: aTestName

	| out bin  newObject|
	out := BinaryObjectAccessor byteArrayFromObject: anObject.
	( ( self outputDir , aTestName , '.boa' ) asFilename writeStream )  nextPutAll: out ; close.
	bin := ( self inputDir , aTestName , '.boa' ) asFilename readContents asByteArray.
	newObject := BinaryObjectAccessor objectFromByteArray: bin.
	self assert: anObject = newObject!

inputDir
	^'c:\temp\vast\'!

javaClassFile
	^'BOAInterchangeTest.java'!

javaHeader
	
	^'
package com.philemonworks.boa.test;
// This class was generated from VAST>>BOAInterchangeTest>>makeJava
import junit.*;
import com.philemonworks.boa.*;
import java.io.*;
import java.util.*;
import java.awt.Rectangle;

public class BOAInterchangeTest extends AbstractBOAInterchangeTest {
'.!

makeJava
	"DoitTool open: 'BOAInterchangeTest new makeJava' 
	"
	
	| out |
	out := (self outputDir , self javaClassFile) asFilename writeStream.
	out nextPutAll: self javaHeader.
	self class selectors do:[:e | 
		('*_java' match: e)
			ifTrue:[ out nextPutAll: (self perform: e) ]].
	out nextPutAll: '}'.
	out close.
!

outputDir
	^'c:\temp\vast\'!

testArray

	self doObject: #(1 2 3 4) name: 'testArray'!

testArray_java

	^'
	public void testArray() {
		ArrayList s = new ArrayList();
		s.add(1);
		s.add(2);
		s.add(4);
		s.add(5);
		ByteArrayOutputStream outStream = openOutputStream("testArray.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testArray.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		ArrayList newS = in.readNextIntValue();
		in.close();
		assertEquals(newS.get(0) , 1);
		assertEquals(newS.get(1) , 2);
		assertEquals(newS.get(2) , 3);
		assertEquals(newS.get(3) , 4);						
	}
	'
!

testChar

	self doObject: $a name: 'testChar'!

testChar_java

	^'
	public void testChar() {
		char s = ''a'';
		ByteArrayOutputStream outStream = openOutputStream("testChar.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testChar.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		char newS = in.readNextCharacterValue();
		in.close();
		assertEquals(s, newS);
	}
	'
!

testDictionary

	| d |
	d := Dictionary new.
	d at: 'today' put: '09-04-2004'.
	self doObject: d name: 'testDictionary'!

testDictionary_java

	^'
	public void testDictionary() {
		HashMap s = new HashMap();
		s.put("today","09-04-2004");
		ByteArrayOutputStream outStream = openOutputStream("testDictionary.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testDictionary.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		int newS = in.readNextIntValue();
		in.close();
		assertEquals(s.get("today"), newS.get("today"));
	}
	'
!

testFalse

	self doObject: false name: 'testTrue'!

testFalse_java

	^'
	public void testFalse() {
		boolean s = false;
		ByteArrayOutputStream outStream = openOutputStream("testFalse.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testFalse.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		boolean newS = in.readNextBooleanValue();
		in.close();
		assertEquals(s, newS);
	}
	'
!

testFloat

	self doObject: 3.14 name: 'testFloat'!

testFloat_java

	^'
	public void testFloat() {
		float s = 3.14;
		ByteArrayOutputStream outStream = openOutputStream("testFloat.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testFloat.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		int newS = in.readNextFloatValue();
		in.close();
		assertEquals(s, newS);
	}
	'
!

testInteger

	self doObject: 1234 name: 'testInteger'!

testInteger_java

	^'
	public void testInteger() {
		int s = 1234;
		ByteArrayOutputStream outStream = openOutputStream("testInteger.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testInteger.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		int newS = in.readNextIntValue();
		in.close();
		assertEquals(s, newS);
	}
	'
!

testNull

	self doObject: nil name: 'testNull'!

testNull_java

	^'
	public void testNull() {
		Object s = null;
		ByteArrayOutputStream outStream = openOutputStream("testNull.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testNull.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		Object newS = in.readNext();
		in.close();
		assertEquals(s, newS);
	}
	'
!

testRectangle

	self doObject: (1@2 corner: 3@4) name: 'testRectangle'!

testRectangle_java

	^'
	public void testRectangle() {
		Rectangle s = new Rectangle(1,2,3,4);
		ByteArrayOutputStream outStream = openOutputStream("testRectangle.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testRectangle.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		Rectangle newS = in.readNext();
		in.close();
		assertEquals(s.left, newS.left);
		assertEquals(s.top, newS.top);
		assertEquals(s.right, newS.right);
		assertEquals(s.bottom, newS.left);						
	}
	'
!

testString

	self doObject: 'Hello World' name: 'testString'!

testString_java

	^'
	public void testString() {
		String s = "Hello World";
		ByteArrayOutputStream outStream = openOutputStream("testString.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testString.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		String newS = in.readNextString();
		in.close();
		assertEquals(s, newS);
	}
	'
!

testTrue

	self doObject: true name: 'testTrue'!

testTrue_java

	^'
	public void testTrue() {
		boolean s = true;
		ByteArrayOutputStream outStream = openOutputStream("testTrue.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testTrue.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		boolean newS = in.readNextBooleanValue();
		in.close();
		assertEquals(s, newS);
	}
	'
!

testZero

	self doObject: 0 name: 'testZero'!

testZero_java

	^'
	public void testZero() {
		int s = 0;
		ByteArrayOutputStream outStream = openOutputStream("testZero.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testZero.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		int newS = in.readNextIntValue();
		in.close();
		assertEquals(s, newS);
	}
	'
! !
	
	

