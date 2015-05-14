"Philemon SmalltalkMT exporter has done all the work
Version PhilemonTerminalClientTest EM 4-0919"
PROJECTNAME PhilemonTerminalClientTest .
PROJECTCATEGORY Philemon .
PREREQUISITES PhilemonObjectSerializationTest PhilemonTerminalClient .
POOLS .

PROFILE
BEGIN
END

CLASS BWAInterchangeTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	javaHeader
	testImage_java
	testList
	javaClassFile
	doObject:name:
	testLabel_java
	testInput_java
	testLabel
	testInput
	testList_java
	testButton
	testButton_java
	testImageLabel
END

CLASS TerminalClientTest
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	testCheckBoxAsBinary
	testCounterAsBinary
	testCompositeAsBinary
	testImageLabelAsBinary
	testMessageWithWidgetAsBinary
	testWidgetAppearance
	testInputAsBinary
	testMenuAsBinary
	doTestBinaryWriteRead:
	testListAsBinary
	testColorAsBinary
	testRadioAsBinary
END

! !

BOAInterchangeTest subclass: #BWAInterchangeTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
TestCase subclass: #TerminalClientTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!BWAInterchangeTest publicMethods !

doObject: anObject name: aTestName

	| out bin  newObject|
	out := TerminalWidgetBinaryAccessor byteArrayFromObject: anObject.
	( ( self outputDir , aTestName , '.boa' ) asFilename writeStream )  nextPutAll: out ; close.
	bin := ( self inputDir , aTestName , '.boa' ) asFilename readContents asByteArray.
	newObject := TerminalWidgetBinaryAccessor objectFromByteArray: bin.
	self assert: anObject name = newObject name.
	^newObject!

javaClassFile
	^'BWAInterchangeTest.java'!

javaHeader
	"DoitTool open: 'BWAInterchangeTest new makeJava' 
	"	
	^'
package com.philemonworks.terminal.server.test;
// This class was generated from VAST>>BWAInterchangeTest>>makeJava
import junit.*;
import com.philemonworks.boa.test.*;
import com.philemonworks.terminal.server.*;
import java.io.*;
import java.util.*;
import java.awt.Rectangle;

public class BWAInterchangeTest extends BOAInterchangeTest {
'.!

testButton

	| w x |
	w := TerminalButton in:(1@2 extent: 3@4).
	w name: 'test'.
	x := self doObject: w name: 'testButton'.
	self assert: x label = w label.
	self assert: x alignment = w alignment.!

testButton_java

	^'
	public void testButton() {
		Button w = new Button("test",1,2,3,4);
		ByteArrayOutputStream outStream = openOutputStream("testButton.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testButton.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		Button newW = in.readNext();
		in.close();
		assertNotNull(newW);
	}
	'
!

testImage_java

	^'
	public void testImage() {
		Image w = new Image("test",1,2,3,4);
		ByteArrayOutputStream outStream = openOutputStream("testImage.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testImage.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		Image newW = in.readNext();
		in.close();
		assertNotNull(newW);
	}
	'
!

testImageLabel

	| w |
	w := TerminalImageLabel in:(1@2 extent: 3@4).
	self doObject: w name: 'testImage'!

testInput

	| w |
	w := TerminalInput in:(1@2 extent: 3@4).
	self doObject: w name: 'testInput'!

testInput_java

	^'
	public void testInput() {
		TextField w = new TextField("test",1,2,3,4);
		ByteArrayOutputStream outStream = openOutputStream("testInput.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testInput.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		TextField newW = in.readNext();
		in.close();
		assertNotNull(newW);
	}
	'
!

testLabel

	| w |
	w := TerminalLabel in:(1@2 extent: 3@4).
	self doObject: w name: 'testLabel'!

testLabel_java

	^'
	public void testLabel() {
		Label w = new Label("test",1,2,3,4);
		ByteArrayOutputStream outStream = openOutputStream("testLabel.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testLabel.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		Label newW = in.readNext();
		in.close();
		assertNotNull(newW);
	}
	'
!

testList

	| w |
	w := TerminalList in:(1@2 extent: 3@4).
	self doObject: w name: 'testList'!

testList_java

	^'
	public void testList() {
		List w = new List("test",1,2,3,4);
		ByteArrayOutputStream outStream = openOutputStream("testList.boa");
		BinaryObjectAccessor out = new BinaryObjectAccessor(outStream);
		out.write(s);
		out.close();
		ByteArrayInputStream inStream = openInputStream("testList.boa");		
		BinaryObjectAccessor in = new BinaryObjectAccessor(inStream);
		List newW = in.readNext();
		in.close();
		assertNotNull(newW);
	}
	'
! !
	

!TerminalClientTest publicMethods !

doTestBinaryWriteRead: anObject

	^TerminalWidgetBinaryAccessor objectFromByteArray: (TerminalWidgetBinaryAccessor byteArrayFromObject: anObject)!

testCheckBoxAsBinary

	| box  accessor bin  copy|
	box := TerminalCheckBox new.
	copy := self doTestBinaryWriteRead: box.
	self assert: copy name = box name.
	self assert: copy bounds = box bounds.	
	self assert: copy tickAlignment = box tickAlignment.		
!

testColorAsBinary

	| copy |
	copy := self doTestBinaryWriteRead: Color red.
	self assert: copy = Color red!

testCompositeAsBinary

	| win  accessor bin  copy|
	win := TerminalWidget windowIn: (1@2 corner: 3@4).
	win add: (TerminalWidget inputIn:(1@1 corner: 1@1)).
	copy := self doTestBinaryWriteRead: win.
	self assert: copy name = win name.
	self assert: copy bounds = win bounds.	
	self assert: copy widgets size = 1.
!

testCounterAsBinary
	| counter copy |
	counter := TerminalTimer new.
	counter interval: (2 to: 6 by: 2).
	counter delayInSeconds: 2.
	counter repeat: true.
	copy := self doTestBinaryWriteRead: counter.
	self assert: copy class == counter class
	" All state of counter has become state, not static. 
	self assert: copy interval asArray = counter interval asArray.
	self assert: copy delayInSeconds = counter delayInSeconds.
	self assert: copy repeat = counter repeat"!

testImageLabelAsBinary

	| label  accessor bin  copy|
	label := TerminalImageLabel new.
	label url: 'localhost'.
	label scaleToFit: false.
	copy := self doTestBinaryWriteRead: label.
	self assert: copy name = label name.
	self assert: copy bounds = label bounds.	
"	Following will fail because url and scaleToFit went state instead of static	
	self assert: copy url = label url.
	self assert: copy scaleToFit = label scaleToFit "
!

testInputAsBinary

	| input  accessor bin  copy|
	input := TerminalInput new.
	input name: 'input' ;bounds: (2@3 corner: 3@4).
	input readOnly: true.
	copy := self doTestBinaryWriteRead: input.
	self assert: copy name = input name.
	self assert: copy bounds = input bounds.	
"	Editable has become state, not included right now 
	self assert: copy editable = input editable."!

testListAsBinary

	| list  accessor bin  copy|
	list := TerminalList new.
	copy := self doTestBinaryWriteRead: list.
	self assert: copy name = list name.
	self assert: copy bounds = list bounds.		
!

testMenuAsBinary
	| menu accessor bin copy |
	menu := TerminalMenuList new.
	menu add: 'Choice' message: nil.
	menu addLine.
	copy := self doTestBinaryWriteRead: menu.
	self assert: copy name = menu name.
	self assert: copy bounds = menu bounds.
	self assert: copy items = menu items!

testMessageWithWidgetAsBinary

	| m  copy|
	m := MessageSend
		receiver: 'self'
		selector: 'inspect:'
		arguments: (Array with:TerminalInput new).
	copy := TerminalWidgetBinaryAccessor objectFromByteArray:(
		TerminalWidgetBinaryAccessor byteArrayFromObject: m).
	self assert: copy arguments first class = TerminalInput!

testRadioAsBinary

	| button  accessor bin  copy|
	button := TerminalRadioButton new.
	copy := self doTestBinaryWriteRead: button.
	self assert: copy name = button name.
	self assert: copy bounds = button bounds.	
	self assert: copy groupName = button groupName.		
!

testWidgetAppearance
	| copy |
	copy := self doTestBinaryWriteRead: TWidgetAppearance default copy.
	self assert: copy notNil	"does not mean it is correct"! !

