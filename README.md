SHAPE
=====

Delphi Custom Message Serializer (Marshaller) and Queue, works on both Delphi and Free Pascal

--------------------------------------------------
| Version | Author      | Date  |
----------|-------------|---------------------------|
| Version 1 | William Yang        | 2010-11-11    |
| Version 1.1 | William Yang        | 2010-12-13   |


***Table of Contents***


* Message Format
* Encoding Option Table
* Element Type ID Table
* Descriptor File
* Encoding Example

 
***Message Format***

Message Frame = [Message Size][Encoding Option][Element]…[Element]

Message Size = WORD

Encoding Option = BYTE



***Encoding Option Table***


--------------------------------------------------
|Option Bit |  Comment                           |
--------------------------------------------------
|0000 0001  | ZLIB Compressed                    |
--------------------------------------------------
|0000 0010  | Encrypted                          |
--------------------------------------------------
|0000 0100  | Named Index, Named Index Prepended to all Element values |
--------------------------------------------------


Element = [Element Type][*Element Size][Element Binary]

Element Type = Byte

Element Size = Word

Element Binary = \00..\FF * Element Size



Element Type ID Table
---------------------


Element Type ID Table

Element Type ID (Hex)
  

Element Type Name
	

Has Element Size

FF
	

Empty Element
	

No

00
	

Boolean (False)
	

No

01
	

Boolean (True)
	

No

02
	

Int16
	

No

03
	

Int/Int32
	

No

04
	

Int64
	

No

05
	

UInt16
	

No

07
	

UInt32
	

No

08
	

UInt64
	

No

09
	

Single (4B)
	

No

0A
	

Double (8B)
	

No

10
	

Str/AnsiString (Null Terminated)
	

No

11
	

Bin/Binary
	

Yes

12
	

UTF8
	

Yes

13
	

UTF16
	

Yes

 
	

 
	

 

20
	

Named Index (String)
	

Yes

 

Routing Information

40
	

Command - AnsiString
	

No

41
	

Destination - AnsiString
	

No

Special IDs

50
	

Message ID – AnsiString * 64
	

No

51
	

Message Timestamp INT32
	

No

52
	

Session ID – AnsiString * 64
	

No
| Element Type ID (Hex) | Element Type Name | Has Element Size |

FF
	

Empty Element
	

No

00
	

Boolean (False)
	

No

01
	

Boolean (True)
	

No

02
	

Int16
	

No

03
	

Int/Int32
	

No

04
	

Int64
	

No

05
	

UInt16
	

No

07
	

UInt32
	

No

08
	

UInt64
	

No

09
	

Single (4B)
	

No

0A
	

Double (8B)
	

No

10
	

Str/AnsiString (Null Terminated)
	

No

11
	

Bin/Binary
	

Yes

12
	

UTF8
	

Yes

13
	

UTF16
	

Yes

 
	

 
	

 

20
	

Named Index (String)
	

Yes

 

Routing Information

40
	

Command - AnsiString
	

No

41
	

Destination - AnsiString
	

No

Special IDs

50
	

Message ID – AnsiString * 64
	

No

51
	

Message Timestamp INT32
	

No

52
	

Session ID – AnsiString * 64
	

No
