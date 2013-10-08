module UnitTests
open Xunit

[<Fact>]
let TestSimpleFile () =
    let testData = """
#url: http://localhost:9800

#test: abc

#request: resource/{id}/{user}
#parameter.id: 1
#parameter.id.description: id is not defined
#parameter.user: george
	 
#header.content-type: text/json
#header.Accept-Encoding: gzip, deflate
#header.Accept-Language: en-GB 
	 
#method: post

#body:
{
	"firstname":"test"	
}

#assert.deepEquals: 'abc'
'{
	"firstname":"test"	
}'
 
#assert.equals: 'abc' 'dsdsd'
#assert.exists: 'abc'
    """
    
    let testData1 = """
# GLOBAL
url: http://localhost:9800

# RESOURCE document
-----------------------------

## URI POST resource/{id}/{user}
-----------------------------

## DESCRIPTION   
Creates a new index

## NOTE
Use with care

## STATUSCODES
200 : OK
400	: Invalid word supplied
  
## INFORMATION
Response Formats: XML JSON JSV CSV X-MSGPACK X-PROTOBUF SOAP 1.1 SOAP 1.2
HTTP Methods: POST
Minimum API version: v1.0
Resource URL: http://localhost:9800/index/create

## PARAMS
OpenIndex (false) open the newly created index 

### EXAMPLE Create a simple index
--------------------------------------
 
### DESCRIPTION 
Request to create a simple index without any fields. The newly created index will be offline as the OpenIndex parameter is set to false. An index has to be opened after creation to enable indexing. There are various parameters which can be set while creating an index. Let’s start with adding two simple fields to the index called ‘firstname’ & ‘lastname’. All field names should be lower case and should not contain any spaces. This is to avoid case based mismatching on field names.

### NOTE
It does not create the same service twice

### PARAMETERS
id: 1
user: george

### HEADERS
content-type: text/json
Accept-Encoding: gzip, deflate
Accept-Language: en-GB 

### BODY
{
	"firstname":"test"	
}

### ASSERTS
deepEquals: 'abc' '{ "firstname":"test"	}'
equals: 'abc' 'dsdsd'
exists: 'abc'
    """
    ()

   // TestRunner.ParseData(testData1);

