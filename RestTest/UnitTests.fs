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
    TestRunner.RunTest(testData);

