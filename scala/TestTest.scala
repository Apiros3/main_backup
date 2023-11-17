/**  With Scala 2.12 and ScalaTest 3.2.2:
fsc -cp ./scalatest-app_2.12-3.2.2.jar TestTest.scala
scala -cp ./scalatest-app_2.12-3.2.2.jar org.scalatest.run TestTest
*/
import org.scalatest.funsuite.AnyFunSuite
// or import org.scalatest.FunSuite with
// ScalaTest 3.0 or earlier


// class TestTest extends AnyFunSuite{ // FunSuite in ScalaTest 3.0
//   var x = 0
//   test("x=0"){ assert(x===0) }
//   x = 1
//   test("x=1"){ assert(x===1) }
// }


//  Corrected:
class TestTest extends AnyFunSuite{
  var x = 0
  test("x=0"){ 
    x=0
    assert(x===0) 
  }
  test("x=1"){ 
    x=1
    assert(x===1)
    x=2
    assert(x===2)
  }
}
