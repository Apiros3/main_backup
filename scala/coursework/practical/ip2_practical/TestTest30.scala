/**     With Scala 2.12 and ScalaTest 3.0.5:
 *This is the version to start with if you are using an older version of ScalaTest.
fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest30.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest30
*/
import org.scalatest.FunSuite
// or import org.scalatest.funsuite.AnyFunSuite with
// ScalaTest 3.1 or later (where you won't need Scalatic)


class TestTest30 extends FunSuite{ // AnyFunSuite in ScalaTest 3.1
  var x = 0
  test("x=0"){ assert(x===0) }
  x = 1
  test("x=1"){ assert(x===1) }
}


/*
//  Corrected:
class TestTest extends FunSuite{
  var x = 0
  test("x=0"){ 
    x=0
    assert(x===0) 
  }
  test("x=1"){ 
    x=1
    assert(x===1)
  }
}
*/  