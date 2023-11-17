/**  With Scala 2.13 on Lab machines:

 * In normal circumstances the CLASSPATH is already set for you:

fsc TestTest.scala
scala org.scalatest.run TestTest

 * If you use jar files in your own space:

   With Scala 2.12 and ScalaTest 3.2.2:
fsc -cp ./scalatest-app_2.12-3.2.2.jar TestTest.scala
scala -cp ./scalatest-app_2.12-3.2.2.jar org.scalatest.run TestTest

Note for myself:
using scala 2.13, use
fsc -cp ./scalatest-app_2.13-3.2.2.jar TestTest.scala
scala -cp /scalatest-app_2.13-3.2.2.jar; org.scalatest.run TestTest

 * (Once this is working you can set your CLASSPATH in .bashrc) 

*/
import org.scalatest.funsuite.AnyFunSuite
// or import org.scalatest.FunSuite with
// ScalaTest 3.0 or earlier

class TestTest extends AnyFunSuite{ // FunSuite in ScalaTest 3.0
  test("simple functions"){
    var t = new IntSet
    t.add(2)
    assert(t.size === 1)
    assert(t.any === 2)
    assert(t.remove(4) === false)
    assert(t.contains(3) === false)
    assert(t.contains(2) === true)
    t.add(2)
    assert(t.contains(2) === true)
    assert(t.size === 1)
    assert(t.remove(2) === true)
    assert(t.contains(2) === false)    
  }
  test("simple functions 2") {
    var t = IntSet(2,3,4,1)
    assert(t.any === 1)
    assert(t.size === 4)
    t.add(12)
    assert(t.remove(2) === true)
    assert(t.any === 1)
    assert(t.remove(1) === true)
    assert(t.any === 3)
    assert(t.size === 3)
  }
  test("for tostring") {
    var t = IntSet(5,3,1)
    assert(t.toString === "{1, 3, 5}")
    t.add(4)
    assert(t.toString === "{1, 3, 4, 5}")
    t.remove(6)
    assert(t.toString === "{1, 3, 4, 5}")
    t.add(3)
    assert(t.toString === "{1, 3, 4, 5}")
    t.remove(5)
    assert(t.toString === "{1, 3, 4}")
    t.add(2)
    assert(t.toString === "{1, 2, 3, 4}")
    t.add(7)
    assert(t.toString === "{1, 2, 3, 4, 7}")
  }
  test("test for equality and subset") {
    var t1 = IntSet(1,3,5,7)
    var t2 = IntSet(2,4,6,8)
    var t3 = IntSet(1,3,7)
    assert(t1.subsetOf(t3) === true)
    assert(t1.subsetOf(t2) === false)
    assert(t1.subsetOf(t1) === true)
    assert(t2.subsetOf(t3) === false)
    assert(t2.subsetOf(t2) === true)
    assert(t2.subsetOf(t1) === false)
    assert(t3.subsetOf(t3) === true)
    assert(t3.subsetOf(t2) === false)
    assert(t3.subsetOf(t1) === false)
    t3.add(5)
    assert(t1.equals(t3) === true)
    assert(t3.equals(t1) === true)
    assert(t2.equals(t1) === false)
    assert(t3.equals(t2) === t2.equals(t3))
  }
  test("test for intersection and union") {
    var t1 = IntSet(1,2,3,4,5)
    var t2 = IntSet(4,5,6,8,7)
    assert(t1.union(t2) === t2.union(t1))
    assert(t1.intersect(t2) === t2.intersect(t1))
    assert(t1.intersect(t2).toString === "{4, 5}")
    var t3 = IntSet(2)
    assert(t1.union(t2) === t1)
    assert(t3.union(t2).toString === "{2, 4, 5, 6, 7, 8}")
    assert(t3.intersect(t2).toString === "{}")  
  }
  test("test for map and filter") {
    var t = IntSet(1,2,3,8,9,11)
    assert(t.map((a:Int) => a+a).toString === "{2, 4, 6, 16, 18, 22}")
    assert(t.map((a:Int) => -a).toString === "{-11, -9, -8, -3, -2, -1}")
    assert(t.map((a:Int) => a%3).toString === "{0, 1, 2}")
    assert(t.map((a:Int) => (3*a)%7).toString === "{2, 3, 5, 6}")

    assert(t.filter((a:Int) => (a == 1))).toString == "{1}"
    assert(t.filter((a:Int) => (a == 0))).toString == "{}"
    assert(t.filter((a:Int) => (a%2 == 0)).toString == "{2, 8}")
    assert(t.filter((a:Int) => (a%2 != 0)).toString == "{1, 3, 9, 11}")
  }
}
