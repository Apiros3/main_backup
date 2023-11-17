

object rand {

    class A {def show(o:A): String = "A"}
    class B extends A {
        override def show(o:A): String = {super.show(o) + "B"}
        def show(o:B): String = "B"
    }
    class C extends B {
        override def show(o:B): String = {super.show(o) + "C"}
        def show(o:C): String = "C"
    }
    var c : C = new C 
    var b : B = c 
    var a : A = b 

    def main(args: Array[String]) : Unit = {
        println(a.show(a))
        println(a.show(c))
        println(b.show(a))
        println(b.show(c))
        println(c.show(b))
        println(c.show(c))
    }

}