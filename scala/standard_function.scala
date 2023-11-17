import java.awt.Rectangle


object standard {

    // class Shapes {
    //     val area: Int = 60 
    //     final def a : Unit = {}
    // }
    // class Rectangle extends Shapes {
    //     override val area: Int = 100
    //     override def a : Unit = {} 
    // }


    class Rectangle(var width: Int, var height: Int)

    class RecArea(private val dim : Rectangle) {
        private var area = dim.width * dim.height  
        //area = dim.width * dim.height
        def area = dim.width * dim.height 

    }

    def main(args: Array[String]) : Unit = {
        var k = new Rectangle 
        println(k.area)

        // assert(1 == 0)
    }
}