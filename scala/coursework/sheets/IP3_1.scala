
object IP1 {

    //Question 1
    class Shapes(var height: Int, var width: Int, var major: Int, var minor: Int, isCircle: Boolean) {
        val isCircular = isCircle;
        def update_height(x: Int) : Unit = {
            require(!isCircular);
            height = x;
        }
        def update_width(x: Int) : Unit = {
            require(!isCircular);
            width = x;
        }
        def update_length(x: Int) : Unit = {
            update_height(x);
            update_width(x);
        }
        def update_minor(x: Int) : Unit = {
            require(isCircular && x <= major);
            minor = x;
        }
        def update_major(x: Int) : Unit = {
            require(isCircular && x >= minor);
            major = x;
        }
        def update_radius(x: Int) : Unit = {
            update_major(x);
            update_minor(x);
        }
    } 
    //This class allows all the given operations to be supported, at the cost of a decrease in readability
    def isSquare(args: Array[Shapes]) : Array[Boolean] = {
        var ret = new Array[Boolean](args.size)
        var i = 0;
        for( i <- 0 until args.size) {
            //checks whether each shape is a square or not
            ret(i) = (!args(i).isCircular && (args(i).height == args(i).width))
        }
        return ret;
    }
    def isCircle(args: Array[Shapes]) : Array[Boolean] = {
        var ret = new Array[Boolean](args.size);
        var i = 0;
        //checks whether each shape is a circle or not
        for( i <- 0 until args.size) {
            ret(i) = (args(i).isCircular && (args(i).major == args(i).minor))
        }
        return ret;
    }

    //Question 2
    //Has a few risks of being editted from the outside, so we should make all vars private. We should also have comments to note the DTI (such as area = width*height)

    //Question 3
    //We can break the invariance by doing something like, d.dimension.width = -1
    class Rectangle(var width: Int, var height: Int)

    class Slab(private val _dimension: Rectangle) {
        private val _area = _dimension.width * _dimension.height
        def dimension = _dimension;
    }   
    //Improved ver.
    class Slab2(private val _dimension: Rectangle) {
        private val _area = _dimension.width * _dimension.height
        private def dimension = _dimension;
    }   

    // Question 4
    class Triangle
    class OpaqueTriangle extends Triangle
    class Renderer {
        def accept(a: Triangle) = println("Accepted for rendering.")
    } 
    class RayTracingRenderer extends Renderer {
        override def accept(a: Triangle) = println("Different Output!!")
        def accept(a: OpaqueTriangle) = println("Accepted for ray-trace rendering")
    }
    // Original Code will the following two lines, as all it does is prints the line of code represented by the class as defined.
    /*
        Accepted for rendering.
        Accepted for ray-trace rendering.
    */
    //Overriding the definition placed in Renderer allows us to change the message output

    //Question 5
    class Ellipse(private var _a: Int, private var _b: Int) {
        def a = _a
        def a_=(a: Int) = {_a = a}

        def b = _b 
        def b_=(b: Int) = {_b = b}

        def exchange : Unit = {
            var tmp = _a; 
            a_=(_b)
            b_=(tmp)
            // println(_a,_b)
        }
    }
    class LoggedEllipse(var _a: Int, var _b: Int) extends Ellipse(_a,_b) {
        var cnt = 0;
        override def exchange : Unit = {
            var tmp = _a;
            a_=(_b)
            b_=(tmp)
        }

        def getIncreases = cnt;
    }

    //Question 6
    /**
        a)


    **/

    //Question 7

    

    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)

        //Break Invariance for Q3
        var d = new Slab(new Rectangle(10,20));
        println(d.dimension.width + ", " + d.dimension.height)
        var e = d;
        e.dimension.width = 100;
        println(d.dimension.width + ", " + d.dimension.height)

        //Test Q4
        val a: OpaqueTriangle = new OpaqueTriangle
        val r1: Renderer = new RayTracingRenderer
        r1.accept(a)
        val r2: RayTracingRenderer = new RayTracingRenderer
        r2.accept(a)

    }
}