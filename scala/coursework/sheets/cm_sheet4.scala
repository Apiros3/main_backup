
object IP5 {

    def next(x: Double, l: Double) : Double = {
        def f : Double = {
            return Math.exp(l*(x-1)) - x 
        }
        def fp : Double = {
            return Math.exp(l*(x-1))*l - 1
        }
        return x - f/fp
    }

    def next2(x: Double, y: Double) : (Double,Double) = {
        def f1 : Double = {
            return 1/((2-x)*(2-y)) - x 
        }
        def f2 : Double = {
            return 2/((3-2*x)*(3-y)) - y 
        }
        def J : (Double,Double,Double,Double) = {
            var x1 = 1/((2-x)*(2-x)*(2-y)) - 1 
            var x2 = 1/((2-x)*(2-y)*(2-y))
            var y1 = 4/((3-2*x)*(3-2*x)*(3-y))
            var y2 = 2/((3-2*x)*(3-y)*(3-y)) - 1
            return (x1,x2,y1,y2)
        }
        var (a,b,c,d) = J 
        var xret = (b*f2-d*f1)/(a*d-b*c)
        var yret = (a*f2-c*f1)/(b*c-a*d)
        return (xret,yret)
    }

    def main(args: Array[String]) : Unit  = {
        var x0 = 0.0
        var l = 1.0

        for(j <- 0 to 100) {
            x0 = 0.0
            l = 1 + j*0.01
            for(i <- 1 to 10) {
                x0 = next(x0,l)
            }
            println(x0)
        }
        
        var x = 0.0; var y = 0.0 
        for(j <- 0 to 20) {
            var (dx,dy) = next2(x,y)
            x += dx 
            y += dy 
        }
        println(x + " " + y)
    }
}