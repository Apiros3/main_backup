object IP1 {
    
    class Dimension(var width: Int, var height: Int)
    class Frame(private val _dimension: Dimension) {
    private val _area = _dimension.width * _dimension.height
    def area = {
        println(_dimension.width * _dimension.height)
        _area
    }
    def dimension = _dimension
    def isPortrait = _dimension.height >= _dimension.width
    }

    def main(args: Array[String]) : Unit  = {
        var b = new Dimension(1,2)
        var a = new Frame(b)
        println(a.area)
        b.width = 3
        println(a.area)

        val str = 
            "Running..."
        println(str)
    }
}