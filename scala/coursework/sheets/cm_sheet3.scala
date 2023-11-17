
object test {

    def main(args: Array[String]) : Unit  = {
        
        var i = 0;
        var dec1 = (1.toDouble)/3; var dec2 = (1.toDouble)/12
        while(i < 100) {
            var tdec = 2.25*dec2 - 0.5*dec1
            println(tdec)
            dec1 = dec2;
            dec2 = tdec

            i += 1;
        }

    }
}