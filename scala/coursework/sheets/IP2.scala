
//Check for Question 2
object SideEffects{
    var x = 3; var y = 5
    def nasty(x: Int) : Int = { y = 1; 2 * x }
    def main(args: Array[String]) = println(nasty(x) + y)
}

object IP2 {
    
    var max_n = 100

    //Question 3
    //Was unable to get ScalaTest Running
    //different types of sorting may include the normal sorting option, but also the sortBy function, where we can define the ordering of the string to our liking (i.e., prefering shorter strings over lexiographically smaller strings)

    //Question 4
    def time() : Unit = {
        val timeEnd:Double = 1.0
        val numSteps:Int = 152538902
        val timeStep:Double = timeEnd/numSteps 
            //this could cause rounding errors (i.e. downwards, s.t timeEnd/numSteps*numSteps < timeEnd)
        // timeEnd=numSteps*timeStep and numSteps∈ N
        var time = 0.0
            //if we hhave timeEnd/numSteps*numSteps < timeEnd (by floating point error), the program will be called numSteps + 1 times
        while (time < timeEnd)
        {
        // Inv: 0 <= time <= timeEnd and time=k*timeStep for some k∈ N
        time += timeStep
        }
        println(time)

            //error could be at most timeStep, where one loop before the end of the loop, timeEnd/numSteps*numSteps was just nearly machine epsilon below timeEnd s.t. at the next step time ≈ timeEnd + timeStep
        // Inv => time == timeEnd
    }
    def truetime() : Unit = {
        val timeEnd: Double = 1.0
        val numSteps: Int = 15912351
        //timeStep < timeEnd
        val timeStep: Double = timeEnd/numSteps
        var time = 0.0
        var i = 0
        // Inv: 0 <= time <= timeEnd and time=i*timeStep for some i ∈ N && 0 <= i <= numSteps
        while(i < numSteps) {
            i += 1
            //will not overflow as i*timeEnd < IntMax
            time = i*timeEnd/numSteps
        }
        // final error would be based on the last calculation of time <=> error is caused by divison, so we can assume there will be an error of at most approximately machine epsilon
        // Inv => time == timeEnd
        // println(time)
    }

    //Question 5
    /** Does patt appear as a substring of line? */
    def search(patt: Array[Char], line: Array[Char]) : Boolean = {
        val K = patt.size; val N = line.size
        // Invariant: I: found = (line[i..i+K) = patt[0..K) for
        // some i in [0..j)) and 0 <= j <= N-K
        var j = 0; var found = false 
        while(j <= N-K && !found){
        // set found if line[j..j+K) = patt[0..K)
        // Invariant: line[j..j+k) = patt[0..k)
            var k = 0
            while(k<K && line(j+k)==patt(k)) k = k+1
            found = (k>=K)
            j = j+1
        }
        // I && (j=N-K+1 || found)
        // found = ( line[i..i+K) = patt[0..K) for some i in [0..N-K+1) )
        found

        /**
            error test cases:
            Replacing line 6 false by true
                search(Array('a'), Array()) fails
            Replacing line 7 <= by <
                search(Array('a'), Array('b','a')) fails
            Replacing line 7 N-K by N-K+1
                search(Array('a'), Array()) causes an out of bounds error
            Replacing line 10 0 with 1
                search(Array('a'), Array('b')) fails
            Replacing line 11 < by <= 
                search(Array('a'), Array('a')) causes an out of bounds error
            Replacing line 12 == by >=
                no mistakes, taking I and not G, we have k is at most K, so replacing ==K with >=K has no effect on the program output
        **/
    }
    def testsearch(patt: Array[Char], line: Array[Char]) : Boolean = {
        assert(search(Array('a'), Array()) == false)
        assert(search(Array('a'), Array('b','a')) == true)
        assert(search(Array('a'), Array('b')) == false)
        assert(search(Array('a'), Array('a')) == true)
        return search(patt,line)
    }

    //Question 6
    def repstring(s: String) : Int = {
        var n = 1
        while(n < s.size) {
            var teststring = true 
            var i = 0
            while(i < s.size-n) {
                if (s(i) != s(i+n)) teststring = false 
                i += 1
            }
            if (teststring) return n 
            n += 1
        }
        s.size
    }

    //Question 7 
    def exists(p: Int => Boolean, N: Int) : Boolean = {
        var ret = false 
        
        var i = 0
        //Invariance I := there exists i in [0..K) s.t. p(i) && 0 <= K <= N
        while(i < N) {
            if (p(i)) ret = true //if p(i) => there does exist i s.t p(i) 
            //else no such i exists so stays as false 
            i += 1
        }
        //I ^ not G <=> there exists i in [0..N) s.t. p(i)
        return ret
    }
    //function to test if code works
    def randboolf(l: Int) : Boolean = {
        if (l == 3) return true
        else return false
    }

    //Question 8
    def findm(p: Int, q: Int) : Int = {
        if (q/p*p == q) {
            //if (q/p == floor(q/p)) <=> m = floor(q/p) = q/p
            return q/p
        }
        else {
            //if (floor(q/p) < q/p < floor(q/p)+1) <=> q/p <= m = floor(q/p)+1 (as we take smallest m)
            return q/p + 1
        }
    }
    def findd(p: Int, q: Int) : Array[Int] = {
        var d = new Array[Int](max_n)
        var pr = p; var qr = q
        var i = 0
        //Invariant I := p/q = sum 1/d(k) + pr/qr <=> 0 <= i 
        while(pr != 0) {
            var m = findm(pr,qr)
            d(i) = m //p/q = sum 1/d(k) + pr/qr - 1/m
            pr = m*pr - qr 
                // qr/pr <= m < qr/pr+1 <=> 0 <= m*pr-qr < pr
            qr *= m  //pr/qr - 1/m => pr/qr
            i += 1
        }
        //I and not G <=> p/q = sum 1/d(k)

        //for showing result nicely
        var dret = new Array[Int](i)
        while(i > 0) {
            i -= 1
            dret(i) = d(i)
        }

        //if d(i) >= d(i+1), we could have had d(i) => d(i)/2, as we have p'/q' > 1/d(i) + 1/d(i+1) >= 2/d(i) = 1/(d(i)/2)
        //by negation, d(i) must be strictly increasing 
        dret
    }

    //Question 9
    def findy (x: Double) : Int = {
        require(x >= 1)
        //we want log3x-1 < y <= log3x
        //<=> x/3 < 3^y <= x
        var i : Double = 1; var y = 0;
        //Invariant I := i = 3^y && i <= x
        while(x >= 3*i) {
            i *= 3 //as x >= 3*i, we can set i*=3 and I still holds
            y += 1 //update y s.t. i = 3^y
        }
        //I and not G <=> (i = 3^y) ^ (i <= x/3 < i <= x)
        return y
    }

    //Question 10
    def eval(a: Array[Double], x: Double) : Double = {
        require(a.size > 0)
        // if (a.size == 0) return 0
        var sum = a(a.size - 1)

        var i = a.size-1
        //N = a.size
        //Invariance I := sum = sum[i..N) a(k)x^(k-i) && 0 <= i <= N 
        while(i > 0) {
            i -= 1 //sum = sum[i+1..N) a(k)x^(k-i-1) 
            sum *= x //sum = sum[i+1..N) a(k)x^(k-i)
            sum += a(i) //sum = sum[i..N) a(k)x^(k-i)
        }
        //I and not G <=> sum = sum[i..N)a(k)x^(k-i) && i = 0
        sum 
    }

    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)
    }
}