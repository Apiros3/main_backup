object IP1 {
    //Question 1    
    def square(a : Int) : Int = {
        a*a
    } 
    def mod3(a : Int) : Int = {
        a%3
    }
    def perfectsquare(a : Int) : Int = {
        var j = 1
        while(j*j <= a) j += 1
        (j-1)*(j-1)
    }
    
    //Question 2
    def sum(a : Array[Int]) : Int = { 
        var N = a.size 
        var csum = 0; var i = N
        //Invariant I := csum = sum(a[i..N)) && 0 <= i <= N
        while(i > 0) {
            //I ^ G = I && i > 0
            i -= 1
            csum += a(i)
        }
        //I ^ not G <=> csum = sum(a[i..N)) && i = 0 
        csum
    }

    //Question 3
    def max_elem(a : Array[Int]) : Int = {
        val N = a.size
        var max_element = 0; var i = 0
        //Invariant I := max_elment = max(a[0..i)) && 0 <= i <= N
        while(i < N) {
            if (a(i) > max_element)
                max_element = a(i)
            i += 1    
        }
        //I ^ not G <=> max_element = max(a[0..i)) && i = N
        max_element
    }

    //Question 5
    def fib(n : Int) : Int = {
        require(n >= 0)
        if (n <= 1) n
        else fib(n-1) + fib(n-2)
    }
    def fib2(n : Int) : Unit = {
        require(n >= 0)
        fibshow(n,0)

        //creates line spacing
        def linespace(a : Int) : Unit = {
            var i = 0
            while(i < a) {
                print("| ")
                i += 1
            }
        }
        def fibshow(n : Int, depth : Int) : Int = {
            linespace(depth)
            println("fib(" + n + ")")
            if (n <= 1) {
                linespace(depth)
                println("= " + n)
                n
            }
            else {
                //call smaller cases first before printing line for top case
                var fib_value = fibshow(n-1, depth+1) + fibshow(n-2, depth+1)
                linespace(depth)
                println("= " + fib_value)
                fib_value
            }
        }
    }

    //Question 6
    def fib_mem(n : Int) : Int = {
        require(n >= 0)
        if (n <= 1) n
        //Invariance (for n >= 2) I := prev = fib(i), pprev = fib(i-1) && 0 <= i <= n
        var prev = 1; var pprev = 0
        var i = 1 
        while(i < n) {
            i += 1
            prev += pprev //prev = fib(i), pprev = fib(i-2)
            pprev = prev - pprev //pprev = fib(i-1)
        }
        //I && not G => prev = fib(i) && i = n
        prev 
    } 

    //Question 7
    def findqr(x : Int, y : Int) : (Int,Int) = {
        require(x >= 0 && y > 0)
        var q = 0; var r = x 
        // Invariant I := (x = q*y + r) && r >= 0  
        while(r >= y) {
            r -= y //r >= 0 as G => r >= y at the start  
            q += 1 //ensures invariance is kept
        }
        //I ^ not G <=> 0 <= r < y && x = q*y + r
        (q,r)
    }

    //Question 8
    //N.B, this code is also defined for x,y = 0 and will not detect 0 inputs
    def gcd(x : Int, y : Int) : Int = {
        require(x >= 0 && y >= 0)
        if (y < x) gcd(y,x)
        //from here, x <= y
        //we will use the fact that gcd(x,y) = gcd(r,x) where r = y%x
        if (x == 0) y
        else gcd(y%x,x)
    }
    def slow_ext_gcd(x : Int, y : Int) : (Int,Int) = {
        var a = gcd(x,y)
        //if some solution (m,n) exists, s.t mx+ny = a, we also have (m+y/a,n-x/a) as a solution, so if solutions exist, they exist in an interval at most y/a (for m). So, we will loop between (0,y/a]
        var m = 0; var n = 0
        // Invariant I := (0 <= m <= y/a + 1)
        while(m <= y/a && m*x + n*y != a) {
            m += 1
            n = (a - m*x)/y 
        }
        //if m = y/a, no solutions, so we will return a flag
        //otherwise, I ^ not G => mx + ny = a, so we will return (m,n)
        if (m == y/a + 1) {
            println("Something must have gone wrong...")
            (0,0)
        }
        else (m,n)
    }
    def fast_ext_gcd(x : Int, y : Int) : Int = {
        require(x > 0 && y > 0) 
        /**
        starting with mx + ny = a
        let x = qy + r, where r = x%y
            then, by subtitution, n(qy+r) + my = a
            <=> (qn + m)y + nr = a
            so, we can find solutions to mx + ny = a given the solution to some m'y + n'r = a
            if we find such m' and n', n = n', m = m'-qn'
        smallest case must terminate at (x,y) = (a,0) which clearly have solutions (1,0) 
        **/
        def findmn(xs : Int, ys : Int) : (Int, Int, Int) = {
            if (ys == 0) {
                (1,0,xs) //returns (m,n,xs)
            }
            else {
                var (tm,tn,d) = findmn(ys, xs%ys)
                (tn,tm-(xs/ys*tn),d)
            }
        }
        var (solm, soln, divisor) = findmn(x,y)
        println("m = " + solm + " + " + y/divisor + "t")
        println("n = " + soln + " - " + x/divisor + "t")
        divisor
    }
    def fast_ext_gcd_loop(x : Int, y : Int) : (Int,Int) = {
        require(x > 0 && y > 0)
        var a = gcd(x,y); 
        //we want some mx + ny = a 
        //consider keeping sols to ax+by=s (xs,ys), and ax+by=t (xt,yt)
        var s = x; var ms = 1; var ns = 0; 
        var t = y; var mt = 0; var nt = 1;
        // take u := s mod t (u = s-t(s/t))
        // <=> (amu+bnu) = (ams+bns) - (amt+bnt)(s/t)
        // <=> mu = ms-mt(s/t), nu = ns-nt(s/t)

        //Invariant I := msx+nsy=s, mtx+nty = t
        //Invariant I := gcd(s,t) = gcd(x,y)
        while(s%t != 0) {
            var tmp = s/t
            var u = s - t*tmp; //(u = s%t)
            var mu = ms-mt*tmp; var nu = ns-nt*tmp 

            s = t; ms = mt; ns = nt;
            t = u; mt = mu; nt = nu;
        }
        //at the end we have:
        //s = msx+nsy, t = mtx+nty, s%t = 0, gcd(s,t) = gcd(x,y) => t = gcd(x,y) ^ t = mtx + nty  
        (mt,nt)

    }

    //Question 9
    def hitnum(a : Array[Int]) : Int = {
        var n = a.size 
        require(n >= 1)
        var i = 1; var hits = 0; var max = a(0)
        while(i < n) {
            if (a(i) > max) {
                hits += 1
                max = a(i)
            }
            i += 1
        }
        hits
    }

    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)
    }
}