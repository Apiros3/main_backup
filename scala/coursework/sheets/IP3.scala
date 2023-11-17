
object IP3 {
    
    //Question 2
    def ternary(y: Int) : Int = {
        require(y >= 0)
        var L = 0; var R = 65536 //y <= 2^31-1 < 2^31 = R^2, placed to prevent overflow (specifically, if 0 <= K < R, K^2 does not overflow)
        //Invariant I := (0 <= L < R <= 2^16) ^ (L^2 <= y < R^2)
        while(L+1 < R) {
            var T1 = (2*L+R)/3; var T2 = (L+2*R)/3
            //This part never overflows as R <= 2^16
            //L <= T1 <= T2 < R
            if (T2*T2 <= y) {
                L = T2 
                //I kept as L^2 = T2^2 <= y and R hasn't been changed
            }
            else if (T1*T1 <= y) {
                L = T1
                R = T2 
                //I kept as L^2 = T1^2 <= y < T2^2 = R^2 by condition
                //T1 != T2 as else T2^2 <= y so there is no overlap (L < R)
            }
            else {
                R = T1 
                //I kept as y < T1^2 = R^2 and L hasn't been changed
                //L != T1 as else T1^2 <= y so there is no overlap (L < R) 
            }
            //!Q2b)
            //at the end, we always have L < R, so we never work with empty intervals, and the loop ensures that this is always the case 
        }
        //I && not G => (L^2 <= y < R^2) && (L < R) && (R <= L+1) => (L^2 <= y < R^2) && (R == L+1)

        return L
    }
    //checks if (note_binary(i) == ternary(i)) for i in [0,N)
    def check_ternary_small(N: Int) : Boolean = {
        require(N >= 0)
        //Lecture note Binary search integer square root to check answer for 0 <= y < 46000
        def note_binary(y: Int) : Int = {
            require(y >= 0)
            // Deal with y=0 or 1
            if (y <= 1) return y
            // Invariant I: a^2 <= y < b^2 and 0 <= a < b
            var a = 0; var b = y
            while(a+1 < b) {
                val m = (a+b)/2 // a < m < b
                if(m*m <= y) a = m else b = m
            }
            // a^2 <= y < (a+1)^2
            return a 
        }
        
        var i = 0
        //Invariant I := 0 <= i <= N && for all 0 <= k < i, note_binary(k) == ternary(k)
        while(i < N && note_binary(i) == ternary(i)) {
            i += 1
        } 
        //returns true if for all 0 <= k < N, note_binary(k) == ternary(k)
        //otherwise, there exists some k < N s.t note_binary(k) != ternary(k) so returns false
        return i == N    
    }
    //checks if a^2 <= y < (a+1)^2 for all y in [0,N) 
    def check_ternary_large(N: BigInt) : Boolean = {
        require(0 <= N && N+1 <= Int.MaxValue)
        var i = 0
        //Similar invariance to above
        while(i < N) {
            var a = BigInt(ternary(i))
            if (a*a <= i && i < (a+1)*(a+1)) i += 1
        }
        //With similar logic again, as to the above
        return i == N
    }

    //Question 3
    def guess_small(X : BigInt) : Int = {
        var guesses = 0
        def tooBig(y: BigInt) : Boolean = {
            return y > X 
        }
        var L = 0; var R = 1001
        //Invariance I := 0 <= L < R <= 1001 && L <= X < R
        while(L+1 < R) {
            var m = BigInt(L + (R-L)/2)
            if (tooBig(m)) {
                //if X < m, setting X < m = R keeps the invariance
                R = m.toInt
            }
            else {
                //if m <= X, setting L = m <= X keeps the invariance
                L = m.toInt
            }
            guesses += 1
        }
        //I and not G => (L <= X < R) and R = L+1
        //check answer
        println(L)
        return guesses
    }
    def guess(X : BigInt) : Int = {
        var guesses = 0 
        def tooBig(y: BigInt) : Boolean = {
            return y > X 
        }
        var R : BigInt = 1
        //Invariance I := 0 < R
        while(!tooBig(R)) {
            R *= 2
            guesses += 1
        }
        //I and not G => L < R and R > X 
        
        var L : BigInt = R/2
        //L < R and L <= X < R (as tooBig(R/2) is false)
        //from here, we use the same invariance as in guess_small
        while(L + 1 < R) {
            var m = L + (R-L)/2 
            if (tooBig(m)) {
                R = m
            }
            else {
                L = m 
            }
            guesses += 1
        }
        //check answer is correct
        println(L)
        return guesses

        //!Q3b)
        //assuming BigInt addition takes O(1) time, we have the initial loop to find L and R taking O(log2X) steps, and another O(log2X) steps taken to find the value of X, so the total computation time is O(log2X)
    } 
    
    //Question 4
    def insertion() : Array[Int] = {

        var a = Array(1,5,4,6,2,7,4,3,5,3,4)
        var N = a.size

        def bin(n : Int) : Int = {
            if (a(n) < a(0)) return 0

            //Invariance I := for all i in [0..L] a(i) <= a(n) && for all i in [R..n) a(n) < a(i)  
            var L = 0; var R = n;
            while(L+1 < R) {
                var m = L + (R-L)/2
                if (a(m) <= a(n)) {
                    // a(m) = a(L) <= a(n) so invariance is kept
                    L = m
                }
                else {
                    R = m
                    // a(m) = a(R) > a(n) so invariance is kept
                }
            }        
            //I and not G => I and R = L+1 => for all i in [0..L] a(i) <= a(n) && for all i in [L+1,n), a(n) < a(i)
            //return where we want to place a(n)
            return L+1
        }

        var i = 0
        //Invariance I := a[0..i) is sorted in increasing order, and a[0..N) is a permutation of a_0[0..N)
        while(i < N) {

            var k = bin(i)
            //k is such that for all j in [0..k), a(j) < a(i), and for all j in (k,i), a(i) <= a(j)
            //effectively, we want to now put a(i) -> a(k) and shift all elements in a(k..i) by 1 to the right

            var n = i
            //Invariance I := a[0..n) = a_0[0..n) && a(n) = a(i), a(n..i] = a_0[n..i) && 0 <= k <= n <= i 
            while (n > k) {
                var t = a(n); a(n) = a(n-1); a(n-1) = t 
                //swaps a(n) with a(n-1)
                //now we have a[0..n-1) = a_0[0..n-1), a(n-1) = a(i), a(n-1..i] = a_0[n-1..i)  
                n -= 1
                //"replaces" n-1 with n s.t the invariance is kept
            }
            //a permutation of a_0 as we only perform transpotitions
            //I and not G => I and n = k => a[0..k-1) = a_0[0..k-1) && a(k) = a(i) && a(k..i] = a_0[k..i)
            //thus a[0..i] is sorted and a is a permutation of a_0
            i += 1
            //I is kept as a[0..i) is sorted and a is a permutation of a_0
        }
        //I and not G => I and i = n => a[0..n) is sorted
        return a

        //number of comparisons of a = sum(log i) = log(n!) ≈ nlogn comparisons 
        //running time bottleneck is the swap which is given by T(n) = T(n-1) + O(n) => T(n) = O(n^2) 
    }

    //Question 6
    //two pointer approach
    //be careful of cases like [4,5,3] which needs to go to [3,4,5] in one step
    def partition(l : Int, r : Int) : Int = {
        var a = Array(5,4,6,2,7)

        var x = a(l) //pivot

        var L = l+1; 
        var cnt = 0 //keeps track of how many elements < x 
        //Invartiance I := l+1 <= L < r && cnt = number of elements in a[l+1,L) s.t a(i) < x , a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N) && a[l..r) is a permutation of a_0[l..r)
        while(L < r) {
            if (a(L) < x) cnt += 1 //cnt = #of elements in a[l+1,L] s.t a(i) < x
            L += 1 //maintains invariance by adding 1 to L s.t. cnt = #of elements in a[l+1,L) s.t a(i) < x
        }
        //I and not G => cnt = #of elements in [l+1,L) s.t a(i) < x and L = r 
        L = l+1;
        var R = r-1;
        //now, we just want all a(i) < x in a[l,l+cnt) and all a(i) >= x (apart from the pivot) in a[l+cnt+1,r) and a[l+cnt] = pivot

        //if the element is already in the correct position, we leave that element and don't come back
        
        //Invariant I :=    (a[l+1..L) < x = a(l) <= a(R..r) || a[l..L-1) < x = a(l+cnt) <= a(R..r))
        //               && a[0..l) = a_0[0..l) && a[r..N) = a_0[r..N)
        //               && a[l..r) is a permutation of a_0[l..r)
        //               && l < L <= l+cnt+1 <= R < r
        while(L <= l+cnt) {
            if (a(L) < x) { //a[l+1,L] < x
                L += 1 //a[l+1,L) < x
            } else if (a(R) >= x) { //a[R..r) >= x
                R -= 1 //a(R..r) >= x
            } else {
                //a(L) >= x and a(R) < x
                //special case if L == l+cnt as we need to put the pivot there at the end, thus need to do this swap simultaneously
                if (L == l+cnt) {
                    a(l) = a(R); a(R) = a(L); a(L) = x
                    // a[l..L-1] < x = a(l+cnt) <= a[R..r))
                }
                else {
                    var t = a(L); a(L) = a(R); a(R) = a(L)
                    // a[l..L] < x <= a[R..r)
                }
                L += 1
                R -= 1
                //ensures invariance is kept and variance decreases
            }
        }
        //I and not G => (a[l+1..L) < x = a(l) <= a(R..r) || a[l..L-1) < x = a(l+cnt) <= a(R..r)) && L = l+cnt+1
        //we want a[l..L-1) < x = a(l+cnt) <= a(R..r)), so check if a(l+cnt) == x and if not swap a(l) with a(l+cnt)
        if (a(l+cnt) != x) {
            a(l) = a(l+cnt); a(l+cnt) = x
        }
        //finally, we are left with a[l..l+cnt) < x <= a(l+cnt) <= a(R..r) and from the first invariance, as a[l..r) has cnt elements less than x, which are all in a[l..l+cnt), we have a[l..l+cnt) < x = a(l+cnt) <= a(l+cnt..r)
        println(a.mkString(" "))
        return l+cnt 
    } 

    // Question 7
    def QSort(l: Int, r: Int) : Unit = {
        var L = l;
        while (r-L > 1) {
            val k = partition(L,r)
            QSort(L,k);
            L = k+1     
        }
    }
    //!Q7b)
    //stack could reach depth N if each partition gives k = r-1
    //Q7c)
    //we could implement a O(N) median function and reimplement the partition s.t. we partition around the median instead of a(l), then we will always have the size of the recursive call halving, so the stack will never get deeper than ceil(log2N)

    //Question 8
    def partition2(l : Int, r : Int) : (Int, Int) = {
        var a = Array(5,4,6,2,7,4,3,5,3,4)

        var x = a(l) //set pivot
        var xcnt = 1 //keeps track of the number of times the same element as the pivot appears in the list

        var L = l+1; var R = r-1;
        //Invariance I := a[l+1,L) <= x < a(R,r) 
        //             && a[l] = x && l+1 <= L <= R < r
        //             && a[0..l) = a_0[0..l) 
        //             && a[r..n) = a_0[r..n) 
        //             && a[l..r) is a permutation of a_0[l..r)
        //             && a[l,l+xcnt) = x && a[l+xcnt,L) != x
        while(L < R) {
            //for debugging
            // if (a(L) > 1) println(a(L)) 
            if (a(L) < x) { //a[l+1,L] < x
                L += 1 //a[l+1,L) < x
            } else if (a(L) > x) { 
                var t = a(L); a(L) = a(R); a(R) = t;
                //swaps a(L) and a(R) => a(R) > x => x <= a[R,r)
                R -= 1 //x <= a(R,r)
            } else {
                var t = a(L); a(L) = a(l+xcnt); a(l+xcnt) = t 
                //swaps a(L) with a(l+xcnt) => a(l+xcnt) = x and a(L) != x => a[l,l+xcnt] = x && a(l+xcnt,L] != x
                xcnt += 1 //a[l,l+xcnt) = x && a[l+cnt,L] != x
                L += 1 //a[l+cnt,L) != x
            }
        }
        // println(a.mkString(" "))
        var i = xcnt
        //Invariant I := a[L..r) = a_0[L..r) && a[0..L) is a permutation of a_0[0..L) and a[L-xcnt,L-i) = x
        while(i > 0) {
            var t = a(l+i-1)
            a(l+i-1) = a(L-i)
            a(L-i) = t
            //ensures invariance is kept
            i -= 1
        }
        //I and not G => I && (i = 0) => a[L-xcnt,L) = x, a is a permutation of a_0, a[0..L-xcnt) < x && a[L..r) > x
        println(a.mkString(" "))
        (L-xcnt,L)
    }


    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)
    }
}