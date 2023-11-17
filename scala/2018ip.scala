

object ip {

    def eai(n : Int) : Double = {
        require(n > 0)
        var k : Double = 1 + 1/n.toDouble
        var ret = 1.0;
        var i = 0;
        //k = 1+1/n ^ ret = k^i 
        while(i < n) {
            ret *= k 
            i += 1
        }
        return ret
    }
    def eaii(n: Int) : Double = {
        require(n > 0)
        var k : Double = 1 + 1/n.toDouble 
        var ret = 1.0 
        var i = n 
        //(1+1/n)^n = ret * k^i
        while(i > 0) {
            if (i%2 == 0) {
                k = k*k 
                i /= 2 
            }
            else {
                ret *= k 
                k = k*k 
                i /= 2 
            }
        }
        return ret 
    }
    def eb(n : Int) : Double = {
        require(n > 0)
        var ifact = 1.0 
        var i = 0
        var ret = 1.0
        //ret = eb(i) ^ ifact = 1/i!
        while(i < n) {
            i += 1
            ifact /= i.toDouble  
            ret += ifact 
        }
        return ret 
    }
    def fastexp(x: Double, n: Int) : Double = {
        var ret = 1.0;
        var k = x
        var i = n 
        //x^n = ret * k^i 
        while(i > 0) {
            if (i%2 == 0) {
                k = k*k 
                i /= 2 
            }
            else {
                ret *= k 
                k = k*k 
                i /= 2 
            }
        }
        return ret 
    }
    def ec(n : Int) : Double = {
        require(n > 0)
        var k : Double = n 

        return fastexp((k+1)/k,n)*(k+1) - fastexp((k/(k-1)),n-1)*n 
    }

    def bins(a: Array[Int], x: Int) : Int = {
        var L = 0; var R = a.size 
        while(L < R) {
            var m = (L+R)/2 
            if (a(m) < x) L = m+1 
            else R = m 
        }

        //I a[0..L) < x, x <= a[R..n) 
        return L 
    }

    def bils(a: Array[Int], x: Int) : (Int,Int) = {
        var L = bins(a,x) //L s.t. a[L..n) >= x
        var R = bins(a,x+1) //R s.t. a[0..R) < x+1
        return (L,R)
    }

    class Node(var value: Double, var index: Int, var prev: Node, var next: Node) 

    // var empty = new Node(0,0,null,null)
    // empty.next = empty 
    // empty.prev = empty  
    def makeZeroVector(size: Int): Node = {
        require(size >= 0)
        var empty = new Node(0,size,null,null)
        empty.next = empty 
        empty.prev = empty 
        
        return empty 
    }
    //returns node s.t. node.index <= index < node.next.index
    def findvec(vector: Node, index: Int) : Node = {
        var curr = vector 
        while(curr.next.index <= index) {
            curr = curr.next 
        }
        return curr 
    }

    def setElement(vector: Node, value: Double, index: Int) : Unit = {
        require(index < vector.index)
        var curr = findvec(vector,index)
        if (curr.index == index) curr.value = value 
        else {
            var tmp = new Node(value,index,curr,curr.next)
            curr.next.prev = tmp 
            curr.next = tmp 
        }
    }
    def zeroElement(vector: Node, index: Int) : Unit = {
        require(index < vector.index)
        var curr = findvec(vector,index)
        if (curr.index == index) {
            curr.next.prev = curr.prev 
            curr.prev.next = curr.next
        }
    }
    def makeVector(elems: Array[Double]) : Node = {
        var vector = makeZeroVector(elems.size)
        for(i <- 0 until elems.size) {
            if (elems(i) != 0) {
                setElement(vector,elems(i),i)
            }
        }
        return vector
    }
    def dotProduct(vec1: Node, vec2: Node) : Double = {
        require(vec1.index == vec2.index)
        var size = vec1.index 
        var a = vec1.next; var b = vec2.next;
        var ret = 0.0
        while(a.index < size || b.index < size) {
            if (a.index == b.index) {
                ret += a.value * b.value 
                a = a.next 
                b = b.next 
            }
            else if (a.index < b.index) {
                a = a.next 
            }
            else {
                b = b.next 
            }
        }
        return ret
    }

    trait DoubleEndedQueue {

        def isEmpty : Boolean 
    
        def addLeft(x:Int) : Unit 

        def getLeft : Int 

        def addRight(x:Int) : Unit 

        def getRight : Int 

    }

    class ArrayDoubleEndedQueue(val MAX: Int) extends DoubleEndedQueue {
        var q = new Array[Int](MAX)
        var size = 0;
        var start = 0;
        def isEmpty : Boolean = {size == 0}
        def addLeft(x:Int) : Unit = {
            require(size < MAX) 
            q((start+MAX-1)%MAX) = x 
            start = (start + MAX - 1)%MAX 
            size += 1 
        }
        def getLeft: Int = {
            require(size > 0)
            var tmp = q(start)
            start = (start+1)%MAX 
            size -= 1
            return tmp 
        }
        def addRight(x: Int) : Unit = {
            require(size < MAX)
            q((start+size)%MAX) = x 
            size += 1 
        }
        def getRight : Int = {
            require(size > 0)
            var tmp = q((start+size-1)%MAX)
            size -= 1 
            return tmp 
        }

        def display : Unit = {
            print("[")
            for(i <- 0 until size) {
                print(q((start+i)%MAX)) 
                if (i != size-1) print(", ")
            }
            println("]")
        }
    }

    class Tree(value: String, count: Int, left: Tree, right: Tree)

    // def addToTree(word: String, t: Tree) : Tree = {
    //     if (t == null) t = new Tree(word, 1, null, null)
    //     else if (t.value == word) t.count += 1 
    //     else if (t.value < word) {
    //         t.right = addToTree(word,t.right)
    //     }
    //     else {
    //         t.left = addToTree(word,t.left)
    //     }
    //     return t 
    // }
    // def flatten(t: Tree) : (Tree, Tree) = {
    //     require(t != null) 

    //     var str = t; var ed = t;
    //     t.prev = null;
    //     t.next = null;
    //     if (t.left != null) {
    //         var (u,v) = flatten(t.left)
    //         str = u;
    //         v.next = t
    //         t.prev = v  
    //     }
    //     if (t.right != null) {
    //         var (u,v) = flatten(t.right)
    //         ed = v;
    //         t.next = u;
    //         u.prev = t;
    //     }
    
    //     return (str,ed)
    // }

    // def flatten2(t: Tree) : (Tree, Tree) = {
    //     var stack = new scala.collection.mutable.Stack[Tree] 
        
    //     var tmp = t;

    //     while(!stack.isEmpty || tmp != null) {
    //         if(tmp != null) {
    //             stack.push(tmp)
    //             tmp = tmp.left 
    //         }
    //         else {
    //             var x = stack.pop()

    //         }

    //     }

    // }

    case class T(var datum: Int)


    def main(args: Array[String]) : Unit = {
        // println(eai(100))
        // println(eaii(100000))
        // println(eb(10000))
        // println(ec(100000))
        
        // var a = Array(2,4,5,6,6,6,7,8,9)
        // println(bins(a,7))
        // println(bils(a,6))
        // println(bils(a,3))
        // var a = Array(2.0,0,4.0,0,0,0,0,5.5)
        // var b = Array(0,0,0.5,0,0,0,3.0,5.5)

        // println(dotProduct(makeVector(a),makeVector(b)))

        // val queue = new ArrayDoubleEndedQueue(4)
        // queue.addLeft(10)
        // queue.addLeft(20)
        // queue.addRight(30)
        // println(queue.getLeft)
        // queue.addRight(40)
        // queue.addRight(10)
        // println(queue.getRight)
        // queue.addRight(15)
        // println(queue.getLeft)
        // print("Queue status: "); queue.display
        
        var a = new T(1)
        var b = new T(1)
        // a.datum = 2;
        println(a == b)
    }
}
