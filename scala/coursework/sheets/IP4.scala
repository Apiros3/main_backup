
object IP4 {

    var MAX = 100
    private val MAX_SIZE = 1000 // max number of names we can store

    //Question 1
    class Test {
        val m = new scala.collection.mutable.HashSet[String]
        assert(m.isEmpty)
        m.add("Aa?");
        assert(m.contains("Aa?"))
        assert(m.size == 1)
        m.add("Aa?");
        assert(m.size == 1)
        assert(!m.contains("Boo!"))
        m.remove("Aa?")
        assert(!m.contains("Aa?"))
        m.remove("Aa?")
        try {
            m.head 
        }
        catch {
            case e: NoSuchElementException => println("Failed Successfully")
            case _ => println("Failed") 
        }
        
        println("successful test")
    }

    //Question 2
    trait Stack[A]{
        //pushes element to stack
        //post : S = elem : S_0
        def push(elem: A) : Unit 
        //pops top element
        //post : S = S' where S_0 = (s' : S')
        def pop : Unit 
        //checks whether S is empty or not
        //post : S = S_0 && returns (S == [])
        def isEmpty : Boolean  
    }
    
    //Question 3 / 4

    /** state: S : P Int
    * init: S = {} */
    trait IntSet{
        /** Add elem to the set.
        * post: (S = S0 ∪ {elem} && elem ∈ [0..N)) || 
        *        S = S0 && elem \∈ [0..N))
        */
        def add(elem: Int): Unit
        /** Does the set contain elem?
        * post: S = S0 ∧ returns elem ∈ S */
        def contains(elem: Int): Boolean
        /** Remove elem from the set.
        * post: S = S0 − {elem} */
        def remove(elem: Int): Unit
        /** The size of the set.
        * post: S = S0 ∧ returns #S */
        def size: Int

        //! 4a), 4b)
        /** returns the front most element of set
        -> first element of the set refers to the smallest element in the set <=> min(S). If no elements exist, we should throw an exception    
        * post: S = S_0 && returns min(S) */
        def head: Int 
    }

    class BitMapSet extends IntSet {
        val a = new Array[Boolean](MAX_SIZE)
        var n = 0

        //Invariant I := n = #(true in a)
        def add(elem: Int) : Unit = {
            if (!a(elem)) n += 1 //we will update a(elem) from false to true so we increment n by 1
            a(elem) = true 
        }
        def contains(elem: Int) : Boolean = {
            return a(elem)
        }
        def remove(elem: Int) : Unit = {
            if (a(elem)) n -= 1 //we will update a(elem) from true to false so we decrement n by 1
            a(elem) = false 
        }
        def size : Int = {
            return n
        }
        //! 4c)
        def head : Int = {
            require(n > 0)
            var i = 0;
            while(!a(i)) i += 1
            return i
        }
    }


    //Question 5
    /** The state of a phone book, mapping names (Strings) to numbers (also
    * Strings).
    * state: book : String → String
    * init: book = {} */
    trait Book{
        /** Add the maplet name -> number to the mapping.
        * post: book = book0 ⊕ {name → number} */
        def store(name: String, number: String) : Unit
        /** Return the number stored against name.
        * pre: name ∈ dom book
        * post: book = book0 ∧ returns book(name) */
        def recall(name: String) : String
        /** Is name in the book?
        * post: book = book0 ∧ returns name ∈ dom book */
        def isInBook(name: String) : Boolean
       
       
        /** Delete the number stored against name (if it exists) 
        * post : returns name ∈ dom book_0 &&
              (name ∈ dom book_0 && book = book_0 - {name->book_0(name)} ||
              name \∈ dom book0_ && book = book_0) 
        */
        def delete(name: String) : Boolean
    }

    object ArraysBook extends Book{
        private val names = new Array[String](MAX)
        private val numbers = new Array[String](MAX)
        private var count = 0
        // These variables together represent the mapping
        // Abs: book = 
        // { names(i) -> numbers(i) | i <- [0..count) }
        // DTI: 0 <= count <= MAX && 
        // entries in names[0..count) are distinct

        // Return the index i<count s.t. names(i) = name; or 
        //              return count if no such index exists
        private def find(name: String) : Int = {
            // Invariant: name not in names[0..i) && i <= count
            var i = 0
            while(i < count && names(i) != name) i += 1
            i
        }

        /** Return the number stored against name */
        def recall(name: String) : String = {
            val i = find(name)
            assert(i < count)
            numbers(i)
        }

        /** Is name in the book? */
        def isInBook(name: String) : Boolean = find(name) < count

        /** Add the maplet name -> number to the mapping */
        def store(name: String, number: String): Unit = {
            val i = find(name)
            if(i == count){
            assert(count < MAX); names(i) = name; count += 1
            }
            numbers(i) = number
        }

        def delete(name: String) : Boolean = {
            var k = find(name)
            //name was not found
            if (k == count) return false 
            //if name was found:
            count -= 1
            names(k) = names(count)
            numbers(k) = numbers(count)
            //overloads entry k with count
            return true
        }
    }

    //Question 6
    class Phonebook extends Book {
        val MAX_SIZE = 1000
        val names = new Array[String](MAX_SIZE)
        val numbers = new Array[String](MAX_SIZE)
        var count = 0 //number of stored elements

        //returns key R s.t. names[0..R) < name <= names[R..count)
        //does binary search in O(log count) = O(log n)
        def locate(name: String) : Int = {
            if (count == 0) return 0
            if (name <= names(0)) return 0 
            var L = 0; var R = count 
            //Invariant I := 0 <= L < R <= count
            //            && names[0..L] < name
            //            && name <= names[R..count)
            while(L+1 < R) {
                var m = (L+R)/2 
                if (names(m) < name) {
                    L = m; //names[0..m] < name so we can set L = m
                }
                else {
                    R = m; //name <= names[m..count] so we can set R = m
                }
            }
            //I and not G => I and R = L+1
            // => names[0..L] < name <= names[L+1..count)
            return R
        }

        def store(name: String, number: String) : Unit = {
            var k = locate(name)
            //name is already in the array
            if (k != count && names(k) == name) {
                numbers(k) = number 
            }
            else {
                assert(count != MAX_SIZE)
                var i = count
                //shifts numbers[k..count) to numbers(k..count] (and same with names) 
                while(k < i) {
                    names(i) = names(i-1)
                    numbers(i) = numbers(i-1)
                    i -= 1
                }
                //places the name and number into the kth slot to maintain ordered array
                names(k) = name 
                numbers(k) = number 

                //update number of names stored
                count += 1
            }
        } //shifting is the bottleneck giving time complexity O(n)

        def recall(name: String) : String = {
            require(isInBook(name))
            var k = locate(name)
            return numbers(k)
        } //bottlneck is the call of locate so time complexity is O(log n)

        def isInBook(name: String) : Boolean = {
            var k = locate(name)
            return (names(k) == name) 
        } //bottleneck is the call of locate so time complexity is O(log n)

        def delete(name: String) : Boolean = {
            var k = locate(name)
            //if the element to delete does not exist return false
            if (names(k) != name) return false 
            //shifts numbers[k+1..count) = numbers[k..count-1) and reduces count by 1
            count -= 1
            while(k < count) {
                names(k) = names(k+1)
                numbers(k) = numbers(k+1)
                k += 1
            }
            return true
        } //bottleneck is shifting the elements to the time complexity is O(n)
    }

    //Question 7
    trait Bag{
        //adds elem to set
        //post: S = S_0 ∪ {elem}
        def add(elem: Int): Unit 
        //returns number of elem in set
        //post: S = S_00 ∧ returns #(elem in S)
        def count(elem: Int): Int
    }

    class Bag_cnt extends Bag {
        val c = new Array[Int](MAX);
        def add(elem: Int) : Unit = {
            require(0 <= elem && elem < MAX)
            c(elem) += 1 //increment number of occurances of elem
        }
        def count(elem: Int) : Int = {
            require(0 <= elem && elem < MAX)
            return c(elem) //returns number of occurances of elem
        }
    }

    //Question 8
    def bag_sort(a : Array[Int]) : Array[Int] = {
        var n = a.size 
        var bag = new Bag_cnt()

        var i = 0;
        //(trivial loop) adds each element of a into bag
        while(i < n) {
            bag.add(a(i))
            i += 1
        }
  
        var ret = new Array[Int](n)
        var j = 0;
        i = 0;
        //Invariant I := ret[0..i) is sorted 
        //            && ret[0..i) is the same multiset as all elements in a[0..n) with value in [0,j)
        //            && i = sum(bag.count(j)) in [0,j)
        while(j < MAX) {
            var k = i + bag.count(j)
            //Invariant I := ret[0..i_0) = ret_0[0..i_0]
            //            && ret[i_0..i) = j 
            //appends ret with the number of occurances of j (k - i_0 = bag.count(j))
            while(i < k) {
                ret(i) = j 
                i += 1
            }
            j += 1
        }
        //I && not G => I && j = MAX 
        //  => ret[0..i) is sorted && i = sum(bag.count(j)) in [0..MAX) = n && I
        //  => ret[0..n) is sorted with elements = a[0..n)
        return ret

        //time complexity => adding elements to bag: O(N)
        // + looping through each possible element in bag: O(MAX)
        // + appending ret: O(sum(bag.count(j))) = O(N)
        // so total time complexity is O(N+MAX)
    }


    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)

        //Quick debugging for Question 1
        var s = new Test()

        //Quick debugging for Question 6
        var t = new Phonebook()
        println(t.isInBook("aaa"))
        t.store("aaa","555")
        println(t.isInBook("aaa"))
        println(t.recall("aaa"))
        t.store("aaa","777")
        println(t.recall("aaa"))

        //Quick debugging for Question 7
        var test = new Bag_cnt()
        println(test.count(3))
        test.add(3)
        println(test.count(3))

    }
}