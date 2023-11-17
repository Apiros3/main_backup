
object IP5 {

    //Question 1 
    class Node(val datum: Int, var next: Node) {
        //set datum_s to concat with the rest of the nodes
        var datum_s = datum.toString
        override def toString : String = {
            //return recursive call by looking at each node, concating datum_s until the linked list finishes
            if (next == null) {
                return datum_s
            }
            return datum_s + " -> " + next.toString
        }
    }
    def newList() : Unit = {
        var myList = new Node(1,null)

        var i = 2;
        //Invariance I := myList_i = Node(i,myList_i-1) && 2 <= i <= 13 && myList_1 = Node(1,null)
        while(i <= 12) {
            myList = new Node(i,myList)
            i += 1
        }
        println("List is " + myList.toString)
        println("rev-List " + reverseList(myList).toString)
    }

    //quick note: ask about notation in invariance for linked lists
    def reverseList(l : Node) : Node = {
        var myList = l 
        //want to set myList.next = null if it is where the linked list started
        var start = true
        var prev = myList
        /**
        denote myList(.next)^i = L(i), myList_0(.next)^i = L_0(i) for all i in [0..k], k is s.t. L_0(k) = null
        Invariance := 
            ((start && prev = myList) || (!start && prev is such that if myList.next = prev, L(i).datum == L_0(k-i-1).datum for all i in [0..k)) 
        */
        while(myList.next != null) {
            var nxt = myList.next
            if (start) {
                myList.next = null
                start = false
            }
            else {
                myList.next = prev 
            }
            //link the list with the reversed side by setting myList.next = prev 
            prev = myList //adjust to I by correcting prev
            myList = nxt 
        } 
        if (!start) 
            myList.next = prev 

        //I && not G => myList_0.next == null or myList is reversed apart from the last node (which is adjusted for right after the loop)
        //In the first case, we have myList_0.next == null => k = 1, so the reverse is itself so we just return itself

        return myList 
    }

    //Question 3 
    // A book trait consisting of functions that takes a name to a number
    //state: Book : {String -> String}
    //init: Book = []
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
    class BookNode(val name : String, var number : String, var next : BookNode) 
    class Phonebook extends Book { 
        //default book with a dummy header 
        //DTI := book = BookNode("","?",_) (dummy header is always at the front) && book(.next)^i.name <= book(.next)^(i+1).name
        //s := set of elements in book(.next)^k, k > 0 
        //abstraction function: Book x = {error if x is not an element of s, returns book_t.number where book_t.name == x and book_t = book(.next)^k for some integer k}
        //initialise by dummy header <=> s = []
        var book = new BookNode("", "?", null)
        def store(names: String, numbers: String) : Unit = {
            var book_t = book 
            //I := book(.next)^i.name <= name where i is the number of times the while loop has been excecuted && book_t = book(.next)^i
            while(book_t.next != null && book_t.next.name <= names) {
                //book(.next)^i+1.name <= names so we can move through to the next loop, updating book_t to match the invariance
                book_t = book_t.next 
            }   
            //I and not G => book_t.name <= names < book_t.next.name
            if (book_t.name == names) {
                //if (names is contained in book, just update the number)
                book_t.number = numbers
            }
            else {
                var in = new BookNode(names,numbers,book_t.next)
                //otherwise insert the node in between book_t and book_t.next
                book_t.next = in
            }
        }

        def recall(names: String) : String = {
            require(isInBook(names)) //we need names to be in book
            var book_t = book 
            while(book_t.name != names) {
                book_t = book_t.next 
            }
            //search through book, until we find book_t.name == names, and return its number
            return book_t.number
        }

        def isInBook(names: String) : Boolean = {
            var book_t = book 
            //search through the entire list until book_t.name == names or no such entry occurs and we reach the end of the list
            while(book_t != null && book_t.name != names) {
                book_t = book_t.next 
            }
            //distinguish between whether names is in book by checking whether the loop reached the end, or we found it somewhere earlier
            return book_t != null 
        }

        def delete(names: String) : Boolean = {
            var book_t = book 
            while(book_t.next != null) {//search through the entire book
                // println(book_t.next.name)
                if (book_t.next.name == names) {
                    //if we find an entry in book with book_t.next.name == names, delete book_t.next from book by setting book_t.next = book_t.next.next (skipping that entry in the link)
                    book_t.next = book_t.next.next
                    return true //return true for deleting names
                } 
                book_t = book_t.next
            }  
            return false //return false if the entry was not found in book
        }
    }

    //Question 4
    /**
    Consider a similar implementation to question 3, but instead we create a class foo(name: String, numbers: String, occurances: Int, next: foo)
    Instead of ordering around name, we now order around occurances. Every time we get a call of recall(k), delete k, and restore k, whilst incrementing the number of occurances
    */ 
    // A book trait consisting of functions that takes a name to a number
    //state: Book : {String -> String}
    //init: Book = []
    trait Book2{
        /** Add the maplet name -> number to the mapping.
        * post: book = book0 ⊕ {name → number} */
        def store(name: String, number: String, occurances: Int) : Unit
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
    class BookNode2(val name : String, var number : String, var occurances: Int, var next : BookNode2) 
    class Phonebook2 extends Book2 { 
        //state: data : seq Int
        //default book with a dummy header 
        //DTI := book = BookNode2("","?",0,_) (dummy header is always at the front) && book.(next)^i.occurances >= book(.next)^(i+1).occurances
        //&& book(.next)^i.occurances is the number of times book(.next).names has been recalled, with its default value (when first inserted) being 0 (needs to be made clear from the input side) && if there is an update to the number, we keep the # of occurances assosiated to that name
        //default book with a dummy header 
        //s := set of elements in book(.next)^k, k > 0 
        //abstraction function: Book x = {error if x is not an element of s, returns book_t.number where book_t.name == x and book_t = book(.next)^k for some integer k}
        //initialise by dummy header <=> s = []
        var book = new BookNode2("", "?", 0, null)

        //Invariance is almost exact from BookNode with a few minor changes
        def store(names: String, numbers: String, occ: Int) : Unit = {
            var book_t = book
            //if names is in book, we just want to update the number
            if (isInBook(names)) {
                //search through book linearly
                while(book_t.name != names) book_t = book_t.next
                //update the number whose entry book_t.name == names
                //override the occ in the function by what was already in book_t.occurances
                book_t.number = numbers 
                return 
            } 
            //else searhc through book just like in question 3, but instead of ordering around names, order around the occurances
            //I := book(.next)^i.next.occurances > occ where i is the number of times the while loop has been excecuted && book_t = book(.next)^i
            while(book_t.next != null && book_t.next.occurances > occ) {
                book_t = book_t.next 
            }   
            //I and not G => book_t.occurances > occ >= book_t.next.occurances, so we just want to insert the new node in between these two
            var in = new BookNode2(names,numbers,occ,book_t.next)
            //update where the link of book_t.next points to
            book_t.next = in
        }

        def recall(names: String) : String = {
            require(isInBook(names)) //make sure the book contains the given name (meet precondition)
            var book_t = book 
            //search until book_t.name == names (precondition gurantees this will happen)
            while(book_t.name != names) {
                book_t = book_t.next 
            }
            //we will return the number associated with book_t.number
            var ret = book_t.number
            var v = book_t.occurances
            //store ret and v as deleting name cause a loss in access to these values
            delete(names) //delete names placed in the book with one less occurance than we want, and then reinsert the same names with one extra occurance
            store(names,ret,v+1)
            return ret 
        }

        def isInBook(names: String) : Boolean = {
            var book_t = book 
            //search through the entire list until book_t.name == names or no such entry occurs and we reach the end of the list
            while(book_t != null && book_t.name != names) {
                book_t = book_t.next 
            }
            //distinguish between whether names is in book by checking whether the loop reached the end, or we found it somewhere earlier
            return book_t != null 
        }

        def delete(names: String) : Boolean = {
            var book_t = book 
            while(book_t.next != null) {//search through the entire book
                // println(book_t.next.name)
                if (book_t.next.name == names) {
                    //if we find an entry in book with book_t.next.name == names, delete book_t.next from book by setting book_t.next = book_t.next.next (skipping that entry in the link)
                    book_t.next = book_t.next.next
                    return true //return true for deleting names
                } 
                book_t = book_t.next
            }  
            return false //return false if the entry was not found in book
        }
    }

    //Question 5
    /** A queue of data of type A.
    * state: q : seq A
    * init: q = [] */
    trait Queue[A]{
        /** Add x to the back of the queue
        * post: q = q0 ++ [x] */
        def enqueue(x: A) : Unit
        /** Remove and return the first element.
        * pre: q 6= []
        * post: q = tail q0 ∧ returns head q0
        * or post: returns x s.t. q0 = [x] ++ q */
        def dequeue(): A
        /** Is the queue empty?
        * post: q = q0 ∧ returns q = [] */
        def isEmpty: Boolean
        /** Is the queue full? 
        * post: q = q0 && returns whether queue is full (with respect to the data structure being used) */
        def isFull : Boolean
    }
    class ArrayQueue extends Queue[Int] {
        //state: data : seq Int
        //abstraction function: if L <= R, data[L..R) = queue, else data[L..MAX) ++ data[0..R) = queue (with ordering respect to the array)
        //DTI: 0 <= L,R < 100 && 0 <= cnt < 100 &&
        //for all elements i in [L..R) (if L <= R), [L..MAX) ++ [0..R) (else), data[i] are distinct elements of the queue, with ordering respect to the abstraction function &&
        //cnt = R-L if L <= R, 100-(L-R) otherwise
        val MAX = 100 
        var data = new Array[Int](MAX)
        var L = 0; var R = 0;
        var cnt = 0 //keeps track of the size of the queue (to distignuish between cases of 0 elements and 100 elements in the queue)
        //data points are in the range [L,R) if L <= R and in the range [L..MAX) || [0..R) otherwise 

        def enqueue(x: Int) : Unit = {
            require(cnt < 100);
            data(R) = x //puts data in the back of the queue
            R = (R+1)%100 //increments R in mod 100 
            cnt+=1 //ensures DTI is kept
        }
        def dequeue() : Int = {
            require(cnt != 0)
            L = (L+1)%100 //increment in mod 100 s.t L and R after dequeue meets our DTI & postconditions 
            cnt -= 1 //ensured DTI is kept
            return data((L+99)%100) //returns the element in L_0
        }
        def isEmpty: Boolean = {
            return cnt == 0 //returns whether queue consists of cnt elements
        }
        def isFull : Boolean = {
            return cnt == MAX
        }
    }
    
    //Question 6
    class QueueNode(var num : Int, var next : QueueNode, var prev : QueueNode)
    class IntQueue extends Queue[Int] {
        //state: data : seq Int
        //abstraction function: last.num = queue.front && elements following last(.prev)^k are elements of the queue in that order
        //DTI: data = QueueNode(0,_,null) (has a dummy head) && data(.next)^i = last(.prev)^k-i for i in [1..k) where k is the number of elements in the queue (k+1 elements in the linked list)
        //init: data = QueueNode(0,null,null) := queue = []
        var data = new QueueNode(0,null,null) //initialise with a dummy head
        var last = data 
        def enqueue(x : Int) : Unit = {
            var in = new QueueNode(x,data.next,data)
            //create a node s.t. it falls under the dummy head and the rest of the data
            if (data.next != null)
                data.next.prev = in //if data.next is not null, we set the previous element accordingly
            data.next = in 
            if (last == data) last = in //if the last element is the dummy variable, set last to the first element
        }
        def dequeue() : Int = {
            require(last.prev != null) //ensures that the queue is not empty, checking with last
            var x = last.num //stores what we are to return
            last = last.prev //move to the 2nd in front of the queue
            last.next = null //deletes the 1st element of queue
            return x 
        }
        def isEmpty : Boolean = {
            return data.next == null //no elements apart from the dummy head is not part of the linked list
        }
        def isFull : Boolean = {
            return false //linked list never full
        }
    }

    //Question 7
    class DoubleNode(var datum: Int, var prev: Node, var next: Node)
    class DoubleEndedQueue {
        //state fst : seq Int
        //init: set two dummy variables for fst := queue = [] 
        //abstract function : deque(i) = fst(.next)^i+1.datum && deque(n-1-i) = lst(.prev)^i+1.datum
        //DTI: fst = DoubleNode(0,null,_) && lst = DoubleNode(0,_,null) &&
        // fst(.next)^i+1 = lst(.prev)^(n-1-i) for all i in [0..n) &&
        //fst(.next)^i.next.prev = fst(.next)^i 
        var fst = new DoubleNode(0,null,DoubleNode(0,null,null))
        fst.next.prev = fst 
        var lst = fst.next 
        //initialise by two dummy variables for the right and left
        /** Is the queue empty? */
        //post: q = q0 && returns q = []
        def isEmpty : Boolean = {
            return fst.next == lst //returns whether the linked list only has the dummy variables
        }
        /** add x to the start of the queue. */
        //post: q = [x] ++ q_0
        def addLeft(x:Int) : Unit = {
            var in = new DoubleNode(x,fst,fst.next)
            fst.next.prev = in //puts in between fst and fst.next
            fst.next = in //ensures the linked list is linked on both sides
        }
        /** get and remove element from the start of the queue. */
        //post: [x] ++ q = q_0
        def getLeft() : Int = {
            var x = fst.next.datum //leftmost element
            fst.next = fst.next.next //deletes the pop'd element by skipping the link, connecting fst.next with the element next to it
            fst.next.prev = fst //adjusts the other side as well
            return x
        }
        /** add element to the end of the queue. */
        //pre : q != []
        //post: q = q_0 ++ [x]
        def addRight(x: Int) : Unit = {
            var in = new DoubleNode(x,lst.prev,lst) //creates a new node to insert to the right
            lst.prev.next = in 
            lst.prev = in //insert the node between lst (dummy variable on right and the element before it)
        }
        /** get and remove element from the end of the queue. */
        //pre : q != []
        //post: q ++ [x] = q_0
        def getRight() : Int = {
            var x = lst.prev.datum //gets value of leftmost element \ dumnmy variable
            //unlinks lst.prev 
            lst.prev = lst.prev.prev 
            lst.prev.next = lst 
        }
    }

    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)

        //Quick debug for Question 
        println("test for Question 1")
        var test = new Node(3,null)
        var test2 = new Node(4,test) 
        println(test2.toString)

        //Quick debug for Question 3
        var test3 = new Phonebook
        println("test for Question 3")
        println(test3.isInBook("aaa"))
        test3.store("ccc","333")
        test3.store("aaa","111")
        test3.store("bbb","222")
        println(test3.recall("bbb"))
        test3.store("bbb","444")
        println(test3.recall("bbb"))
        println(test3.delete("ddd"))
        println(test3.delete("aaa"))
        println(test3.isInBook("aaa"))

        //Quick debug for Question 4
        var test3_2 = new Phonebook2
        println("test for Question 4")
        println(test3_2.isInBook("aaa"))
        test3_2.store("ccc","333",0)
        test3_2.store("aaa","111",0)
        test3_2.store("bbb","222",0)
        println(test3_2.recall("bbb"))
        test3_2.store("bbb","444",0)
        println(test3_2.recall("bbb"))
        println(test3_2.delete("ddd"))
        println(test3_2.delete("aaa"))
        println(test3_2.isInBook("aaa"))

        //Quick test for Question 5
        var test4 = new IntQueue
        println("test for Question 5")
        println(test4.isEmpty)
        test4.enqueue(4)
        println(test4.isEmpty)
        test4.enqueue(6)
        println(test4.isEmpty)
        println(test4.dequeue())
        test4.enqueue(9)
        println(test4.dequeue())
        println(test4.dequeue())
        println(test4.isEmpty) 

        //Quick test for Question 6
        var test5 = new IntQueue
        println("test for Question 6")
        println(test5.isEmpty)
        test5.enqueue(4)
        println(test5.isEmpty)
        test5.enqueue(6)
        println(test5.isEmpty)
        println(test5.dequeue())
        test5.enqueue(9)
        println(test5.dequeue())
        println(test5.dequeue())
        println(test5.isEmpty)        
    }
}