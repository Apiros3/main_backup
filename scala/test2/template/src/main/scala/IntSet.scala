
// A class of objects to represent a set
class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet : Node = Node(0,null) // or however empty set is represented

  /**
  abstraction: S = {L(i).datum : i in 1 until N} where N is the size of L 
  DTI: cnt = |S|, L(i) is sorted, L(i) contains no duplicates, first node (theSet.datum is a dummy value), the linked list contains no loops, and can access all elements in L starting from theSet and moving through next
  */

  //dummy header will be used to deal with deletion and more efficient implementation (asymptotic complexity does not change)
  //we will make the nodes sorted to deal with faster computation for the equals, subset, union and intersection in later sections, as asymptotic complexity searching through the linked list will be O(N) either way

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
    //we will choose to implement set such that there are no duplicates in the linked list => we can obtain toString in O(N) instead of O(NlogN) by making the addition of elements into the set O(N) instead of O(1)
    //Also, this way, the size of the set is easily managable (the implementation will have to face with duplicates somewhere, so we just decide to do this before adding anything to the set (also saves))
  override def toString : String = { 
    if (theSet.next == null) return "{}"
    var S = "{"
    var tmp = theSet
    while(tmp.next != null) {
      S += tmp.next.datum.toString + ", "
      tmp = tmp.next 
    }
    return S.dropRight(2) + "}"
  }

  //keeps track of how many elements are in the set
  var cnt = 0

  /** Find whether given element is in the set or not
    * Post: S = S_0 ^ returns e ∊ S */
  //time complexity O(N), going through each element of theSet
  private def find(e: Int) : Node = { 
    var tmp = theSet //first element of set
    while(tmp.next != null && tmp.next.datum < e) {
      tmp = tmp.next
    }
    //I and not G => T(i).datum < e for T(i) <= tmp and T(i).datum >= e for T(i) > tmp
    //tmp is s.t. tmp.next == null if we went through the entire list and couldnt find e, or tmp.next.datum >= e if we found the node with the possibility of it being e
    return tmp
  }
  /** Add element e to the set
    * Post: S = S_0 U {e} */
  //bottleneck is the find, taking O(N) time to add an element (if we toggle the function slightly, we can add k elements in O(N + klogk), (sorting elements, then running through the set to see where we want to place our element, so we can add multiple elements in near-linear time))
  def add(e: Int) : Unit = {
    var tmp = find(e)
    //only add e in the case that we couldn't find it in the set
    if (tmp.next == null || tmp.next.datum != e) {//tmp.datum < e < tmp.next.datum
      //implementation to place the node between tmp and tmp.next 
      var a = Node(e,tmp.next)
      tmp.next = a
      //account for DTI
      cnt += 1
    }
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
    //cnt is a stored value so returns in O(1)
  def size : Int = {
    //simply returns the size of the linked list, stored as a value of cnt
    return cnt 
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
    //bottleneck is finding the element, so takes O(N) time
  def contains(e: Int) : Boolean = {
    //returns whether find(e).next is null (didn't find element until the end) or find(e).next.datum == e 
    var tmp = find(e).next 
    if (tmp == null) return false 
    return tmp.datum == e 
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
    //if precondition is held, returns the first element in O(1)
  def any : Int = {
    require(cnt != 0) //require precondition is held
    //returns first element for simplicity
    return theSet.next.datum 
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  //bottleneck is sorting used in f, so takes at most O(NlogN) time
  //function to convert intset to sorted array, given the first node
  private def f_array(st: Node) : Array[Int] = {
    var a = new scala.collection.mutable.ArrayBuffer[Int](0)
    //simply appends all elements into arraybuffer then converts it to an array (already sorted by DTI)
    var tmp = st.next 
    while(tmp != null) {
      a += tmp.datum 
      tmp = tmp.next
    }
    return a.toArray
  }

  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
      var that_a = f_array(s.theSet)
      if (that_a.size != cnt) return false 
      var set_a = f_array(theSet)
      //compare each element, if at any point the two elements differ, return false (if we can reach the end, return true )
      for(i <- 0 until cnt) {
        if (that_a(i) != set_a(i)) return false 
      }
      return true 
    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  //bottleneck is the find function taking O(N)
  def remove(e: Int) : Boolean = {
    var tmp = find(e) 
    if (tmp.next == null || tmp.next.datum != e) return false //element was not found || find reached the end of the linked list without finding anything
    //else tmp.next.datum = e, so we will connect tmp to tmp.next.next 
    tmp.next = tmp.next.next 
    cnt -= 1 //account for DTI 
    return true //as we found e
  }
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
    //bottleneck is using f and the pointer approach, taking O(N+M), where N and M are sizes of the sets
  def subsetOf(that: IntSet) : Boolean = {
    var that_a = f_array(that.theSet)
    var set_a = f_array(theSet)

    //pointer approach starting from the left 
    var L = 0
    for(i <- 0 until that_a.size) { //move the pointer, looking for the element in set_a
      while(L != set_a.size && set_a(L) != that_a(i)) {
        L += 1
      }
      //return L s.t. set_a(L) = that_a(i) or L ==  set_a.size if we couldn't find any (then it is not a subset) 
      if (L == set_a.size) return false  
      L += 1 //as set_a(L) != that_a(i+1)
    }
    return true //loop ends if we have found corresponding L for element in that_a
  }

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
    //pointer approach, choosing whichever is smaller and pushing it as the next node in res
    //takes O(N+M), where N and M are the sizes of the two sets 
  def union(that: IntSet) : IntSet = {
    var ret = new IntSet //initialise ret 
    var st = that.theSet.next; var sm = theSet.next //both pointing to the first element of their respective sets
    var movret = ret.theSet //I:= movret is the last element of ret (or the dummy header)
    def appendwith_sm : Unit = {
      var a = Node(sm.datum,null)
      movret.next = a //append a to the end of ret 
      movret = a //keep invariance
      sm = sm.next 
    }
    def appendwith_st : Unit = {
      var a = Node(st.datum,null)
      movret.next = a //append a to the end of ret 
      movret = a //keep invariance
      st = st.next 
    }
    //Invariance I:= S U that.S = ret.S + (union L(st),L(sm))
    while(st != null || sm != null) {//loop ends if we have reached the end of both sets 
      //case distinction, where either of the sets have reached the end, or the element in consideration is smaller. Special case if the elements are the same
      if (st == null) {
        appendwith_sm
      }
      else if (sm == null) {
        appendwith_st
      }
      else if (st.datum == sm.datum) {
        appendwith_sm //special case where pointer moves for both cases
        st = st.next 
      } 
      else if (st.datum > sm.datum) {
        appendwith_sm
      }
      else {
        appendwith_st
      }
    }  
    return ret //returns completed union set
  }

  //function to insert x after the given node
  private def insertx(x: Int, m: Node) : Unit = {
    var tm = Node(x,null)
    m.next = tm 
  }
  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  //two pointer approach, looking through each set linearly, so takes O(N+M), where N and M are the repective sizes of each set 
  def intersect(that: IntSet) : IntSet = {
    var ret = new IntSet
    var movret = ret.theSet
    var st = that.theSet.next; var sm = theSet.next //both pointing to the first element of their respective sets
    //Invariance: movret points to the last element of the set (or the dummy header if there is none)
    //Invariance I := S and that.S = ret.S + (intersection L(st), L(sm)) && ret.S = filter(x.datum < min(st.datum,sm.datum),x in S and that.S)
    while(st != null && sm != null) { //loop ends when either of the set reaches the end (as then there is no elements left for an intersection)
      if (st.datum < sm.datum) {
        st = st.next 
      }
      else if (st.datum > sm.datum) {
        sm = sm.next 
      }
      else { //datums are equal
        insertx(st.datum,movret)
        movret = movret.next
        st = st.next 
        sm = sm.next 
      }
    }
    return ret 
  }

  
  //function to turn an array into an intset 
  def f_intset(a: Array[Int]) : IntSet = {
    var ret = new IntSet 
    var movret = ret.theSet
    //movret is the last element of the set representation
    var atm = a.sorted
    //inserts x into the last position of the array  
    //insert each distinct element in atm to ret.S
    for(i <- 0 until atm.size) {
      if (i == 0 || atm(i) != atm(i-1)) {
        insertx(atm(i),movret)
        movret = movret.next
      }
    }
    return ret 
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
    //bottleneck of the time complexity is the sorting, so it takes O(NlogN)
  def map(f: Int => Int) : IntSet = {
    //make set to array => apply map => sort => removedup => make back into intset 
    //make set to array
    var a = f_array(theSet)
    //apply map to array 
    for(i <- 0 until a.size) {
      a(i) = f(a(i))
    }
    //sort array
    //make it back to an intset and return it 
    return f_intset(a)
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
    //Bottleneck is the loop to filter, so the code will take O(N) time 
  def filter(p : Int => Boolean) : IntSet = {
    var a = f_array(theSet) //to make code readable
    var ret = new IntSet //initialise returning set
    var movret = ret.theSet //keep movret to insert values

    for(i <- 0 until a.size) {
      if (p(a(i))) { //we put a(i) into ret
        insertx(a(i),movret)
        movret = movret.next 
      }
    }
    return ret 
  }
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}