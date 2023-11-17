
object IP5 {

    var MAX = 100
    var MAX_T = 1000000

    //Question 1
    //yes, it would be much better. For instance, we can replace the linked list with a binary tree so that we have logarithmic search, insertion, and deletion time, instead of linear search, linear insertion, and linear deletion (as for any of the operations we need to search through the linked list to see if the element is already in the list or not)

    //Question 3
    def hash_int(x: Int) : Int = {
        return x%MAX
    }
    class chain_hash {
        /**
        DTI: cnt = #of a(i) != 0 for i in [0..MAX) &&
             for any element in the table, it can be accessed by incrementing (in mod MAX) from its hash value, and all iterators it will go through in this implementation either satisfies a(i) != 0 or d(i) (value is taken up by some other element, or slot had an element that was deleted) && (a(i) = 0) <= d(i)
        */
        var table = new Array[Int](MAX)
        var a = new Array[Int](MAX)
        var d = new Array[Boolean](MAX) //whether space is a deleted space or not
        var cnt = 0

        def find(x: Int) : Int = {
            //search from the hash of x until either the searching index is empty 
            var x_hash = hash_int(x)
            var i = x_hash
            while((d(i) || a(i) != 0) && table(i) != x && (i+1)%MAX != x_hash) {
                i = (i+1)%MAX 
            }
            //breaks the loop if either: i has encountered an empty slot, i has encountered x, we have looped through the entire table 
            return i 
        }

        def add(x: Int) : Boolean = {
            require(cnt < MAX)
            var i = find(x)
            //increment count if x exists
            if (table(i) == x && a(i) != 0) {
                a(i) += 1
                return false 
            }
            else {
                //search for a slot that is empty and add the new element there
                while(a(i) != 0 && !d(i)) i = (i+1)%MAX  
                table(i) = x 
                a(i) = 0
                d(i) = false 
                return true 
            }
            cnt += 1
        }
        def count(x: Int) : Int = {
            var i = find(x)
            //if element exists in the hash table, return a(i), else return 0
            if (table(i) == x && a(i) != 0) {
                return a(i)
            }
            else {
                return 0
            }
        }
        def delete(x: Int) : Boolean = {
            var i = find(x)
            //if element exists in the hash table, delete the element 
            if (table(i) == x && a(i) != 0) {
                a(i) = 0 //deletes element from table 
                //loop to maintain DTI s.t. other functions can be called efficiently 
                d(i) = true //note that it is a slot with a deleted element

                //closing gaps will not change the asymptotic complexity and will complicate the implementation so we will use the simple form (to avoid edge cases and keep readability)
        
                cnt -= 1
                return true  
            }
            return false
        }

    }

    //Question 4 
    class Tree(var word: String, var left: Tree, var right: Tree)
    // Redefined Tree to save you writing "new Tree(...)"
    def Tree(word: String, left: Tree, right: Tree) = new Tree(word, left, right)

    def printTree(tree: Tree) : Unit = {
        var cnt = 0
        //define the print function for the subtree that will be used recursively
        def printSubTree(tr: Tree, i : Int) : Unit = {
            //first print itself (or null if the tree is null) and then the left and right trees afterwards
            spacing(i)
            if (tr == null) println("null")
            else {
                println(tr.word) 
                printSubTree(tr.left,i+1)
                printSubTree(tr.right,i+1)
            } 
        }
        def spacing(x : Int) : Unit = {
            var st = " . "
            print(st * x)
        }
        //print the tree, with the spacing starting from 0
        printSubTree(tree, 0)
    }

    def printTree2(tree: Tree) : Unit = {
        var stack = new scala.collection.mutable.Stack[(Tree,Int)]
        //pushes the entire tree to stack
        stack.push((tree,0))
        //until the stack is empty, continuously print each subtree, and keep adding its subtree to the stack if it is not null
        while(!stack.isEmpty) {
            var (t1,t2) = stack.pop()
            spacing(t2) //space about the designated spacing
            if (t1 == null) println("null") //cout null and do nothing else if the tree is empty
            else {
                println(t1.word) //else print the word on the node
                //push subtrees into the stack such that the left tree is found first when we start to pop the stack
                stack.push((t1.right,t2+1))
                stack.push((t1.left,t2+1))
            }
        }

        def spacing(x : Int) : Unit = {
            var st = " . "
            print(st * x)
        }
    }

    //Question 5
    def flip(t: Tree) : Unit = {
        if (t == null) return 

        //exchange the left and right branches
        var tmp = t.left 
        t.left = t.right 
        t.right = tmp 

        //call the function recursively on both the left and right branches
        flip(t.left)
        flip(t.right)
        //alternatively we could implement this with a stack, with the above two lines flipped
    }

    //Question 6
    class Tree2(var word: String, var count: Int, var left: Tree2, var right: Tree2)
    //straightforward implementation of summing count recursively
    def sumcount(t: Tree2) : Int = {
        if (t == null) return 0
        return t.count + sumcount(t.left) + sumcount(t.right)
    }

    def sumcount2(t: Tree2) : Int = {
        var stack = new scala.collection.mutable.Stack[Tree2]
        stack.push(t)
        var cnt = 0

        //define c(x) to be the count of that tree
        //Invariant I := c(t) = cnt + sum(c(i) : i in stack)
        //Variant I := sum(c(i) : i in stack) 
        while(!stack.isEmpty) {
            var tmp = stack.pop()
            if (tmp != null) {
                //use the fact that c(i) = i.count + c(i.left) + c(i.right)
                cnt += tmp.count 
                stack.push(tmp.left)
                stack.push(tmp.right)
            } 
        }
        //I and not G -> cnt = c(t)
        return cnt 
    }

    //Question 7
    //The dictionary that we will use will be the knuth_words.txt  
    //return all permutations of string

    def anagram(dict: Array[String], key: String) : Unit = {
        var st = new scala.collection.mutable.HashSet[String]
        var ret = new scala.collection.mutable.HashSet[String]
        
        //add every element into set 
        var i = 0; var n = dict.size 
        while(i < n) {
            st.add(dict(i))
            i += 1
        } 

        //s++c is a permutation of key
        def stringperm(s: Array[Char], c: Array[Char]) : Unit = {
            var tm = s.mkString("")
            //checks if s is a word or not
            if (st.contains(tm)) ret.add(tm)
            var j = 0; var N = c.size 
            while(j < N) {
                //otherwise consider pushing any char from c to s and call stringperm recursively 
                var tmp = c.slice(0,j) ++ c.slice(j+1,N)
                stringperm(s++Array(c(j)),tmp)
                j += 1
            }
        }
        
        stringperm(Array(),key.toCharArray())
        //program runs slower on long strings as there are n! permutations of the string
        println(ret)
    } 

    def anagram2(dict: Array[String], key: String) : Unit = {
        var mp = new Array[(String,String)](0)
        var ret = new scala.collection.mutable.HashSet[String]

        var i = 0
        //put each element of dict into array with the sorted in fst and the original in snd
        while(i < dict.size) {
            var tmp = dict(i).toCharArray.sorted.mkString("")
            mp = mp ++ Array((tmp,dict(i)))
            i += 1
        }
        mp = mp.sorted
        def getallkeys(x: String) : Unit = {
            var l1 = 0; var r1 = mp.size 
            //binary search to find l1 s.t. (i <= l1) => mp(i)(0) < x (and else >= x)
            while(l1+1 < r1) {
                var m = (l1+r1)/2 
                var (t1,t2) = mp(m)
                if (t1 < x) l1 = m 
                else r1 = m 
            }
            var l2 = 0; var r2 = mp.size
            //binary search to find l1 s.t. (i <= l1) => mp(i)(0) <= x (and else > x)
            while(l2+1 < r2) {
                var m = (l2+r2)/2
                var (t1,t2) = mp(m)
                if (t1 <= x) l2 = m 
                else r2 = m 
            }
            var i = l1+1
            while(i <= l2) {
                var (t1,t2) = mp(i)
                ret.add(t2)
                i += 1
            }
        }
        
        def stringperm(s: String, c: Array[Char]) : Unit = {
            getallkeys(s)
            if (c.size > 0) {
                var tmp = c.slice(1,c.size)
                //search with version where we decide to use the next element in c and version where we decide not to
                stringperm(s + c(0).toString,tmp)
                stringperm(s,tmp)
            }
        }
        stringperm("",key.toCharArray.sorted)
        println(ret)
    }

    def longestanagram(dict: Array[String]) : Unit = {
        require(dict.size > 1)
        var mp = new Array[(String,String)](0)

        var i = 0
        //same as before
        while(i < dict.size) {
            var tmp = dict(i).toCharArray.sorted.mkString("")
            mp = mp ++ Array((tmp,dict(i)))
            i += 1
        }
        mp = mp.sorted

        var mxstr = ""
        i = 1
        //I := mxstr.size >= mp(j)(0).size for all j in [1..i) s.t. mp(j)(0) = mp(j-1)(0) 
        while(i < mp.size) {
            var (t1,t2) = mp(i-1); var (s1,s2) = mp(i)
            if (t1 == s1 && mxstr.size < t1.size) {
                mxstr = t1
            }
            i += 1
        }        
        i = 0
        while(i < mp.size) {
            //look through mp and if the sorted element is mxstr, print them
            var (t1,t2) = mp(i)
            if (t1 == mxstr) println(t2)
            i += 1
        }
    }

    def largestanagram(dict: Array[String]) : Unit = {
        require(dict.size > 1)
        var mp = new Array[(String,String)](0)
        var i = 0
        //same as before
        while(i < dict.size) {
            var tmp = dict(i).toCharArray.sorted.mkString("")
            mp = mp ++ Array((tmp,dict(i)))
            i += 1
        }
        mp = mp.sorted

        var mxl = 0; var mxr = 1; var mxlen = 1
        i = 1
        var curl = 0; var curr = 1;
        //Invariant I := mp[curl,curr) have the same sorted key && mp[mxl,mxr) is the longest sequence of equal sorted keys in the range [0..i) && mxlen = mxr-mxl && curr = i
        while(i < mp.size) {
            //look through every element, updating the longest sequence currently of equal elements in curl and curr
            var (t1,t2) = mp(i-1); var (s1,s2) = mp(i)
            if (t1 == s1) {
                curr += 1
                if (curr-curl > mxlen) {
                    //if mxlen is smaller than the current range, update mxl and mxr
                    mxl = curl
                    mxr = curr
                    mxlen = mxr-mxl 
                }
            }
            else {
                //otherwise reset our current running sequence
                curl = i 
                curr = i+1
            }
            i += 1
        }

        //now we just want to print elements in [mxl,mxr)
        while(mxl < mxr) {
            var (t1,t2) = mp(mxl)
            println(t2)
            mxl += 1
        }

    }

    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)

        //Debugging for Question 4 / 5
        var tr = Tree("three", Tree("four", Tree("five",null,null), Tree("six",Tree("seven", Tree("one",null,null), null), null)), Tree("two",null,null))
        printTree2(tr)
        flip(tr)
        printTree2(tr)
        
        //Debugging for Question 7
        var allWords = scala.io.Source.fromFile("knuth_words.txt").getLines().toArray 
        anagram(allWords,"happiness")
        println()
        anagram2(allWords,"happiness")
        longestanagram(allWords)
        largestanagram(allWords)
    }
}