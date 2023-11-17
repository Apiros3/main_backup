

object ip {

    def swap(a: Array[Int], l: Int, r: Int) : Unit = {
        var tmp = a(l)
        a(l) = a(r)
        a(r) = tmp 
    }
    
    def partition(a: Array[Int], l: Int, r: Int) : Int = {
        var L = l+1; var R = r-1;
        var x = a(l)
        while(L <= R) {
            if (a(L) < x) L += 1
            else {
                swap(a,L,R) 
                R -= 1;
            }
        }
        swap(a,L-1,l)
        return L-1;
    }

    def partitionDual(a: Array[Int], l:Int, r:Int) : (Int,Int) = {
        require(l+1 < r)
        var L = l+1; var R = r-2;
        var M = l+1;
        if (a(l) > a(r-1))
            swap(a,l,r-1)            
        var x = a(l); var y = a(r-1);
        while(M <= R) {
            var tmp = a(M)
            if (tmp < x) {
                swap(a,M,L)
                M += 1
                L += 1 
            } 
            else if (tmp < y) {
                M += 1
            }
            else {
                swap(a,M,R)
                R -= 1
            }
            // for (i <- a) print(i + ",")
            // println()
            // println(L + " " + M + " " + R)
        }
        swap(a,l,L-1)
        swap(a,r-1,R+1)
        return (L-1,R+1);
    }

    def sortDual(a:Array[Int], l:Int, r:Int) : Unit = {
        var (u,v) = partitionDual(a,l,r)
        if (l+1 < u) sortDual(a,l,u)
        if (u+2 < v) sortDual(a,u+1,v)
        if (v+2 < r)   sortDual(a,v+1,r)    
    }

     class Trie {
        var count = 0 
        val children = new Array[Trie](26)
    }

    class TrieBag{
        private var root:Trie = new Trie
        private def toIndex(c: Char):Int = c.toInt - 'a'.toInt
        private def toChar(i: Int ):Char = (i+'a'.toInt ).toChar
        def count(word: String):Int = {
            var curr = root 
            for (i <- 0 until word.size) {
                if (curr.children(toIndex(word(i))) == null) return 0 
                curr = curr.children(toIndex(word(i)))
            }
            return curr.count 
        }
        def find(word: String) : Trie = {
            var curr = root 
            for (i <- word) {
                if (curr.children(toIndex(i)) == null) curr.children(toIndex(i)) = new Trie
                curr = curr.children(toIndex(i)) 
            }
            return curr 
        }
        def add(word: String):Unit = {
            var curr = find(word)            
            curr.count += 1 
        }
        def delete(word: String):Unit = {
            var curr = find(word)
            if (curr.count > 0) curr.count -= 1
        }
        def dfs(word: String, curr: Trie) : Unit = {
            for (i <- 0 until 26) {
                if (curr.children(i) != null) {
                    if (curr.children(i).count > 0) println (word + toChar(i)) 
                    dfs(word+toChar(i),curr.children(i))   
                }
            }
        }
        def printBag() : Unit = {
            dfs("",root)
        }
    }

    def main(args: Array[String]) : Unit = {
        // println("Running...")
        // var b = Array(7,2,6,4,2,2,6,2,6,4,5,7,4,8,9,4,3,2,3)
        // sortDual(b,0,b.size)
        // for (i <- b) print(i)

        // var S = "aaa"
        // S = S + 'a'
        // println(S)

        var tm = new TrieBag 
        tm.add("abc")
        println(tm.count("abc"))
        tm.add("bac")
        tm.add("ba")
        tm.add("a")
        tm.printBag()

        tm.delete("abc")
        println(tm.count("abc"))

    }
}

// class Tree(var data: Int, var left: Tree, var right:Tree)