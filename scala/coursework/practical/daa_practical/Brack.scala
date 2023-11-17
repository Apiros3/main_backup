/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 30

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}
	
  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)  
  op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray.init

 
  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z
	
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
		if (i+1 == j) return w(i) == z
		for(p <- 0 until 3) {
			for(q <- 0 until 3) {
				if (op(p)(q) == z) {
					var ret = false 
					for(r <- i+1 until j) {
						ret |= (PossibleRec(w,i,r,p) && PossibleRec(w,r,j,q)) 
					}
					if (ret) return true
				}
			}
		}
		return false 
	} 

	
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		if (i+1 == j) {
			if (w(i) == z) return 1 
			else return 0
		}
		var ret = 0
		
		for(p <- 0 until 3) {
			for(q <- 0 until 3) {
				if (op(p)(q) == z) {
					for(r <- i+1 until j) {
						ret += (NumberRec(w,i,r,p) * NumberRec(w,r,j,q)) 
					}
				}
			}
		}
		return ret
	} 

	
	//TASK 3
	//TODO Runtime analysis of recursive solution along with tests
	/**
	N = 9
	Bracketing values for ABBABABC
	A can be achieved in 200 ways
	B can be achieved in 146 ways
	C can be achieved in 83 ways

	real    0m0.911s
	user    0m1.578s
	sys     0m0.422s

	N = 10
	Bracketing values for ABBABABCB
	A can be achieved in 319 ways
	B can be achieved in 896 ways
	C can be achieved in 215 ways

	real    0m1.158s
	user    0m1.984s
	sys     0m0.531s

	N = 11
	Bracketing values for ABBABABCBA
	A can be achieved in 1790 ways
	B can be achieved in 1513 ways
	C can be achieved in 1559 ways

	real    0m2.207s
	user    0m2.828s
	sys     0m0.281s

	N = 12
	Bracketing values for ABBABABCBAA
	A can be achieved in 4236 ways
	B can be achieved in 9174 ways
	C can be achieved in 3386 ways

	real    0m10.433s
	user    0m11.047s
	sys     0m0.594s


	# of possible bracketing takes Catalan numbers
	C(n) = (2n)!/(n!(n+1)!) = approx 4^n/sqrt(n^3*pi)
	So we can expect to see the running time to be at least exponential

	Define T(n) as the # of operations
	Then, T(n) >= 6*sum(T(i) : i in [0,n)), T(1) = O(1)
	So T(n-1) >= 6*sum(T(i) : i in [0,n-1))
	This gives T(n) - T(n-1) >= 6*T(n-1)
	Therefore T(n) >= 7*T(n-1), so T(n) >= O(7^n)

	*/
	
	
	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree) {
		t match {
			case Leaf(value) => {
				print(value)
			}
			case Node(left,right) => {
				print("(")
				print_tree(left)
				print_tree(right) 
				print(")") 
			}
		}
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution
	
	
	def Tabulate_pos(w: Array[Int], n: Int): Unit = {
		for(i <- 0 until n) {//len = 1, poss(i)(i+1)(w(i)) = 1
			poss(i)(i+1)(w(i)) = true 
		}
		for(len <- 2 to n) { //for each size
			for(i <- 0 to n-len) { //starting point i, where j = i+len
			//we will try to find whether the bracketing of w[i..j) evaluating to 0,1,2 exists 
				for(k <- i+1 until i+len) { //we will consider the possible result from bracketing w[i..k) and w[k..j) 
					for(p <- 0 until 3) {
						for(q <- 0 until 3) {
							if (poss(i)(k)(p) && poss(k)(i+len)(q)) {
								poss(i)(i+len)(op(p)(q)) = true
							}
						}
					}
				}
			}
		}
	}

	def Tabulate_cnt(w: Array[Int], n: Int): Unit = {
		for(i <- 0 until n) {//len = 1, poss(i)(i+1)(w(i)) = 1
			ways(i)(i+1)(w(i)) = 1 
		}
		for(len <- 2 to n) { //for each size
			for(i <- 0 to n-len) { //starting point i, where j = i+len
			//we will try to find whether the bracketing of w[i..j) evaluating to 0,1,2 exists 
				for(k <- i+1 until i+len) { //we will consider the possible result from bracketing w[i..k) and w[k..j) 
					for(p <- 0 until 3) {
						for(q <- 0 until 3) {
							ways(i)(i+len)(op(p)(q)) += (ways(i)(k)(p) * ways(k)(i+len)(q))
						}
					}
				}
			}
		}
	}

	def Tabulate(w: Array[Int], n: Int): Unit = {
		for(i <- 0 until n) {//len = 1, poss(i)(i+1)(w(i)) = 1
			ways(i)(i+1)(w(i)) = 1 
			exp(i)(i+1)(w(i)) = Leaf((('A').toInt + w(i)).toChar)
		}
		for(len <- 2 to n) { //for each size
			for(i <- 0 to n-len) { //starting point i, where j = i+len
			//we will try to find whether the bracketing of w[i..j) evaluating to 0,1,2 exists 
				for(k <- i+1 until i+len) { //we will consider the possible result from bracketing w[i..k) and w[k..j) 
					for(p <- 0 until 3) {
						for(q <- 0 until 3) {
							if (ways(i)(k)(p) * ways(k)(i+len)(q) != 0) {
								ways(i)(i+len)(op(p)(q)) += (ways(i)(k)(p) * ways(k)(i+len)(q))
								if (exp(i)(i+len)(op(p)(q)) == null) //prioritize on first find (match example from practical sheet)
									exp(i)(i+len)(op(p)(q)) = Node(exp(i)(k)(p),exp(k)(i+len)(q))
							}
						}
					}
				}
			}
		}
	}

	//Task 6
	//TODO Runtime analysis of dynamic programming version with tests
	/**
	We expect time complexity to be approximately O(N^3)

	N = 4
	ABBA
	Bracketing values for ABBA
	A can be achieved 2 ways
	For example:(A((BB)A))
	B can be achieved 1 way
	For example:(A(B(BA)))
	C can be achieved 2 ways
	For example:((A(BB))A)

	real    0m1.650s
	user    0m0.500s
	sys     0m0.141s

	N = 10
	ABABABCACB
	Bracketing values for ABABABCACB
	A can be achieved 1252 ways
	For example:(A(B(A(B(A(B(C(A(CB)))))))))
	B can be achieved 2693 ways
	For example:(A(B((AB)(A(B(C(A(CB))))))))
	C can be achieved 917 ways
	For example:((AB)(A(B(A(B(C(A(CB))))))))

	real    0m10.201s
	user    0m0.656s
	sys     0m0.125s

	N = 20
	ABCACBCCCBBBAAABCACC
	Bracketing values for ABCACBCCCBBBAAABCACC
	A can be achieved 869526317 ways
	For example:(A(B(C(A(C(B((C(C(C(B((BB)A)))))(A((AB)(C(A(CC))))))))))))
	B can be achieved 495303513 ways
	For example:(A(B(C(A(C(B(C((C(C(B((BB)A))))(A((AB)(C(A(CC)))))))))))))
	C can be achieved 402433360 ways
	For example:((AB)(C(A(C(B((C(C(C(B((BB)A)))))(A((AB)(C(A(CC)))))))))))

	real    0m12.848s
	user    0m1.016s
	sys     0m0.141s

	N = 28 
	ABCABCAAAABBBBBBBBBAAAAAAAAA
	Bracketing values for ABCABCAAAABBBBBBBBBAAAAAAAAA
	A can be achieved 302624296 ways
	For example:(A(B(C(A(B(C(A((AA)(A(B(B(B(B(B(B(B(B(B(A((AA)(A(((AA)A)(AA)))))))))))))))))))))))
	B can be achieved 1552811263 ways
	For example:(A(B(C(A(B(C(A(A((AA)(B(B(B(B(B(B(B(B(B((AA)(A((AA)(A((AA)A)))))))))))))))))))))))
	C can be achieved 469925501 ways
	For example:((AB)(C(A(B(C(A((AA)(A(B(B(B(B(B(B(B(B(B(A((AA)(A(((AA)A)(AA))))))))))))))))))))))

	real    0m10.816s
	user    0m1.172s
	sys     0m0.328s

	In conclusion, DP works much better for larger N

	*/

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString = 
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"
		
		if (args.length > 2){
			println(errString)
			sys.exit
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit;
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}
		
		//Executing appropriate command
    if(command=="-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			if(PossibleRec(plainInt, 0, len, i)){
				println(('A'.toInt + i).toChar + " is Possible");
			}
			else{
				println(('A'.toInt + i).toChar + " is not Possible");
			}
		}
    }
    else if(command=="-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			if(z == 1){
				printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
			}
			else{
				printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }      
    else println(errString)
  }
}


