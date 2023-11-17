object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] = {
    val keylen = key.size
    val plainlen = plain.size 
    var cipher = new Array[Char](plainlen)
    for( i <- 0 to plainlen-1) {
      cipher(i) = xor(plain(i),key(i%keylen))
    }
    cipher
  }
    

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) : Unit = {
    val textlen = ciphertext.size
    val criblen = crib.size 
    var teststring = new Array[Char](criblen)

    for( i <- 0 to textlen - criblen - 1) {
      for( j <- 0 to criblen-1) {
        // creates testring = crib[0..K) xor text[i,i+K), where K = criblen
        teststring(j) = xor(crib(j),ciphertext(i+j))
      }
      if(tryString(teststring, ciphertext, i)) return
    }
  }

  def tryString(key: Array[Char], ciphertext: Array[Char], pos: Int) : Boolean = {
    //use to debug to see what the keystring is
    /** 
      var str = new String(key)
      println(str) 
    **/
    for( i <- 1 to key.size - 2) {
      var j = 0
      //check if key[0,K-i) == key[i,K)
      while(i+j < key.size && key(j) == key(i+j)) {
        j += 1
      }
      //do if key[0,K-i) == key[i,K)
      if (i+j == key.size) {
        var ans = new Array[Char](i)
        for( k <- 0 to ans.size) {
          ans((k+pos)%ans.size) = key(k)
        }
        var str = new String(ans)
        println(str)
        print(new String (encrypt(ans,ciphertext)))
      // print(new String (encrypt(key,plain)))
        return true
      }
    }
    return false
  }


  /** The first optional statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) : Unit = {
    var clen = ciphertext.size    

    for( shift <- 1 to 30) {
      //counts the number of times cipher[t] = cipher[shift+t]
      var cnt = 0
      for( i <- 0 to clen-shift-1) {
        if (ciphertext(i) == ciphertext(i+shift)) {
          cnt += 1
        }
      }
      println(shift + ": " + cnt)
    }

  }

  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) : Unit = {

    var clen = ciphertext.size 
    var testlen = klen 
    while(testlen < clen) {
      //tests each multiple of klen 
      for( i <- 0 to clen-testlen-1) {
        if (ciphertext(i) == ciphertext(i+testlen)) {
          var possible_element = xor(ciphertext(i),' ') 
          
          //output character if given choice is a possibility
          if (32 <= possible_element.toInt && possible_element.toInt <= 127) {
            println(i%klen + 1 + " " + possible_element)
          }
        }
      }

      testlen += klen
    }
  }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else readStdin()

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}