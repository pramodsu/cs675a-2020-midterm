package midterm

object Main {
  def main(args: Array[String]) : Unit = {
    println("Run sbt test for the tests.")
    // The following may give you useful information about the words.
    // WordList.info()
    val p = CryptArithmetic.findPuzzle(11)
    println(p.toString)
  }
}
