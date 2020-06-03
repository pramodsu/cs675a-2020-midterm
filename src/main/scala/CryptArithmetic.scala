package midterm

import com.microsoft.z3

object CryptArithmetic {
  case class Puzzle(wordA : String, wordB : String, wordC : String, mapping : Map[Char, Int]) {
    override def toString() = 
      wordA + " + " + wordB + " = " + wordC + "; " + 
      mapping.map{ case (k, v) => k.toString + "->" + v.toString }.mkString("", ", ", "")
  }

  /** Return a valid cryptarithmetic puzzle with length at least minLen. */
  def findPuzzle(minLen : Int) : Puzzle = {
    throw new NotImplementedError("Implement findPuzzle.")
  }
}
