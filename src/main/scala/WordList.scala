package midterm

import scala.util.Random

object WordList {
  /** This is the list of words that you must use. Do not change it. */
  val words = Set(
    "have", "estate", "four", "land", "annual", "human", "nokia", "took",
    "code", "easy", "things", "called", "times", "team", "linux", "whole",
    "stay", "none", "call", "very", "along", "listed", "speed", "active",
    "never")

  def info() {
    // # of words is 100
    println("# of words: " + words.size.toString()) 
    // max length?
    println("max length: " + words.foldLeft(0)((m, w) => if (m < w.size) w.size else m).toString())
    // characters in words
    val chars = words.foldLeft(Set.empty[Char])((s, w) => s ++ w.toSet)
    // characters that appear in the words
    println("characters in the words: " + chars.toList.sorted.mkString("", "", ""))
    // number of characters
    println("# of unique chars: " +chars.size.toString())
  }
}
