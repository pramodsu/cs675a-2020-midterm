package midterm

abstract class Literal {
  val variable : Int
  def unary_~() : Literal
  def toInt : Int
}

case class PositiveLiteral(v: Int) extends Literal {
  assert (v > 0)
  override def toString() : String = v.toString()
  override val variable = v
  override def toInt = v
  def unary_~() : Literal = NegativeLiteral(v)
}

case class NegativeLiteral(v :Int) extends Literal {
  assert (v > 0)
  override def toString() : String = "-" + v.toString()
  override val variable = v
  override def toInt = -v
  def unary_~() : Literal = PositiveLiteral(v)
}

object Literal {
  def create(v : Int) = PositiveLiteral(v)
}

class Clause(ls: Set[Literal]) {
  val lits : Set[Literal] = ls

  def this(ls: Literal*) {
    this(ls.toSet)
  }

  override  def toString() : String = 
    lits.map(_.toString()).mkString("(", " + ", ")")

  /** Does this clause contain a specific literal. */
  def contains(l : Literal) : Boolean = 
    lits.contains(l)
  /** Does this clause contain a specific variable. */
  def contains(v : Int) : Boolean =
    lits.exists(l => l.variable == v)
  /** Filter the literals in this clause. */
  def filter(p : Literal => Boolean) : Clause = 
    new Clause(lits.filter(p))
  /** Map the literals in this clause. */
  def map(f : Literal => Literal) : Clause =
    new Clause(lits.map(f))
  /** foldLeft over the literals in this clause. */
  def foldLeft[A](init : A)(f : (A, Literal) => A) : A =
    lits.foldLeft(init)((a, b) => f(a,b))
  /** Concatenate this clause with another. */
  def ++(c : Clause) : Clause =
    new Clause(lits ++ c.lits)
  /** Return the number of literals in this clause. */
  def size : Int = lits.size

  /** This function returns true if the clause is trivially satisfiable, i.e.
   *  it contains both v and ~v for something variable v.
   */
  def isTrue() : Boolean = {
    for(l <- lits) {
      if (lits.contains(~l)) return true
    }
    return false
  }

  /** This function returns true if the clause is trivially unsatisfiable, i.e.
   *  it is the empty clause.
   */
  def isFalse() : Boolean = {
    return size == 0
  }

  /** Can this be compared with 'a'? */
  def canEqual(a: Any) = a.isInstanceOf[Clause]

  /** Implement equality comparison. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Clause => that.canEqual(this) && this.lits == that.lits
      case _ => false
    }

  /** Implement a hashCode. */
  override def hashCode: Int = {
    return 31*(lits.hashCode() + 7)
  }
}
