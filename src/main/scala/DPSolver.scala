package midterm

import scala.collection.mutable.ListBuffer

object DPSolver
{
  /** Implement this helper function to resolve two clauses. */
  def resolve(v : Int, c1 : Clause, c2: Clause) : Clause = {
    // TODO
    throw new NotImplementedError("Implement resolve!")
  }
}

class DPSolver
{
  /** Adds a clause to the solver. */
  def addClause(cl : Clause) : Unit = {
    throw new NotImplementedError("Implement DPSolver.addClause")
  }

  /** Solves the current set of clauses.
   *  Returns true if SAT and false if UNSAT. */
  def solve() : Boolean = {
    throw new NotImplementedError("Implement DPSolverEx.solve")
  }
}
