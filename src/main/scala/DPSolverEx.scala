package midterm

import scala.collection.mutable.ListBuffer

abstract class ProofNode {
  val cl : Clause
}
/** A leaf clause in a resolution proof of UNSAT is a node corresponding to a clause
 *  in the original formula.
 */
case class LeafClause(override val cl: Clause) 
  extends ProofNode

/** A resolved clause node in a resolution proof of UNSAT is a node
 *  corresponding to to result of performing resolution on some two clauses in
 *  the formula.
 */
case class LearnedClause(override val cl: Clause, v : Int, c1 : ProofNode, c2: ProofNode) 
  extends ProofNode

class DPSolverEx
{
  /** Adds a clause to the solver. */
  def addClause(cl : Clause) : Unit = {
    throw new NotImplementedError("Implement DPSolverEx.addClause")
  }

  /** Solves the current set of clauses.
   *  Returns (true, None) if SAT and (false, Some(p)) if UNSAT where p is of
   *  type ProofNode if the problem is UNSAT. p must be a valid resolution
   *  proof of UNSAT.
   **/
  def solve() : (Boolean, Option[ProofNode]) = {
    throw new NotImplementedError("Implement DPSolverEx.solve")
  }
}
