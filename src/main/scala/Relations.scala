package midterm

import scala.collection.mutable.{Map => MutableMap, HashMap}

object Relations {
  /** This class represents a CNF formula corresponding to some expression.
   *
   *  The list clauses contains the clauses in the formula.  The map is a map
   *  from nodes in the expression to the formula.
   *
   *  NOTE: You do not need to use this structure if you don't like it.
   **/
  case class CNF(clauses: List[Clause], map: Map[Expr, Literal])

  /** This is a function that converts an expression to CNF. 
   *
   *  NOTE: be warned this has not been tested much, so feel free to discard it
   *  and use your own implementation.
   */
  def toCNF(e : Expr, m : Map[Expr, Literal], newLit : () => Literal) : CNF = {
    m.get(e) match {
      case Some(l) => CNF(List.empty, m)
      case None =>
        val l = newLit()
        e match {
          case BooleanLit(v) => 
            val newMap = m + (e -> l) 
            if (v) { CNF(List(new Clause(l)), newMap) }
            else { CNF(List(new Clause(~l)), newMap) }
          case Variable(n) =>
            val newMap = m + (e -> l)
            CNF(List.empty, newMap)
          case And(args) =>
            val result = args.foldLeft(CNF(List.empty, m)){
              (cnf, arg) => {
                val rArg = toCNF(arg, cnf.map, newLit)
                CNF(cnf.clauses ++ rArg.clauses, rArg.map)
              }
            }
            val newMap = (result.map + (e -> l))
            val argLits = (args.map(a => newMap(a)))
            val trueClause = l :: argLits.map(a => ~a).toList
            val falseClauses = argLits.map(a => new Clause(~l, a))
            val clauses = List(new Clause(trueClause.toSet)) ++ falseClauses
            CNF(clauses ++ result.clauses, newMap)
          case Or(args) =>
            val result = args.foldLeft(CNF(List.empty, m)){
              (cnf, arg) => {
                val rArg = toCNF(arg, cnf.map, newLit)
                CNF(cnf.clauses ++ rArg.clauses, rArg.map)
              }
            }
            val newMap = (result.map + (e -> l))
            val argLits = (args.map(a => newMap(a)))
            val falseClause = ~l :: argLits.map(a => a).toList
            val trueClauses = argLits.map(a => new Clause(l, ~a))
            val clauses = List(new Clause(falseClause.toSet)) ++ trueClauses
            CNF(clauses ++ result.clauses, newMap)
          case Not(arg) =>
            val result = toCNF(arg, m, newLit)
            CNF(result.clauses, result.map + (e -> ~(result.map(arg))))
        }
    }
  }

  /**
   * This function substitutes variables in one expression with other variables.
   *
   * For example, suppose:
   *  e = And(List(Variable("a"), Variable("b"))); i.e. (and a b)
   *  m = List(Variable("a) -> Variable("z")).toMap; i.e. { a -> z }
   * Then:
   *  subst(e, m) = And(List(Variable("z"), Variable("b"))); i.e. (and z b)
   *
   *  It is not necessary to implement this function, but it may be useful.
   */
  def subst(e : Expr, subs : Map[Variable, Variable]) : Expr = {
    // TODO
    throw new NotImplementedError("Implement Relations.subst")
  }

  /** Function should return true if the relation whose inputs are the variables
   *  in 'inputs', outputs are the variables in 'output' and whose indicator
   *  function is the expression 'e' is a function, and false otherwise. */
  def isFunction(e : Expr, inputs : Seq[Variable], outputs : Seq[Variable]) : Boolean = {
    // TODO
    true
  }

  /** Function should return true if the relation whose inputs are the
   *  variables in 'inputs', outputs are the variables in 'output' and whose
   *  indicator function is the expression 'e' is a transitive relation, and
   *  false otherwise. */
  def isTransitive(e : Expr, inputs : Seq[Variable], outputs : Seq[Variable]) : Boolean = {
    // TODO
    false
  }
}
