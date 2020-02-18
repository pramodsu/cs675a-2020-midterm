package midterm

import org.scalatest.flatspec.AnyFlatSpec

/** These are tests for our code. */
class CNFTestSpec extends AnyFlatSpec {
  "sanity checks on isTrue" should "pass" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)
    val e = Literal.create(5)
    val c1 = new Clause(a, ~b, c)
    val c2 = new Clause(~a, b, ~d, e)
    val c5 = new Clause(~d, b, c, d, ~a, e)
    val c6 = new Clause(~d, b, c, d, ~a)
    val c7 = new Clause(d, ~b, b, c, d, ~a)
    val c8 = new Clause(d, b, c, d, ~a, e)
    val c9 = new Clause(d, b, c, d, ~a)
    val c10 = new Clause(d, b, b, c, d, ~a)
    assert (!c1.isTrue)
    assert (!c2.isTrue)
    assert (c5.isTrue)
    assert (c6.isTrue)
    assert (c7.isTrue)
    assert (!c8.isTrue)
    assert (!c9.isTrue)
    assert (!c10.isTrue)
  }
}

/** These are tests for the D-P Solver. */
class DPSolverSpec extends AnyFlatSpec {
  "sanity checks on resolve" should "pass" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)
    val e = Literal.create(5)
    val c1 = new Clause(a, ~b, c)
    val c2 = new Clause(~a, b, ~d, e)
    val c3 = DPSolver.resolve(b.variable, c1, c2)
    val c4 = DPSolver.resolve(a.variable, c1, c2)
    assert (c3.isTrue)
    assert (c4.isTrue)
  }

  "solver test 1" should "be UNSAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val clauses  = Set(
      new Clause(a, b), 
      new Clause(~a, ~b),
      new Clause(a),
      new Clause(b))
        
    val S = new DPSolver()
    clauses.foreach(S.addClause(_))
    val r = S.solve()
    assert (!r)
  }

  "solver test 2" should "be SAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val clauses  = Set(
      new Clause(a, b), 
      new Clause(~a, ~b),
      new Clause(a),
      new Clause(~b))
        
    val S = new DPSolver()
    clauses.foreach(S.addClause(_))
    val r = S.solve()
    assert (r)
  }

  "solver test 3" should "be SAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)

    val clauses  = Set(
      new Clause(a, b), 
      new Clause(~a, ~b),
      new Clause(a),
      new Clause(~b))
        
    val S = new DPSolver()
    clauses.foreach(S.addClause(_))
    val r = S.solve()
    assert (r)
  }

  "solver test 4" should "be SAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)
    val e = Literal.create(5)

    val clauses = Set(
      new Clause(d, c, ~b, ~a, e),
      new Clause(e, ~c, b, ~a, d),
      new Clause(~b, a, ~c, ~e, d),
      new Clause(d, ~a, ~c, e, ~b),
      new Clause(~c, ~b, d, e, ~a),
      new Clause(~b, ~d, ~c, ~a, ~e),
      new Clause(~b, ~a, d, c, ~e),
      new Clause(b, a, ~c, ~e, d))

    val S = new DPSolver()
    clauses.foreach(S.addClause(_))
    val r = S.solve()
    assert (r)
  }

  "solver test 5" should "be UNSAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)
    val e = Literal.create(5)

    val clauses = Set(
                    new Clause(d, ~b, c),
                    new Clause(~b, d, ~c),
                    new Clause(~b, ~d, e),
                    new Clause(~a, ~e, c),
                    new Clause(d, b, c),
                    new Clause(~a, e, ~d),
                    new Clause(~d, e, a),
                    new Clause(~b, ~e, c),
                    new Clause(a, b, ~c, d, ~e),
                    new Clause(a, b, ~c, d, e),
                    new Clause(a, b, ~c, ~d, ~e),
                    new Clause(a, b, c, ~d, ~e),
                    new Clause(a, ~b, ~c, ~d, ~e),
                    new Clause(~a, ~b, ~c, ~d, ~e),
                    new Clause(~a, b, ~c, ~d, ~e),
                    new Clause(~a, b, ~c, d, e),
                    new Clause(~a, b, ~c, d, ~e))

    val S = new DPSolver()
    clauses.foreach(S.addClause(_))
    val r = S.solve()
    assert (!r)
  }
}

/** These are tests for the D-P Solver with proof generation
 *  They also test the proof verifier. */
class CheckableSolverTestSpec extends AnyFlatSpec {
  "proof-gen test 1" should "be UNSAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val clauses  = Set(
      new Clause(a, b), 
      new Clause(~a, ~b),
      new Clause(a),
      new Clause(b))
        
    val S = new DPSolverEx()
    clauses.foreach(S.addClause(_))
    val (r, proof) = S.solve()
    assert (!r)
    assert (ProofChecker.verify(clauses, proof.get))
  }

  "proof-gen test 2" should "be SAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val clauses  = Set(
      new Clause(a, b), 
      new Clause(~a, ~b),
      new Clause(a),
      new Clause(~b))
        
    val S = new DPSolverEx()
    clauses.foreach(S.addClause(_))
    val (r, m) = S.solve()
    assert (r)
    assert (m.isEmpty)
  }

  "proof-gen test 3" should "be SAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)

    val clauses  = Set(
      new Clause(a, b), 
      new Clause(~a, ~b),
      new Clause(a),
      new Clause(~b))
        
    val S = new DPSolverEx()
    clauses.foreach(S.addClause(_))
    val (r, m) = S.solve()
    assert (r)
    assert (m.isEmpty)
  }

  "proof-gen test 4" should "be SAT" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)
    val e = Literal.create(5)

    val clauses = Set(
      new Clause(d, c, ~b, ~a, e),
      new Clause(e, ~c, b, ~a, d),
      new Clause(~b, a, ~c, ~e, d),
      new Clause(d, ~a, ~c, e, ~b),
      new Clause(~c, ~b, d, e, ~a),
      new Clause(~b, ~d, ~c, ~a, ~e),
      new Clause(~b, ~a, d, c, ~e),
      new Clause(b, a, ~c, ~e, d))

    val S = new DPSolverEx()
    clauses.foreach(S.addClause(_))
    val (r, m) = S.solve()
    assert (r)
    assert (m.isEmpty)
  }
  
  "proof-gen test 5" should "have a valid proof" in {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val c = Literal.create(3)
    val d = Literal.create(4)
    val e = Literal.create(5)

    val clauses = Set(
                    new Clause(d, ~b, c),
                    new Clause(~b, d, ~c),
                    new Clause(~b, ~d, e),
                    new Clause(~a, ~e, c),
                    new Clause(d, b, c),
                    new Clause(~a, e, ~d),
                    new Clause(~d, e, a),
                    new Clause(~b, ~e, c),
                    new Clause(a, b, ~c, d, ~e),
                    new Clause(a, b, ~c, d, e),
                    new Clause(a, b, ~c, ~d, ~e),
                    new Clause(a, b, c, ~d, ~e),
                    new Clause(a, ~b, ~c, ~d, ~e),
                    new Clause(~a, ~b, ~c, ~d, ~e),
                    new Clause(~a, b, ~c, ~d, ~e),
                    new Clause(~a, b, ~c, d, e),
                    new Clause(~a, b, ~c, d, ~e))

    val S = new DPSolverEx()
    clauses.foreach(S.addClause(_))
    val (r, proof) = S.solve()
    assert (!r)
    assert (ProofChecker.verify(clauses, proof.get))
    println(proof.get.toString())
  }
}
