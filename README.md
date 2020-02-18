CS675A Midterm
==============

You can compile using sbt:

    $ sbt compile


You can run the tests using sbt test.

    $ sbt test

I suggest you start sbt once and keep the sbt shell running. This works well for me on Linux.

    $ sbt
    sbt:main> set fork := true
    ...
    sbt:main> compile
    ...
    sbt:main> test

You can run a specific testsuite by using the testOnly command:

    sbt:main> testOnly *CNFTestSpec

The files in this repository are:

Problem #1
----------
1. src/main/scala/CNF.scala
2. src/main/scala/DPSolverEx.scala
3. src/main/scala/DPSolver.scala
4. src/main/scala/ProofChecker.scala

Problem #2
----------
1. src/main/scala/Expr.scala
2. src/main/scala/Relations.scala
3. src/main/scala/Solver.scala

Problem #3
----------
1. src/main/scala/CryptArithmetic.scala
3. src/main/scala/WordList.scala

Tests are all in src/test/scala/MidtermTests.scala

The tests for each problem are in the class:

* Problem 1(a): DPSolverSpec.
* Problem 1(b) and 1(c): CheckableSolverTestSpec
* Problem 2: SubstituteSpec, IsFunctionSpec and 
