package midterm

import java.security.{MessageDigest => MD}
import java.nio.ByteBuffer

abstract class Expr {
  def computeDigest() : Array[Byte]
  val digest : Array[Byte] = computeDigest()
  override val hashCode : Int = ByteBuffer.wrap(digest.slice(0, 4)).getInt
  
  def hash(baseName : String, args: Seq[Expr]) : Array[Byte] = {
    val md = MD.getInstance("SHA-256")
    md.reset()
    md.update(baseName.getBytes("UTF-8"))
    args.foreach { arg => md.update(arg.digest) }
    md.digest()
  }

  // This function doesn't seem to be required, but I've put it in anyway.
  override def equals(that : Any) : Boolean = {
    that match {
      case e : Expr => hashCode == that.hashCode && MD.isEqual(digest, e.digest)
      case _        => false
    }
  }
}

case class BooleanLit(value: Boolean) extends Expr {
    override def toString = value.toString()
    override def computeDigest() : Array[Byte] = hash("$" + value.toString(), List.empty)
}

case class Variable(name: String) extends Expr {
    override def toString(): String = name
    override def computeDigest() : Array[Byte] = hash("VAR:" + name, List.empty)
} 
case class And(args: Seq[Expr]) extends Expr {
    override def toString(): String = {
        args.map(x => x.toString()).mkString("(and ", " ", ")")
    }
    override def computeDigest() : Array[Byte] = hash("#AND", args)
}
case class Or(args: Seq[Expr]) extends Expr {
    override def toString(): String = {
        args.map(x => x.toString()).mkString("(or ", " ", ")")
    }
    override def computeDigest() : Array[Byte] = hash("#OR", args)
}
case class Not(arg: Expr) extends Expr {
    override def toString(): String = {
        "(not " + arg.toString() + ")"
    }
    override def computeDigest() : Array[Byte] = hash("#NOT", List(arg))
}

