// Detokenising the ouput of Tokeniser
//=====================================
//
// call with 
//
//     scala detokenise.scala fib.tks
//
//     scala detokenise.scala loops.tks

object Detokenise {

import java.io._
import scala.util._ 

abstract class Token extends Serializable 
case object T_SEMI extends Token
case object T_LPAREN extends Token
case object T_RPAREN extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token

def deserialise[T](fname: String) : Try[T] = {
  import scala.util.Using
  Using(new ObjectInputStream(new FileInputStream(fname))) {
    in => in.readObject.asInstanceOf[T]
  }
}

def main(args: Array[String]) = {
  val fname = args(0)
  val tks = deserialise[List[Token]](fname).getOrElse(Nil)
  println(s"Reading back from ${fname}:\n${tks.mkString("\n")}")  
}


}