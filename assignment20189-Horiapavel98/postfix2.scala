// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// let- and right-associativity
abstract class Assoc
case object RA extends Assoc
case object LA extends Assoc

// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

def is_op(op: String) : Boolean = {
	ops.contains(op)
}

def prec(op1: String, op2: String) : Boolean = op1 match {
	case "+" | "-" | "*" | "/" => precs(op1) <= precs(op2)
	case _ => precs(op1) < precs(op2)
}

// (8) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
	case Nil => out:::st
	case first::rest => first match {
		case "(" => {
			syard(rest,first::st,out)
		}
		case ")" => {
			val newOut = for(i <- (0 until st.indexOf("(")).toList) yield(st(i))
			syard(rest,st.drop(newOut.length + 1),out ::: newOut)
		}
		case "+" | "-" | "*" | "/" | "^" => {
			if(st == Nil) syard(rest,first::st,out)
			else {
				if(is_op(st.head)){
					if(prec(first,st.head)) {
						syard(toks,st.tail,out:+st.head)
					}else {
						syard(rest,first::st,out)
					}
				}else {
					syard(rest,first::st,out)
				}
			}
		}
		case _ => syard(rest,st,out:+first)
	}
}


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (9) Implement a compute function that produces a Long(!) for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Long] = Nil) : Long = {
	toks match {
	case Nil => st.head
	case first::rest => first match {
			case "+" => compute(rest,(st(1)+st(0))::st.drop(2))
			case "-" => compute(rest,(st(1)-st(0))::st.drop(2))
			case "*" => compute(rest,(st(1)*st(0))::st.drop(2))
			case "/" => compute(rest,(st(1)/st(0))::st.drop(2))
			case "^" => compute(rest,(scala.math.pow(st(1),st(0)).toLong::st.drop(2)))
			case _ => compute(rest,first.toLong::st)
		}
	}
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

