// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================


// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (6) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 
def is_op(op: String) : Boolean = {
	ops.contains(op)
}
def prec(op1: String, op2: String) : Boolean = {
	precs(op1) <= precs(op2)
}

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
		case "+" | "-" | "*" | "/" => {
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
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (7) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

def compute(toks: Toks, st: List[Int] = Nil) : Int ={
	toks match {
	case Nil => st.head
	case first::rest => first match {
		case "+" => compute(rest,(st(1)+st(0))::st.drop(2))
		case "-" => compute(rest,(st(1)-st(0))::st.drop(2))
		case "*" => compute(rest,(st(1)*st(0))::st.drop(2))
		case "/" => compute(rest,(st(1)/st(0))::st.drop(2))
		case _ => compute(rest,first.toInt::st)
	}
	}
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15




