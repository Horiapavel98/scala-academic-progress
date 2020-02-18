// A parser and interpreter for the While language
// 

import scala.language.implicitConversions
import scala.language.reflectiveCalls

// more convenience for the semantic actions later on
case class ~[+A, +B](_1: A, _2: B)


type IsSeq[A] = A => Seq[_]

abstract class Parser[I : IsSeq, T] {
  def parse(ts: I): Set[(T, I)]

  def parse_all(ts: I) : Set[T] =
    for ((head, tail) <- parse(ts); if tail.isEmpty) yield head
}

class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(sb: I) = 
    for ((head1, tail1) <- p.parse(sb); 
         (head2, tail2) <- q.parse(tail1)) yield (new ~(head1, head2), tail2)
}

class AltParser[I : IsSeq, T](p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)   
}

class FunParser[I : IsSeq, T, S](p: => Parser[I, T], f: T => S) extends Parser[I, S] {
  def parse(sb: I) = 
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

case class StringParser(s: String) extends Parser[String, String] {
  def parse(sb: String) = {
    val (prefix, suffix) = sb.splitAt(s.length)
    if (prefix == s) Set((prefix, suffix)) else Set()
  }
}

case object NumParser extends Parser[String, Int] {
  val reg = "[0-9]+".r
  def parse(sb: String) = reg.findPrefixOf(sb) match {
    case None => Set()
    case Some(s) => {
      val (head, tail) = sb.splitAt(s.length)
      Set((head.toInt, tail)) 
    }
  }
}


implicit def string2parser(s : String) = StringParser(s)

implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

implicit def StringOps(s: String) = new {
  def || (q : => Parser[String, String]) = new AltParser[String, String](s, q)
  def || (r: String) = new AltParser[String, String](s, r)
  def ==>[S] (f: => String => S) = new FunParser[String, String, S](s, f)
  def ~[S](q : => Parser[String, S]) = 
    new SeqParser[String, String, S](s, q)
  def ~ (r: String) = {
    print("da");new SeqParser[String, String, String](s, r) }
}


// the abstract syntax trees for the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp 

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Write(s: String) extends Stmt


case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class And(b1: BExp, b2: BExp) extends BExp
case class Or(b1: BExp, b2: BExp) extends BExp

case object IdParser extends Parser[String, String] {
  val reg = "[a-z][a-z,0-9]*".r
  def parse(sb: String) = reg.findPrefixOf(sb) match {
    case None => Set()
    case Some(s) => Set(sb.splitAt(s.length))
  }
}

lazy val AExp: Parser[String, AExp] = 
  (Te ~ "+" ~ AExp) ==> { case x ~ _ ~ z => Aop("+", x, z): AExp } ||
  (Te ~ "-" ~ AExp) ==> { case x ~ _ ~ z => Aop("-", x, z): AExp } || Te 
lazy val Te: Parser[String, AExp] = 
  (Fa ~ "*" ~ Te) ==> { case x ~ _ ~ z => Aop("*", x, z): AExp } || 
  (Fa ~ "/" ~ Te) ==> { case x ~ _ ~ z => Aop("/", x, z): AExp } || Fa  
lazy val Fa: Parser[String, AExp] = 
   ("(" ~ AExp ~ ")") ==> { case _ ~ y ~ _ => y } || 
   IdParser ==> Var || 
   NumParser ==> Num

// boolean expressions with some simple nesting
lazy val BExp: Parser[String, BExp] = 
   (AExp ~ "==" ~ AExp) ==> { case x ~ _ ~ z => Bop("==", x, z): BExp } || 
   (AExp ~ "!=" ~ AExp) ==> { case x ~ _ ~ z => Bop("!=", x, z): BExp } || 
   (AExp ~ "<" ~ AExp) ==> { case x ~ _ ~ z => Bop("<", x, z): BExp } || 
   (AExp ~ ">" ~ AExp) ==> { case x ~ _ ~ z => Bop(">", x, z): BExp } ||
   ("(" ~ BExp ~ ")" ~ "&&" ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => And(y, v): BExp } ||
   ("(" ~ BExp ~ ")" ~ "||" ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => Or(y, v): BExp } ||
   ("true" ==> (_ => True: BExp )) || 
   ("false" ==> (_ => False: BExp )) ||
   ("(" ~ BExp ~ ")") ==> { case _ ~ x ~ _ => x }

// statement / statements
lazy val Stmt: Parser[String, Stmt] =
  (("skip" ==> (_ => Skip: Stmt)) ||
   (IdParser ~ ":=" ~ AExp) ==> { case x ~ _ ~ z => Assign(x, z): Stmt } ||
   ("write(" ~ IdParser ~ ")") ==> { case _ ~ y ~ _ => Write(y): Stmt } ||
   ("if" ~ BExp ~ "then" ~ Block ~ "else" ~ Block) ==>
    { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w): Stmt } ||
   ("while" ~ BExp ~ "do" ~ Block) ==> { case _ ~ y ~ _ ~ w => While(y, w) }) 
 
lazy val Stmts: Parser[String, Block] =
  (Stmt ~ ";" ~ Stmts) ==> { case x ~ _ ~ z => x :: z : Block } ||
  (Stmt ==> ( s => List(s) : Block))

// blocks (enclosed in curly braces)
lazy val Block: Parser[String, Block] =
  (("{" ~ Stmts ~ "}") ==> { case x ~ y ~ z => y} || 
   (Stmt ==> (s => List(s))))


Stmts.parse_all("x2:=5+3;")
Block.parse_all("{x:=5;y:=8}")
Block.parse_all("if(false)then{x:=5}else{x:=10}")

val fib = """n := 10;
             minus1 := 0;
             minus2 := 1;
             temp := 0;
             while (n > 0) do {
                 temp := minus2;
                 minus2 := minus1 + minus2;
                 minus1 := temp;
                 n := n - 1
             };
             result := minus2""".replaceAll("\\s+", "")

Stmts.parse_all(fib)


// an interpreter for the WHILE language
type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
  case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
  case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  case And(b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
  case Or(b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
}

def eval_stmt(s: Stmt, env: Env) : Env = s match {
  case Skip => env
  case Assign(x, a) => env + (x -> eval_aexp(a, env))
  case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env) 
  case While(b, bl) => 
    if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
    else env
  case Write(x) => { println(env(x)) ; env }  
}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}

def eval(bl: Block) : Env = eval_bl(bl, Map())

// parse + evaluate fib program; then lookup what is
// stored under the variable result 
println(eval(Stmts.parse_all(fib).head)("result"))


// more examles

// calculate and print all factors bigger 
// than 1 and smaller than n
println("Factors")

val factors =  
   """n := 12;
      f := 2;
      while (f < n / 2 + 1) do {
        if ((n / f) * f == n) then  { write(f) } else { skip };
        f := f + 1
      }""".replaceAll("\\s+", "")

eval(Stmts.parse_all(factors).head)

// calculate all prime numbers up to a number 
println("Primes")

val primes =  
   """end := 100;
      n := 2;
      while (n < end) do {
        f := 2;
        tmp := 0;
        while ((f < n / 2 + 1) && (tmp == 0)) do {
          if ((n / f) * f == n) then  { tmp := 1 } else { skip };
          f := f + 1
        };
        if (tmp == 0) then { write(n) } else { skip };
        n  := n + 1
      }""".replaceAll("\\s+", "")

eval(Stmts.parse_all(primes).head)
