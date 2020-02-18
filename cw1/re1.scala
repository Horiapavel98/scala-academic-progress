// A simple matcher for basic regular expressions

abstract class Rexp
case object ZERO extends Rexp                    // matches nothing
case object ONE extends Rexp                     // matches an empty string
case class CHAR(c: Char) extends Rexp            // matches a character c
case class ALT(r1: Rexp, r2: Rexp) extends Rexp  // alternative
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp  // sequence
case class STAR(r: Rexp) extends Rexp            // star

// nullable function: tests whether a regular 
// expression can recognise the empty string
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
}

// the derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
}

// the derivative w.r.t. a string (iterates der)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

// the main matcher function
def matcher(r: Rexp, s: String) : Boolean = 
  nullable(ders(s.toList, r))


// examples from the homework

val r = STAR(ALT(SEQ(CHAR('a'), CHAR('b')), CHAR('b')))
der('a', r)
der('b', r)
der('c', r)

val r2 = SEQ(SEQ(CHAR('x'), CHAR('y')), CHAR('z'))
der('x', r2)
der('y', der('x', r2))
der('z', der('y', der('x', r2)))


// the optional regular expression (one or zero times)
def OPT(r: Rexp) = ALT(r, ONE)

// the n-times regular expression (explicitly expanded)
def NTIMES(r: Rexp, n: Int) : Rexp = n match {
  case 0 => ONE
  case 1 => r
  case n => SEQ(r, NTIMES(r, n - 1))
}


// Test Cases

// the evil regular expression  a?{n} a{n}
def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))

// the evil regular expression (a*)*b
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}


// test: (a?{n}) (a{n})
println("Test (a?{n}) (a{n})")

for (i <- 0 to 20 by 2) {
  println(f"$i: ${time_needed(2, matcher(EVIL1(i), "a" * i))}%.5f")
}

// test: (a*)* b
println("Test (a*)* b")

for (i <- 0 to 20 by 2) {
  println(f"$i: ${time_needed(2, matcher(EVIL2, "a" * i))}%.5f")
}


// the size of a regular expressions - for testing purposes 
def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
}

// the expicit expansion in EVIL1(n) increases
// drastically its size

size(EVIL1(1))  // 5
size(EVIL1(3))  // 17
size(EVIL1(5))  // 29
size(EVIL1(7))  // 41
size(EVIL1(20)) // 119

// given a regular expression and building successive
// derivatives might result into bigger and bigger
// regular expressions...here is an example for this:

// (a+b)* o a o b o (a+b)*
val BIG_aux = STAR(ALT(CHAR('a'), CHAR('b')))
val BIG = SEQ(BIG_aux, SEQ(CHAR('a'),SEQ(CHAR('b'), BIG_aux)))

size(ders("".toList, BIG))              // 13
size(ders("ab".toList, BIG))            // 51
size(ders("abab".toList, BIG))          // 112
size(ders("ababab".toList, BIG))        // 191
size(ders("abababab".toList, BIG))      // 288
size(ders("ababababab".toList, BIG))    // 403
size(ders("abababababab".toList, BIG))  // 536


size(ders(("ab" * 200).toList, BIG))    // 366808

for (i <- 0 to 200 by 10) {
  println(f"$i: ${time_needed(2, matcher(BIG, "ab" * i))}%.5f")
}




//////////////////////////////////////
def concat(A: Set[String], B: Set[String]) : Set[String] =
  for (s1 <- A; s2 <- B) yield s1 ++ s2
  

val A = Set("foo", "bar")
val B = Set("a", "b")


