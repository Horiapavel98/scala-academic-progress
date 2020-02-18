// A Version with an explicit n-times regular expression;
// this keeps the size of the regular expression in the
// EVIL1 test-case quite small

abstract class Rexp 
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp   //explicit constructor for n-times


def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
}


def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r1, i) => 
    if (i == 0) ZERO else SEQ(der(c, r1), NTIMES(r1, i - 1))
}

def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))


// the optional regular expression: one or zero times
// this regular expression is still defined in terms of ALT
def OPT(r: Rexp) = ALT(r, ONE)


// Test Cases

// evil regular expressions
def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}



// test: (a?{n}) (a{n})
for (i <- 0 to 1000 by 100) {
  println(f"$i: ${time_needed(2, matcher(EVIL1(i), "a" * i))}%.5f")
}

// test: (a*)* b
for (i <- 0 to 20) {
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
  case NTIMES(r, _) => 1 + size(r)
}

// EVIL1(n) has now a constant size, no matter
// what n is; also the derivative only grows 
// moderately 

size(EVIL1(1))  // 7
size(EVIL1(3))  // 7
size(EVIL1(5))  // 7
size(EVIL1(7))  // 7
size(EVIL1(20)) // 7

size(ders("".toList, EVIL1(5)))       // 7
size(ders("a".toList, EVIL1(5)))      // 16
size(ders("aa".toList, EVIL1(5)))     // 35
size(ders("aaa".toList, EVIL1(5)))    // 59
size(ders("aaaa".toList, EVIL1(5)))   // 88
size(ders("aaaaa".toList, EVIL1(5)))  // 122
size(ders("aaaaaa".toList, EVIL1(5))) // 151

// but the size of the derivatives can still grow 
// quite dramatically in case of EVIL2

size(ders("".toList, EVIL2))       // 5
size(ders("a".toList, EVIL2))      // 12
size(ders("aa".toList, EVIL2))     // 28
size(ders("aaa".toList, EVIL2))    // 58
size(ders("aaaa".toList, EVIL2))   // 116
size(ders("aaaaa".toList, EVIL2))  // 230
size(ders("aaaaaa".toList, EVIL2)) // 456

size(ders(("a" * 20).toList, EVIL2)) // 7340068