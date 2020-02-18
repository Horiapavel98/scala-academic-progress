// Part 1 about finding Knight's tours
//=====================================

// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end are of any help.



type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1) Complete the function that tests whether the position x
//    is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  (x._1 >= 0 && x._2 >= 0 && x._1 < dim && x._2 < dim && path.contains(x) == false)
}



//(2) Complete the function that calculates for a position x
//    all legal onward moves that are not already in the path. 
//    The moves should be ordered in a "clockwise" manner.
 

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  List((x._1 + 1, x._2 + 2),
  (x._1 + 2, x._2 + 1),
  (x._1 + 2, x._2 - 1),
  (x._1 + 1, x._2 - 2),
  (x._1 - 1, x._2 - 2),
  (x._1 - 2, x._2 - 1),
  (x._1 - 2, x._2 + 1),
  (x._1 - 1, x._2 + 2)).filter(move => is_legal(dim,path,move))
  //(a :: b :: c :: d :: e :: f :: g :: h :: List()).filter(move => is_legal(dim,path,move) == true)
}


//some test cases
//
//assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(3) Complete the two recursive functions below. 
//    They exhaustively search for knight's tours starting from the 
//    given path. The first function counts all possible tours, 
//    and the second collects all tours in a list of paths.

def count_tours(dim: Int, path: Path) : Int = {
  if (dim*dim == path.length) {
    1
  }
  else (for (move <- legal_moves(dim,path,path.head)) yield (count_tours(dim,move::path))).sum
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
  if (dim*dim == path.length) {
    List(path)
  }
  else (for (move <- legal_moves(dim,path,path.head)) yield (enum_tours(dim,move::path))).flatten
}


//(5) Implement a first-function that finds the first 
//    element, say x, in the list xs where f is not None. 
//    In that case Return f(x), otherwise None. If possible,
//    calculate f(x) only once.

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  case Nil => None
  case x::xs => {
    val result = f(x)
    if(result != None) result
    else first(xs,f)
  }
}


// test cases
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None




//(6) Implement a function that uses the first-function from (5) for
//    trying out onward moves, and searches recursively for a
//    knight tour on a dim * dim-board.


def first_tour(dim: Int, path: Path) : Option[Path] = {
  if (dim*dim == path.length) Some(path)
  else first(legal_moves(dim,path,path.head),x => first_tour(dim,x::path))
}
 





/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//     time_needed(count_tours(dim, List((0, 0))))
// in order to print out the time that is needed for 
// running count_tours

// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println
  } 
}


*/
