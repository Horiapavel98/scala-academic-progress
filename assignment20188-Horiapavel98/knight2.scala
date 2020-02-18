// Part 2 about finding a single tour for a board using the Warnsdorf Rule
//=========================================================================

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  (x._1 >= 0 && x._2 >= 0 && x._1 < dim && x._2 < dim && path.contains(x) == false)
}

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

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    (legal_moves(dim,path,x).map(move => (move, legal_moves(dim,path,move).length)).toList.sortBy(_._2)).map(pair => pair._1)
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  case Nil => None
  case x::xs => {
    val result = f(x)
    if(result != None) result
    else first(xs,f)
  }
}

def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
  if (dim < 5) None
  if(dim*dim == path.length) {
    if(ordered_moves(dim,List(),path.head).contains(path.last) == true) {
      Some(path)
    }
    else {
      None
    }
  }
  else first(ordered_moves(dim,path,path.head), x => first_closed_tour_heuristic(dim,x::path)) 
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
  if(dim < 5) None
  else {
    if(dim*dim == path.length) Some(path)
    else first(ordered_moves(dim,path,path.head), x => first_tour_heuristic(dim,x::path))
  }
}

