package Language

/**
  * Algorithm to parse a arthim
  * @param input
  */

class Test1(input: String) {
  //(1*(2+3)-5)
  // E -> E + T | T | E - T
  // T -> T * F | F | T / F
  // F -> N | (E)

  var i = 0

  trait Expr
  trait Op
  case object Plus extends Op
  case object Minus extends Op
  case object Mul extends Op
  case object Div extends Op

  case class Binary(op: Op,l: Expr, r: Expr) extends Expr
  case class Num(n: Int) extends Expr

  def peek(): String = input(i)+""
  def peek(s: String): Boolean = {
    peek().equals(s)
  }

  def consume() = {
    if(i < input.length - 1) {
      i = i + 1;
    }
  }

  def consume(s: String): Unit = {
    if(peek(s)) { consume() }
    else throw new Exception("exception")
  }

  def parseF(): Expr = {
    val p  = peek()
    if(p.matches("[0-9]*")) {
      consume()
      return Num(p.toInt)
    }
    else {
      consume("(")
      val res = parseE()
      consume(")")
      return res
    }
  }

  def parseE(): Expr = {
    var e = parseT();
    if(peek("+")) {
       consume();
       e = new Binary(Plus, e, parseE())
    }
    else if(peek("-")) {
      consume();
      e = new Binary(Minus, e, parseT())
    }
    return e
  }

  def parseT(): Expr = {
     var e = parseF()
     if(peek("*")) {
       consume();
       e = new Binary(Mul, e, parseT())
     }
     else if(peek("/")) {
       consume();
       e = new Binary(Div, e, parseT())
     }
     return e
  }
}

object Test2 extends App {
  val t = new Test1("(1+2)*3+5")
  println(t.parseE())
  val t1 = new Test1("(3*2+3/2)*5")
  println(t1.parseE())
}
