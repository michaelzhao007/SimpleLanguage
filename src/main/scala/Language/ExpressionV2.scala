package Language

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

object ExpressionV2 extends App with RegexParsers with JavaTokenParsers  {
  class Exp
  case class BinOp(op: String, elem1: Exp, elem2: Exp) extends Exp
  case class Num(v: Double) extends Exp

  def expr: Parser[Exp] = (term ~ opt(("+" | "-") ~ expr)) ^^ {
    case a ~ None => a
    case a ~ Some("+" ~ b) => BinOp("+", a, b)
    case a ~ Some("-" ~ b) => BinOp("-", a, b)
  }

  def term: Parser[Exp] =  factor ~ opt(("*" | "/") ~ term) ^^ {
    case a ~ None => a
    case a ~ Some("*" ~ b) => BinOp("*", a, b)
    case a ~ Some("/" ~ b) => BinOp("/", a, b)
  }
  def factor: Parser[Exp] =  opt("-" | "+") ~ decimalNumber ^^ {
    case None ~ n => Num(n.toDouble)
    case Some("+") ~ n => Num(n.toDouble)
    case Some("-") ~ n => Num(-n.toDouble) } | "(" ~> expr <~ ")"

  def number = """\d+(\.\d*)?""".r map { elem => Num(elem.toDouble) }

  def parse(input: String) = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def eval(exp: Exp): Double = {
    exp match {
      case Num(v) => v
      case BinOp("+", l, r) => eval(l) + eval(r)
      case BinOp("-", l, r) => eval(l) - eval(r)
      case BinOp("*", l, r) => eval(l) * eval(r)
      case BinOp("/", l, r) => eval(l) / eval(r)
    }
  }

  println(eval(parse("-25.5+13.3*2.5+(1.0-3.0)")))

}


