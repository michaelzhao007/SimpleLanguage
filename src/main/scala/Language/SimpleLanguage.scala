package Language

import scala.collection.mutable
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, PackratParsers, RegexParsers}
object SimpleLanguage extends JavaTokenParsers with PackratParsers with ImplicitConversions {

  case class ParseException(msg: String) extends Exception with Expr

  private val eoi = """\z""".r // end of input
  private val eol = sys.props("line.separator")
  private val separator = eoi | eol

  sealed trait Expr
  case class BinOpExpr(op: String, left: Expr, right: Expr) extends Expr
  case class Num(v: Int) extends Expr
  case class Variable(name: String, f: Num) extends Expr
  case class Function(args: Int, vars: List[Num]) extends Expr
  case class Loop(repeat: Expr, exp: Expr) extends Expr
  case class Computation(id: String, v: List[Num]) extends Expr
  case class Assign(name: String, v: Expr) extends Expr

  val vmap = new mutable.HashMap[String, Expr]()
  val fmap = new mutable.HashMap[String, Function]()

  def numericLit = """\d+(\.\d*)?""".r map { elem => Num(elem.toInt) }

  def lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  def ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }
  def identifier = lcid | ucid
  def function = (("function" ~ "of") ~> numericLit) ~ (":" ~> repsep(numericLit, ",")) ^^ { case arg ~ l => Function(arg.v, l) }

  def assignFuncWithID = ((identifier <~ "is") ~ function) ^^ {
    case varname ~ f => {
      fmap.put(varname, f)
      f
    }
  }

  def assignNumWithID = ((identifier <~ "is") ~ numericLit) ^^ {
    case varname ~ f => {
      vmap.put(varname, f)
      Variable(varname, f)
    }
  }

  def assignVarWithID = ((identifier <~ "is") ~ identifier) ^^ {
    case varname ~ valname => {
      val f = vmap.get(valname)
      f match {
        case None => new ParseException("variable is never assigned")
        case Some(v) => {
          vmap.put(varname, v)
          v
        }
      }
    }
  }

  def assign = assignFuncWithID | assignNumWithID | assignVarWithID

  def compute = ("what" ~ "is") ~> identifier ~ opt(rep("[" ~> numericLit <~ "]")) ^^ {
    case valname ~ None => Computation(valname, Nil)
    case valname ~ Some(f) => Computation(valname, f)
  }

  def eval(t: Object): Any = t match {
    case Num(v) => Num(v)
    case Computation(name, Nil) => {
        val f = vmap.get(name)
        f match {
          case None => new ParseException("variable is not associated with any existing function")
          case Some(v) => v
        }
    }
    case Computation(name, arl) => {
      val f = fmap.get(name)
      f match {
        case None => new ParseException("variable is not associated with any existing function")
        case Some(v) => {
          val compRes = (for {
            i <- 0 until arl.size
          } yield v.vars(i).v * arl(i).v).foldLeft(0){_ + _}
          val rem  = v.vars.drop(arl.size)
          if(rem.size <= 1) {
            val variable = Variable(name, Num(compRes + rem.head.v))
            vmap.put(name, variable)
            variable
          }
          else {
            val newfs = rem ++ List(Num(compRes))
            val newf = Function(rem.size, newfs)
            fmap.put(name, newf)
            newf
          }
        }
      }
    }
    case v => v
  }

  def lang = compute | assign

  def parseLang = phrase(lang)

  def parse(text: String) = {
    val textArr = text.split(";")
    for (t <- textArr) {
      val parsedResult = parseAll(parseLang, t)
      parsedResult match {
        case Success(r, _) => println(eval(r))
        case Failure(cause, _) =>
          throw new ParseException(s"Failed parsing message, Cause:= $cause")
        case Error(cause, _) =>
          throw new ParseException(s"An Error occurred while parsing message, Cause:= $cause")
      }
    }
  }
}

object SimpleLanguageTest extends App {
  //
  //
  SimpleLanguage.parse("ab is 10;what is ab;cd is function of 2:2,3,5;what is cd[7][2];")
}
