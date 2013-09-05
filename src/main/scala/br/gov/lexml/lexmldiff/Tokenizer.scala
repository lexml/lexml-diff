package br.gov.lexml.lexmldiff

import scala.util.parsing.combinator._

object Tokenizer extends RegexParsers {
  override val skipWhitespace = false
  def number: Parser[String] = """\d{1,3}(?:\.\d{3})*(?:,\d+)?"""r
  def punctuation: Parser[String] = """\p{P}"""r
  def spaces:Parser[String] = ("""\p{Z}+"""r) ^^^ " "
  def control : Parser[String] = """\p{Cntrl}"""r
  def words : Parser[String] = """[\p{L}\p{Nd}]+""".r
  def any : Parser[String] = "."r
  def tokens : Parser[List[String]] = rep (number | words | punctuation | spaces | control | any)
 
  def apply(input : String) : List[String] = parseAll(tokens,input) match {
    case Success(result,_) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
  
  def test[T](p : Parser[T], input : String) : T = parseAll(p,input) match {
    case Success(result,_) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
    
}