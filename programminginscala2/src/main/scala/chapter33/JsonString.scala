package chapter33

import scala.util.parsing.combinator._

class JSON extends JavaTokenParsers {
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"
  def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"
  def arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]"
  def member: Parser[Any] = stringLiteral ~ ":" ~ value
}

class JSON1 extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)
  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"
  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }
  def value: Parser[Any] = (
    obj
      | arr
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
      | "null" ^^ (x => null)
      | "true" ^^ (x => true)
      | "false" ^^ (x => false)
    )
}

object ParseJSON extends JSON1 {
  def main(args: Array[String]) = {
    val json =
      """
        |{
        |"address book": {
        |"name": "John Smith",
        |"address": {
        |"street": "10 Market Street",
        |"city" : "San Francisco, CA",
        |"zip" : 94111
        |},
        |"phone numbers": [
        |"408 338-4238",
        |"408 111-6892"
        |]
        |}
        |}
      """.stripMargin
    println(parseAll(value, json))
  }
}
