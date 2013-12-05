package com.sysiq.common.parsers

import scala.util.parsing.combinator._

sealed abstract class Tag
case class Empty extends Tag
case class Comment(text: String) extends Tag
case class Unknown(text: String) extends Tag
case class JSPTag(tagName: String, attrs: Map[String, String]) extends Tag
case class HTMLTag(tagName: String, attrs: Map[String, String]) extends Tag
case class Directive(tagName: String, attrs: Map[String, String]) extends Tag
case class HTMLTagEnd(tagName: String) extends Tag
case class DocType(text: String) extends Tag

object JSPParser extends RegexParsers {
  def unkn = ".+".r
  def spaces = "\\s*".r
  def name = "[\\p{Alpha}-:]+".r
  def value = "\"" ~> "[^\"]*".r <~ "\""
  def attr = name ~ "=" ~ value ^^ { case k ~ _ ~ v => k -> v }

  //def tag = 

  def jspTag: Parser[JSPTag] =
    "<%" ~> name ~ rep(attr) <~ "%>" ^^ {
      case tagName ~ attrs => JSPTag(tagName, attrs.toMap)
    }

  def htmlTag0: Parser[HTMLTag] =
    "<" ~> name ~ opt(rep(attr)) <~ (">" | "/>") ^^ {
      case tagName ~ attrs => HTMLTag(tagName, attrs.getOrElse(List()).toMap)
    }

  def htmlTag2: Parser[HTMLTag] =
    "<" ~> name ~ rep(attr) ~ ">" <~ "[^<]*".r ~> "</" ~ name <~ ">" ^^ {
      case tagName ~ attrs ~ _ => HTMLTag(tagName, attrs.toMap)
    }

  def htmlEndTag: Parser[HTMLTagEnd] =
    "</" ~> name <~ ">" ^^ {
      case tagName => HTMLTagEnd(tagName)
    }

  def tags: Parser[List[Tag]] = rep(jspTag | directive | comment | docType | htmlTag2 | htmlTag0 | /*htmlTag1 |*/ htmlEndTag | unknown)

  def unknown: Parser[Unknown] = unkn ^^ {
    case text => {      
      Unknown(text)
    }
  }
  def comment: Parser[Comment] =
    "<%--" ~> "(?s)[^-]*-?[^-]*-?[^-]*".r <~ "--%>" ^^ {
      case text => Comment(text)
    }

  def directive: Parser[Directive] =
    "<%@" ~> name ~ rep(attr) <~ "%>" ^^ {
      case tagName ~ attrs => Directive(tagName, attrs.toMap)
    }

  def docType: Parser[DocType] =
    "<!" ~> "[^>]*".r <~ ">" ^^ {
      case text => DocType(text)
    }

}