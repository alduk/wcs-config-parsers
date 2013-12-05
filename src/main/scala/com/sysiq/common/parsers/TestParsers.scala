package com.sysiq.common.parsers
import scala.io._
import JSPParser._
import scala.util.parsing.input.CharArrayReader
import java.io.File
import scala.Array.canBuildFrom
import scala.collection.SortedMap
import scala.collection.Map
object TestParsers {

  type Tags = List[Tag]

  val dir = """C:\IBM\WCDE_ENT70\workspace\Stores\WebContent\AuroraStorefrontAssetStore\"""
  val auroraPath = "/Stores/WebContent/AuroraStorefrontAssetStore/"

  case class Page(url: String, importedBy: List[Page] = List(), imports: List[Page] = List())
  object Dummy extends Page("---Dummy---Page----")

  def main(args: Array[String]): Unit = {
    val group = parseDir(dir)
    printGrouped(group, "++++////////////+++++++")
    println(group.size)

  }

  def printGrouped(grouped: Map[String, Page], separator: String) = {
    grouped foreach { e =>
      println(e._1)
      println("	Imported by:")
      e._2.importedBy foreach { p => println(s"	${p.url}") }
      println("	Imports:")
      e._2.imports foreach { p => println(s"	${p.url}") }
      println(separator)
    }

  }

  private def parse(parseFunc: => Parser[Tag], sample: String) = {
    val res = parseFunc(new CharArrayReader(sample.toCharArray(), 0))
    res match {
      case Success(a, b) => (a, b)
      case Failure(a, b) => (a, b)
    }
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  private def parseFile(jspPath: String): Tuple2[Tags, _] = {
    val source = Source.fromFile(jspPath)(Codec.ISO8859)
    println(s"Parsing: $jspPath")
    val text = source.mkString
    val v2 = parseAll(tags, text)
    v2 match {
      case Success(a, b) => (a, b)
      case Failure(a, b) => (List(), b)
    }
  }

  private def filterTags(l: List[Tuple2[String, Tags]]) = l.map { a =>
    val t1 = a._1.replaceAll("\\\\", "/")
    val t2 = t1.replaceAll("C:/IBM/WCDE_ENT70/workspace", "")
    (t2, (a._2 map {
      case c @ HTMLTag(tagName, _) if tagName == "c:import" => Some(c)
      case d @ Directive(name, _) if name == "include" => Some(d)
      case _ => None
    }).distinct)
  }

  def toPage(path: String, tag: Option[Tag]) = {
    tag match {
      case c @ Some(HTMLTag(tagName, attrs)) => pageFromUrl(path, attrs.get("url"))
      case d @ Some(Directive(name, attrs)) => pageFromUrl(path, attrs.get("file"))
      case _ => List()
    }
  }

  def pageFromUrl(path: String, url: Option[String]) = {
    url match {
      case Some(u) => List(Page(normalizeUrl(path, u)))
      case None => List()
    }
  }

  def normalizeUrl(path: String, file: String) = {
    val s0 = file.replaceAll("/\\$\\{sdb.jspStoreDir\\}/", auroraPath)
    val s1 = s0.replaceAll("\\$\\{jspStoreDir\\}", auroraPath)
    val s2 = s1.replaceAll("\\$\\{env_jspStoreDir\\}", auroraPath)
    s2 match {
      case s if (s.indexOf("..") > -1 || !s.startsWith("/")) => {
        val res = new File(new File(path).getParentFile(), s).getCanonicalPath()
        res.replaceAll("C:", "").replaceAll("\\\\", "/")
      }
      case s if s.indexOf("..") == -1 => s
    }
  }

  def parseDir(directory: String): Map[String, TestParsers.Page] = {
    val files = recursiveListFiles(new File(directory)).filter(f => !f.isDirectory() && (f.getAbsolutePath().endsWith("jsp") || f.getAbsolutePath().endsWith("jspf")))
    val res = (files.map { (f: File) => (f.getAbsolutePath(), parseFile(f.getAbsolutePath())._1) }).toList
    val htmlTags = filterTags(res)

    val pages = for (
      t <- htmlTags;
      tag <- t._2
    ) yield Page(t._1, List(), toPage(t._1, tag))

    val grouped = pages groupBy (_.url)
    val grouped1 = grouped map { e =>
      (e._1, Page(e._1, List(), e._2.tail flatMap { _.imports }))
    }
    println(grouped1.size)

    val grouped2 = collection.mutable.Map[String, Page]()
    for (
      (url, page) <- grouped1;
      (url1, page1) <- grouped1 filter { _._2.imports.exists(_.url == page.url) }
    ) {
      val p = grouped2 getOrElseUpdate (url, page)
      grouped2 += (url -> (p copy ( importedBy = page1 :: p.importedBy )))
    }
    println(grouped2.size)

    val a = (grouped1 ++ grouped2).toArray
    SortedMap(a : _*)

    /*    (grouped1 ++ grouped2) filter { t =>
      t._2.imports.isEmpty && t._2.importedBy.isEmpty
    }*/
  }

}