import argonaut._
import Argonaut._
import MultiMap._
import AutoClose._
import java.io.PrintWriter

object PreprocessWiki {
  def wikiName(s : String) = {
    // Paris (plant) => paris
    val s1 = if(s.indexOf('(') > 0) {
        s.substring(0,s.indexOf('(')-1).toLowerCase
    } else {
      s.toLowerCase
    }
    // Paris, Texas => paris
    if(s1.contains(',')) {
      Seq(s1, s1.substring(0, s.indexOf(',')))
    } else {
      Seq(s1)
    }
  }

  def split[A,B](i : Iterator[Either[A,B]]) : (Seq[A], Seq[B]) = {
    val as = collection.mutable.ListBuffer[A]()
    val bs = collection.mutable.ListBuffer[B]()
    i.foreach({
      case Left(a) => as += a
      case Right(b) => bs += b
    })
    (as.toSeq, bs.toSeq)
  }


  def main(args : Array[String]) {
    val wordnet = io.Source.fromFile("wordnet.json").mkString("")
      .decodeOption[Map[String, WordNetEntry]]
      .getOrElse(throw new RuntimeException("Could not parse wordnet"))

    val words = wordnet.values.filter(_.isInstance).flatMap(_.lemmas).map(_.toLowerCase).toSet

    println(words.size)

    val redirLine = "(.*)=>(.*)".r

    val _catsRedirects = io.Source.fromInputStream(
      new java.util.zip.GZIPInputStream(
        new java.io.FileInputStream("wiki-cats-redirs.csv.gz"))).getLines.flatMap({ line =>
          line match {
            case redirLine(from, to) =>
              if(wikiName(from).exists(w => words contains w) ||
                 wikiName(to).exists(w => words contains w)) {
                Some(to -> from)
              } else {
                None
              }
            case _ =>
              None
          }
        })


    val disambigLine = "<http.*resource/(.*)> <http.*wikiPageDisambiguates> <http:.*resource/(.*)>.*".r

    val disambigs = io.Source.fromInputStream(
      new java.util.zip.GZIPInputStream(
        new java.io.FileInputStream("disambiguations_en.ttl.gz"))).getLines
          .flatMap({ line =>
            line match {
              case disambigLine(_from, _to) =>
                val from = _from.replaceAll("_", " ")
                val to = _to.replaceAll("_", " ")
                println(s"$from -> $to")
                if(wikiName(from).exists(w => words contains w) ||
                   wikiName(to).exists(w => words contains w)) {
                  Some(to -> from)
                } else {
                  None
                }
              case _ =>
                None
          }
        })
          

    val catsRedirects = (_catsRedirects ++ disambigs).toMultiMap

    println(catsRedirects.size)

    val cats = io.Source.fromInputStream(
      new java.util.zip.GZIPInputStream(
        new java.io.FileInputStream("wiki-cats-redirs.csv.gz"))).getLines.flatMap({ line =>
          if(!(line contains "=>")) {
            val elems = line.split("\t")
            if(elems(0).startsWith("Category:")) {
              val name = elems(0).drop("Category:".size)
              Some(Left(name -> WikiCategory(elems.drop(1).toSeq)))
            } else {
              val names = wikiName(elems(0)) ++ 
                catsRedirects.getOrElse(elems(0), Nil).flatMap(wikiName)
              if(names.exists(w => words contains w)) {
                Some(Right(elems(0) -> WikiArticle(names, elems.drop(1).toSeq)))
              } else {
                None
              }
            }
          } else {
            None
          }
        })


    val (allCategories, _articles) = split(cats)

    val articles = _articles.toMap
    println(articles.contains("John Walker (runner)"))

    val activeCategories = articles.values.flatMap(_.categories).toSet

    val categories = allCategories.filter({
      case (name, obj) if activeCategories contains name => true
      case _ => false
    }).toMap

    withResource(new java.io.PrintWriter("wiki.json")) { out =>
      out.println(Wiki(articles, categories).asJson.spaces2)
    }
  }
}
