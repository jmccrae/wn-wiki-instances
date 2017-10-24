import argonaut._
import Argonaut._

object FindNonaccepted {
  val commaSep = "(.*), (.*)".r
  val disambig = "(.*) \\((.*)\\)".r

  def main(args : Array[String]) {
    
    val wn = io.Source.fromFile("wordnet.json").mkString("").
      decodeOption[Map[String, WordNetEntry]].get

    val accepted = io.Source.fromFile("accepted.tsv").getLines.map({ line =>
      val elems = line.split("\t")
      (elems(0), elems(1))
    }).toSet

    val acceptedWn = accepted.map(_._2)

    val out = new java.io.PrintWriter("manual-check.tsv")

    out.println("WordNet ID\tLemmas\tDefinition\tWikiID\tType")
    accepted.filter({
      case (wikiId, wnId) => {
        val lemmas = wn(wnId).lemmas.map(_.toLowerCase)
        val defn = wn(wnId).defn.toLowerCase
        wikiId.toLowerCase match {
          case x if lemmas.contains(x) => false
          case commaSep(x,y) if lemmas.contains(x) && defn.contains(y) => false
          case disambig(x,y) if lemmas.contains(x) && defn.contains(y) => false
          case _ => true
        }
      }
    }).foreach({
      case (wikiId, wnId) => {
        val wne = wn(wnId)
        out.println(s"$wnId\t${wne.lemmas.mkString(",")}\t${wne.defn.replaceAll("\t", " ")}\t$wikiId")
      }
    })


    wn.filter({
      case (id, wne) => wne.isInstance && !acceptedWn.contains(id)
    }).foreach({
      case (id, wne) =>
        out.println(s"""$id\t${wne.lemmas.mkString(",")}\t${wne.defn}""")
    })
    
    out.close
  }
}
