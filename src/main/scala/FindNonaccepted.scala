import argonaut._
import Argonaut._

object FindNonaccepted {
  def main(args : Array[String]) {
    
    val wiki = io.Source.fromFile("wordnet.json").mkString("").
      decodeOption[Map[String, WordNetEntry]].get

    val accepted = io.Source.fromFile("accepted.tsv").getLines.map({ line =>
      val elems = line.split("\t")
      elems(1)
    }).toSet

    wiki.filter({
      case (id, wne) => wne.isInstance && !accepted.contains(id)
    }).foreach({
      case (id, wne) =>
        println(s"""$id\t${wne.lemmas.mkString(",")}\t${wne.defn}""")
    })
    
  }
}
