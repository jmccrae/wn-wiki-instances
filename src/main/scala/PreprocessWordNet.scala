import scala.xml._
import MultiMap._
import argonaut._
import Argonaut._
import java.io.PrintWriter
import AutoClose._

object PreprocessWordNet {
  def main (args : Array[String]) {
    val xml = XML.loadFile("/home/jmccrae/projects/gwn-scala-api/wordnets/wn31.xml")

    val entries = (xml \\ "LexicalEntry").flatMap({ e =>
      if((e \ "Lemma" \ "@partOfSpeech").text == "n") {
        val lemma = (e \ "Lemma" \ "@writtenForm").text
        val synsets = (e \ "Sense").map(sense => (sense \ "@synset").text)
        synsets.map(synset => synset -> lemma)
      } else {
        Nil
      }
    }).toMultiMap

    val synsets = (xml \\ "Synset").flatMap({ e =>
      if((e \ "@partOfSpeech").text == "n") {
        val id = (e \ "@id").text
        val parents = (e \ "SynsetRelation").filter(sr => {
          (sr \ "@relType").text == "hypernym" ||
          (sr \ "@relType").text == "instance_hypernym"
        }).map(sr => {
          (sr \ "@target").text
        })
        val defn = (e \ "Definition").text
        val isInstance = (e \ "SynsetRelation").exists(sr => {
          (sr \ "@relType").text == "instance_hypernym"
        })
        
        Some(id -> WordNetEntry(parents, entries.getOrElse(id, Nil), defn, isInstance))
      } else {
        None
      }
    })

    val wordnet = WordNet(synsets.toMap)

    withResource(new PrintWriter("wordnet.json")) { out =>
      out.println(wordnet.byId.asJson.spaces2)
    }
  }

}
