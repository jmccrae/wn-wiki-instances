import argonaut._
import Argonaut._

case class WordNet(byId : Map[String, WordNetEntry])

case class WordNetEntry(parents : Seq[String], lemmas : Seq[String], defn : String, isInstance : Boolean)

object WordNetEntry {
  implicit def WordNetCodeJson : CodecJson[WordNetEntry] = 
    casecodec4(WordNetEntry.apply, WordNetEntry.unapply)("parents", "lemmas", "defn", "is_instance")
}


