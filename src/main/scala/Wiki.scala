import argonaut._
import Argonaut._

case class Wiki(articles : Map[String, WikiArticle],
                categories : Map[String, WikiCategory])

case class WikiArticle(alternatives : Seq[String],
  categories : Seq[String])

case class WikiCategory(categories : Seq[String])

object Wiki {
  implicit def wikiCodecJson : CodecJson[Wiki] =
    casecodec2(Wiki.apply, Wiki.unapply)("articles", "categories")
}

object WikiArticle {
  implicit def codecJson : CodecJson[WikiArticle] =
    casecodec2(WikiArticle.apply, WikiArticle.unapply)("alternatives", "categories")
}

object WikiCategory {
  implicit def codecJson : CodecJson[WikiCategory] =
    casecodec1(WikiCategory.apply, WikiCategory.unapply)("categories")
}
