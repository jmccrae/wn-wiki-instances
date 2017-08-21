import MultiMap._
import argonaut._
import Argonaut._
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import java.io.PrintWriter
import AutoClose._

class UnambiguousPairing {
  private val data = MutMap[(String,String),(MutMap[String, String],MutMap[String, String])]()
  private val burned = MutSet[(String, String)]() 

  def add(wnCat : String, wikiCat : String, wnId : String, wikiId : String) {
    if(!burned.contains((wnCat, wikiCat))) {
      val (m,mi) = data.get((wnCat, wikiCat)) match {
        case Some(m) => m
        case None =>
          val m2 = MutMap[String,String]()
          val m2i = MutMap[String,String]()
          data.put((wnCat, wikiCat), (m2,m2i))
          if(data.size % 100000 == 0) {
            System.err.println(data.size)
          }
          (m2,m2i)
      }
      if((m.contains(wikiId) && m(wikiId) != wnId) ||
         (mi.contains(wnId) && mi(wnId) != wikiId)) {
        data.remove((wnCat, wikiCat))
        burned.add((wnCat, wikiCat))
      } else {
        m.put(wikiId, wnId)
        mi.put(wnId, wikiId)
      }
    }
  }

  def printResult(out : PrintWriter) {
    for {
      ((wnCat, wikiCat), (m,_)) <- data
      (wikiId, wnId) <- m
      if m.size > 1 && m.values.toSet.size > 1
    } out.println(s"${m.size}\t$wnCat\t$wikiCat\t$wikiId\t$wnId")
  }
}

object FindUnambiguousMappings {
  def findPairs(wn : WordNet, wiki : Wiki) : Iterable[(String, String)] = {
    val wnByLemma : Map[String, Seq[String]] = wn.byId.toSeq.flatMap({
      case (id, wne) => 
        wne.lemmas.map(l => l.toLowerCase -> id)
    }).toMultiMap

    val wikiByLemma : Map[String, Seq[String]] = wiki.articles.toSeq.flatMap({
      case (title, article) =>
        article.alternatives.map(a => a -> title)
    }).toMultiMap

    for {
      k <- wnByLemma.keys.toStream
      x <- wnByLemma(k)
      y <- wikiByLemma.getOrElse(k, Nil)
    } yield {
      (x,y)
    }
  }

  def wnCats(wordnet : WordNet, wnId : String) : Seq[String] = {
    val wne = wordnet.byId(wnId)
    wne.parents ++ wne.parents.flatMap(p => wnCats(wordnet, p))
  }

  def wikiCats(wiki : Wiki, articleId : String) : Seq[String] = {
    val article = wiki.articles(articleId)
    article.categories ++ article.categories.flatMap(c => _wikiCats(wiki, c, 1))
  }

  def _wikiCats(wiki : Wiki, catId : String, depth : Int) : Seq[String] = {
    if(depth >= 3) {
      Nil
    } else {
      wiki.categories.get(catId) match {
        case Some(category) =>
          category.categories ++ category.categories.flatMap(c => _wikiCats(wiki, c, depth + 1))
        case None =>
          Nil
      }
    }
  }


  def main(args : Array[String]) {
    io.Source.fromFile("wordnet.json").mkString("").
        decodeOption[Map[String,WordNetEntry]].foreach({ wn =>
      io.Source.fromFile("wiki.json").mkString("").
          decodeOption[Wiki].foreach({ wiki =>
        var i = 0
        val wordnet = WordNet(wn)
        val pairing = new UnambiguousPairing()

        for((wnId, wikiId) <- findPairs(wordnet, wiki)) {
          i += 1
          for(wnCat <- wnCats(wordnet, wnId)) {
            for(wikiCat <- wikiCats(wiki, wikiId)) {
              pairing.add(wnCat, wikiCat, wnId, wikiId)
            }
          }
          if(i % 10000 == 0) {
            System.err.println(">" + i)
          }
        }
        println(i)
        withResource(new PrintWriter("unambiguous-mappings.tsv")) { out =>
          pairing.printResult(out)
        }
      })
    })
  }
}
