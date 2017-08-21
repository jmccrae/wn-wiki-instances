import MultiMap._
import argonaut._
import Argonaut._
import scala.collection.mutable.{Map => MutMap, Set => MutSet, ListBuffer}
import java.io.PrintWriter
import AutoClose._



class WeightedPair {
  val weights : MutMap[(String, String), Option[Int]] = MutMap()
  val wn2wiki : MutMap[String, String] = MutMap() 
  val wiki2wn : MutMap[String, String] = MutMap()
  
    def add(wt : Int, wnId : String, wikiId : String) {
      if((wn2wiki.contains(wnId) && wn2wiki(wnId) != wikiId)
        || (wiki2wn.contains(wikiId) && wiki2wn(wikiId) != wnId)) {
        weights.put((wnId, wikiId), None)
      } else {
        weights.get((wnId, wikiId)) match {
          case Some(Some(i)) =>
            weights.put((wnId, wikiId), Some(i + wt))
          case Some(None) =>
          case None =>
            weights.put((wnId, wikiId), Some(wt))
        }
        wn2wiki.put(wnId, wikiId)
        wiki2wn.put(wikiId, wnId)
      }
    }

    def keys = weights.keys

    def size = weights.size

    def score = weights.values.map({
      case Some(wt) => wt
      case None => /*AMBIG_PENALTY*/ -10
    }).sum
        
}

class WeightedPairing {
  private val data = MutMap[(String,String),WeightedPair]()
  private val burned = MutSet[(String, String)]()
  private val weak = MutMap[(String,String), ListBuffer[(String,String)]]()


  def add(wnCat : String, wikiCat : String, wnId : String, wikiId : String, wt : Int) {
//    if(wt == 1) {
//      weak.getOrElse((wnCat, wikiCat), ListBuffer()).append((wnId, wikiId))
//    } else {
      if(!burned.contains((wnCat, wikiCat))) {
        val wp = data.get((wnCat, wikiCat)) match {
          case Some(m) => m
          case None => {
            if((data.size + 1) % 10000 == 0) {
              System.err.println(data.size+1)
            }
            new WeightedPair()
          }
        }
        wp.add(wt, wnId, wikiId)
        if(wp.score > 0) {
          data.put((wnCat,wikiCat),wp)
        } else {
          data.remove((wnCat, wikiCat))
          burned.add((wnCat, wikiCat))
        }
      }
//    }
  }

  def printResult(out : PrintWriter, mapping : Map[String, String]) {
    for {
      ((wnCat, wikiCat), wp) <- data
      if wp.size > 1 
    } {
      for((wnId, wikiId) <- wp.keys
           if !mapping.contains(wnId)) {
           out.println(s"${wp.score}\t$wnCat\t$wikiCat\t$wikiId\t$wnId")
      }
      for((wnId, wikiId) <- weak.getOrElse((wnCat, wikiCat), Nil)
           if !mapping.contains(wnId)) {
           out.println(s"${wp.score}\t$wnCat\t$wikiCat\t$wikiId\t$wnId")
      }
    }

  }
}

object FindWeightedMappings {
  def findPairs(wn : WordNet, wiki : Wiki) : Iterable[(String, String,String)] = {
    val wnByLemma : Map[String, Seq[String]] = wn.byId.toSeq.flatMap({
      case (id, wne) => 
        if(wne.isInstance) {
          wne.lemmas.map(l => l.toLowerCase -> id)
        } else {
          Nil
        }
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
      (x,y,k)
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
    val mapping = io.Source.fromFile("accepted.tsv").getLines.map({line =>
      val e = line.split("\t")
      e(1) -> e(0)
    }).toMap
//    println(mapping.get("wn31-11085224-n"))
    io.Source.fromFile("wordnet.json").mkString("").
        decodeOption[Map[String,WordNetEntry]].foreach({ wn =>
      io.Source.fromFile("wiki.json").mkString("").
          decodeOption[Wiki].foreach({ wiki =>
        var i = 0
        val wordnet = WordNet(wn)
        val pairing = new WeightedPairing()

        for((wnId, wikiId, overlap) <- findPairs(wordnet, wiki)
             if !mapping.contains(wnId)) {
          i += 1
          for(wnCat <- wnCats(wordnet, wnId)) {
            for(wikiCat <- wikiCats(wiki, wikiId)) {
              val wt = overlap.split("\\s").size
              pairing.add(wnCat, wikiCat, wnId, wikiId, wt)
            }
          }
          if(i % 10000 == 0) {
            System.err.println(">" + i)
          }
        }
        println(i)
        withResource(new PrintWriter("weighted-mappings.tsv")) { out =>
          pairing.printResult(out, mapping)
        }
      })
    })
  }
}
