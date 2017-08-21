import scala.collection.mutable.{Map => MutMap, Set => MutSet, ListBuffer}
import AutoClose._


class StringCache {
  val from = MutMap[String, Int]()
  val to = ListBuffer[String]()

  def get(s : String) : Int = {
    from.get(s) match {
      case Some(i) => i
      case None => {
        val i = from.size
        from.put(s,i)
        to += s
        i
      }
    }
  }

  def get(i : Int) : String = to(i)
}

object MappingFromUnambiguousCategories {
  val cache = new StringCache()

  def encode(s : String, t : String) : Long = {
    (cache.get(s).toLong << 32) + cache.get(t)
  }

  def decode(l : Long) : (String, String) = {
    (cache.get((l >> 32).toInt), cache.get(l.toInt))
  }

  def main(args : Array[String]) {
    val data = MutMap[Long,MutSet[Long]]()

    for(line <- io.Source.fromFile("unambiguous-mappings.tsv").getLines) {
      val Array(wnCat, wikiCat, wikiId, wnId) = line.split("\t")

      val l = encode(wnCat, wikiCat)
      if(!data.contains(l)) {
        data.put(l, MutSet())
      }
      data(l).add(encode(wikiId, wnId))
    }

    val sortedPairs = data.keys.toSeq.sortBy(x => -data(x).size)

    for(l2 <- sortedPairs.take(20)) {
      val (wnCat, wikiCat) = decode(l2)
      println(s"$wnCat\t\t$wikiCat\t${data(l2).size}")
    }

  }
}
