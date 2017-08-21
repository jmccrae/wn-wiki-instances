import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import argonaut._
import Argonaut._
import scala.util.{Try, Success, Failure}

object PersonEvaluation {
  def year2int(s : String) = {
    if(s.endsWith(" BC")) 
      s.dropRight(3).toInt
    else
      s.toInt
  }

  def main(args : Array[String]) {
    val bornDied = ".* \\((\\d+( BC)?)-(\\d+( BC)?)\\)".r
    val died = ".* \\(died in (\\d+( BC)?)\\)".r
    val born = ".* \\(born in (\\d+( BC)?)\\)".r

    val wn = io.Source.fromFile("wordnet.json").mkString("")
      .decodeOption[Map[String,WordNetEntry]]
      .getOrElse(throw new RuntimeException("Could not parse wordnet.json"))
    val persons = wn.flatMap({
      case (wnId, wne) => {
        wne.defn match {
          case bornDied(b, _, d, _) => Some(wnId -> (Some(year2int(b)), Some(year2int(d))))
          case died(d, _) => Some(wnId -> (None, Some(year2int(d))))
          case born(b, _) => Some(wnId -> (Some(year2int(b)), None))
          case _ => None
        }
      }
    })
    System.err.println(s"${persons.size} people with DOB")

    val accepted = io.Source.fromFile("accepted.tsv").getLines.map({line =>
      val Array(i,j) = line.split("\t")
      i.replaceAll(" ", "_") -> j
    }).toMap

    val ERR = 5
    var correctBirth = 0
    var incorrectBirth = 0
    var correctDeath = 0
    var incorrectDeath = 0

    val deathDate = "<http.*resource/(.*)> <http.*deathDate> \"(-?\\d+).*".r
    val birthDate = "<http.*resource/(.*)> <http.*birthDate> \"(-?\\d+).*".r

    io.Source.fromInputStream(
      new GZIPInputStream(
        new FileInputStream("/home/jmccrae/data/infobox_dates.tql.gz"))
      ).getLines.foreach({ line =>
      line match {
        case birthDate(res, _year) =>
          Try(_year.toInt) match {
            case Success(year) =>{
              accepted.get(res).flatMap(w => persons.get(w)).flatMap(s => s._1)
                .foreach(tYear => {
                  if(year - ERR < tYear && year + ERR > tYear) {
                    correctBirth += 1
                  } else {
                    println(s"Incorrect Birth $res $year != $tYear")
                    incorrectBirth += 1
                  }
                })
            }
            case Failure(_) =>
              println("Bad year: " + _year)
          }
         case deathDate(res, _year) =>
          Try(_year.toInt).foreach(year => {
            accepted.get(res).flatMap(w => persons.get(w)).flatMap(s => s._2)
              .foreach(tYear => {
                if(year - ERR < tYear && year + ERR > tYear) {
                  correctDeath += 1
                } else {
                  println(s"Incorrect Death $res $year != $tYear")
                  incorrectDeath += 1
                }
              })
          })
        case _ =>
      }
    })
    println(s"Births: $correctBirth/ ${incorrectBirth+correctBirth}")
    println(s"Deaths: $correctDeath/ ${incorrectDeath+correctDeath}")
  }
}
