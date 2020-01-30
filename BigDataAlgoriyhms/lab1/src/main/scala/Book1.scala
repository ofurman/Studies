import scala.io.Source
import java.io._
import scala.math

object Chapters {

  def clearText(text: String): Array[String] = {
    val stopwords = Source.fromFile("stopwords_en.txt", "UTF-8").mkString.split("\\s+")
    text.replaceAll("""[\p{Punct}]""", "").split("\\s+").drop(1).filterNot(stopwords.contains(_))
  }

  def tfIdf(documents: Array[Array[String]]) = {
    documents.map(text => {
      text.groupBy(identity).toList.
        map(x => (x._1,(x._2.length/text.length.toFloat)*math.log(documents.length.toFloat/documents.count(text => text.contains(x._1))))).sortWith((x,y) => x._2 > y._2)
    }).toList
  }

  def matchingScore(chapters: List[List[(String, Double)]], wordQuery: String) = {
    val v = chapters.map(_.filter(_._1==wordQuery).foldLeft(0.0)(_+_._2))
    val ms = (v.indices zip v).toMap.filter(x=> x._2!=0).toList.sortWith((x,y)=> x._2>y._2)
    println(s"\nMatching score for word $wordQuery")
    ms.foreach(x=>println(s"chapter ${x._1} score ${x._2}"))
  }

  def main(args: Array[String]): Unit = {
    val chapters = Source.fromFile("hpmor.txt", "UTF-8").mkString.toLowerCase.split("chapter \\d\\d?:").drop(1)
    val filteredChapters = chapters.map(clearText)
    val tfIdfChapters = tfIdf(filteredChapters)
    tfIdfChapters.zipWithIndex.foreach{ chi =>
      val chapter = chi._1
      val i = chi._2
      println(chapter.take(20))
//      val writer = new PrintWriter(s"sorted_words_chapter$i") {
//        chapter.take(50).foreach(x => write(s"${x._1}\t ${x._2}\n"))
//        close()
//      }
    }
    val ms = matchingScore(tfIdfChapters, "severus")
    println(ms)
  }
}