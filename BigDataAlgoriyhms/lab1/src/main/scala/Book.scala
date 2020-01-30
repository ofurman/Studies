import scala.io.Source
import java.io._
import scala.util.Random

object kek {

  def main(args: Array[String]): Unit = {
    val words = Source.fromFile("hpmor.txt", "UTF-8").mkString.toLowerCase.
      replaceAll("""[\p{Punct}]""", "").split("\\s+")
    val stopwords = Source.fromFile("stopwords_en.txt", "UTF-8").mkString.split("\\s+")
    val filteredWords = words.filterNot(stopwords.contains(_))
    val indexedFilteredWords = filteredWords.zipWithIndex
    val grouped = indexedFilteredWords.groupBy(x=>x._1)
    val reducedWords = grouped.view.mapValues(x=>x.size).toMap
    val sorted = reducedWords.toSeq.sortWith((x, y)=>x._2>y._2)
    val writer = new PrintWriter("sorted_words.csv") {
      sorted.take(50).foreach(x=>write(s"${x._1}\t ${x._2}\n"))
      close()
    }
  }
}