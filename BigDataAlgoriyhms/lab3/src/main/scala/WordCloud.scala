import org.apache.spark.{SparkConf, SparkContext}
import scala.io.Source

object WordCloud {
  def main(args: Array[String]) {
    val conf = new SparkConf().setMaster("local").setAppName("WordCloud")
    val sc = new SparkContext(conf)

    val words = sc.textFile("Capital.txt").flatMap(line => line.split(" ")).
      map(w => w.toLowerCase.replaceAll("""[\p{Punct}]""", ""))
    val stopwords = Source.fromFile("stopwords_en.txt", "UTF-8").mkString.split("\\s+")

    val filteredWords = words.filter(!stopwords.contains(_))
    val wordCount = filteredWords.countByValue()
//    wordCount.foreach( w => println(w._1, "\t",w._2))
    sc.parallelize(wordCount.toSeq).saveAsTextFile("wordCloud.txt")
  }
}