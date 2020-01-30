import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.io.Source


object Paragraph {
  def main(args: Array[String]): Unit = {

    val conf = new SparkConf().setMaster("local").setAppName("Alg")
    val sc = new SparkContext(conf)

    val stopWords = Source.fromFile("stopwords_en.txt").mkString.split("\\s+")

    val removeStopWords = sc.textFile("Capital.txt").flatMap(word => word.split("\\s+")).
      map(_.toLowerCase.replaceAll("""[\p{Punct}]""", "")).
      filter(!stopWords.contains(_)).collect()

    val pairs = sc.parallelize(removeStopWords.sliding(2).toSeq).
      map(x => (x(0),x(1))).
      groupByKey.
      map(k => (k._1,k._2.toSet.toList)).collect()

    pairs.foreach((v) => println("key: ",v._1,"\nvalue: ",v._2))

    val r = new scala.util.Random()
    var word = pairs(r.nextInt(pairs.length))


    (1 to 30).foreach({ _ =>
      print(word._1, " ")
      word = pairs(r.nextInt(pairs.length))
    })






  }
}
