import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.io.Source

object lab4 {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local").setAppName("Alg")
    val sc = new SparkContext(conf)

    def myParser(line: String) = {
      val parsed = line.replaceAll("[^,\\d]", "").split(",").toList
      parsed
    }

    val graph = sc.textFile("graph.txt").map(myParser).filter(_.length > 1).
      map{
        case h :: tail => (h,tail)
      }.
      flatMap{ vertex =>
        var p2 = List[(Int,Int)]()
        for (elem <- vertex._2) {
          p2 = (elem.toInt, vertex._1.toInt) :: p2
        }
      p2
      }.groupByKey()


    graph.collect().foreach(f=>{
      print(f)
    })
  }
}
