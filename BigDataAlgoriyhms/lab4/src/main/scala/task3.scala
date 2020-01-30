import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object task3 {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local").setAppName("Alg")
    val sc = new SparkContext(conf)

    val graph = sc.textFile("web-Stanford.txt").map(line => line.split("\\s").toList).
      map {
        case List(a, b) => a.toInt -> b.toInt
      }
    val acg = graph
    val revGraph = acg.map(a => a._2 -> a._1)

    val undirGraph: RDD[(Int,Iterable[Int])] = (graph union revGraph).groupByKey()

    val colGraph = undirGraph.collectAsMap()

    val cc: RDD[(Int,Float,Int)] = undirGraph.map(kvs => {
      val k = kvs._1
      val vs = kvs._2.toSeq
      val lowFrac = vs.size*(vs.size-1)
      var acc = 0
      for (elem <- vs) {
        acc = acc + colGraph.find(tup => tup._1 == k).toSeq.head._2.toSeq.intersect(vs).size
      }
      (k,acc/lowFrac.toFloat,vs.size)
    })

    val ccAvg = cc.reduce((acc,value) => (acc._1 max value._1,acc._2 + value._2,0))


    cc.take(5).foreach(println)
//    println(ccAvg._2/ccAvg._1)
  }
}
