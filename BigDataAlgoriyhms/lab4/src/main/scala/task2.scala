import org.apache.spark.{SparkConf, SparkContext}


case class AvgDeg(inDeg: Int, outDeg: Int)


object task2 {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local").setAppName("Alg")
    val sc = new SparkContext(conf)

    val graph = sc.textFile("web-Stanford.txt").map(line => line.split("\\s").toList).
      map {
        case List(a, b) => a.toInt -> b.toInt
      }

    val gc = graph
    val outerDegGraph = gc.groupByKey().map(vertex => (vertex._1,AvgDeg(inDeg = 0,outDeg = vertex._2.size)))

    val gcc = graph.map(x=> x._2 -> x._1).groupByKey()
    val nnodes = gcc.collect().size
    val innerDegGraph = gcc.map(vertex => (vertex._1,AvgDeg(inDeg = vertex._2.size,outDeg = 0)))

    var zeroval = AvgDeg(0,0)
    val graph1 = (outerDegGraph union innerDegGraph).
      reduceByKey((avg1,avg2) => AvgDeg(inDeg = avg1.inDeg+avg2.inDeg,
                                        outDeg = avg1.outDeg+avg2.outDeg)).
      reduce((left,right) => (left._1 max right._1, AvgDeg(inDeg = left._2.inDeg + right._2.inDeg,
                                                            outDeg = left._2.outDeg+right._2.outDeg)))

    println(s"indeg = ${graph1._2.inDeg/graph1._1.toFloat}")
    println(s"outdeg = ${graph1._2.outDeg/graph1._1.toFloat}")
  }
}
