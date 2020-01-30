import org.apache.spark.{SparkConf, SparkContext}

case class Task(min: Int, max: Int, avg: Double, set:Set[Int])

object Alg {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local").setAppName("Alg")
    val sc = new SparkContext(conf)
    val rand = scala.util.Random
    val setOfInt = (1 to 100000).map(i => (i,rand.nextInt(Int.MaxValue)))
    val rddsetOfInt = sc.parallelize(setOfInt)

    def seq(x: Task, y: (Int,Int)): Task = {
      Task(min = if (x.min > y._2) y._2 else x.min,
        max = if (x.max < y._2) y._2 else x.max,
        avg = (x.avg*y._1+y._2)/(y._1+1),
        set = x.set + y._2)
    }

    def combo(x: Task, y: Task): Task = {
      Task(
        min = if (x.min < y.min) x.min else y.min,
        max = if (x.max > y.max) x.max else y.max,
        avg = (x.avg + y.avg),
        set = x.set ++ y.set
      )
    }

    val zeroElem = new Task(min = Int.MaxValue,max = Int.MinValue,avg = 0, set = Set())
    val answer = rddsetOfInt.aggregate(zeroElem)(seqOp = seq, combOp = combo)
    println(answer.min,answer.max,answer.avg)
//    println(answer.set)
    println(answer.set.size)
  }
}
