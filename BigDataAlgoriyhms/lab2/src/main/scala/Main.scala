import scala.io.Source
import java.io._

import scala.util.{Random, hashing}

object kek {



  def main(args: Array[String]): Unit = {

    def hashf() = {
      val a = Random.nextInt(10)
      val b = Random.nextInt(10)
      val c = 6
      (x: Int) => (a+x*b)%c
    }

    def kshingle(f: String, k: Int): Set[String] = {
      val file = Source.fromFile(f, "UTF-8").mkString.toLowerCase.
        replaceAll("""[\p{Punct}]""", "").split("\\s+").
        toList.mkString(" ").sliding(k).toSet

      file
    }

    def minhashJaccard(f1: String, f2: String, k: Int, H:Int): Double = {
      val shingledF1 = kshingle(f=f1, k=k)
      val shingledF2 = kshingle(f=f2, k=k)

      val hashfs = List.fill(H)(hashf())
      val charmatrix = (shingledF1.map((_,1))++shingledF2.map((_,2))).toSeq.
        sortBy(_._1).
        groupBy(_._1).
        map( kv => (kv._1, kv._2.map( _._2) )).
        toList.
        sortBy(_._1).
        zipWithIndex.
        map{
          case ((s, List(1)),i) => (i,List(0,1))
          case ((s, List(2)),i) => (i,List(1,0))
          case ((s, List(1,2)),i) => (i,List(1,1))
          case ((s, List(2,1)),i) => (i,List(1,1))
        }.toList


      charmatrix.foreach({ (i,(t1,t2)) =>

      })

      println(charmatrix)
      2.0
    }



    def jaccard(f1: String, f2: String, k: Int): Double = {

      val file1 = kshingle(f=f1, k=k)
      val file2 = kshingle(f=f2, k=k)

      val union = file1.toSet.union(file2.toSet).size
      val intersection = file1.toSet.intersect(file2.toSet).size

      (union-intersection).toFloat/union.toFloat
    }

    println(minhashJaccard("f1.txt", "f2.txt", 7,20))
  }
}