package timeusage

import org.apache.spark.sql.{Row, SparkSession}
import org.apache.spark.sql.types._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {
    test("create StructType") {
      val spark = SparkSession.builder().appName("test").master("local").getOrCreate()
      
      val rdd = spark.sparkContext.parallelize(List((1, "1.17e+07", "1.1"), (2, "10.1", "2.2")))
      val rowRDD = rdd.map(t => {
        val row1 = Row(t._1)
        val row2 = Row(List(t._2, t._3).map(_.toDouble).toArray: _*)
        Row.merge(row1, row2)
//        val numbers = List(t._2, t._3).map(_.toDouble).toArray: _*
//        Row(t._1, numbers)
      })
//      val rowRDD = rdd.map(t => Row(t._1, List("1.1", "2.2").map(_.toDouble).toArray))
      val fields = StructField("id", IntegerType, false) :: StructField("n1", DoubleType, false) :: StructField("n2", DoubleType, false) :: Nil
      val schema = StructType(fields)
      val df = spark.createDataFrame(rowRDD, schema)
      df.show
    }
}