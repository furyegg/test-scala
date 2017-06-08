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
      
      val rdd = spark.sparkContext.parallelize(List((1, "a", "true"), (2, "b", "false")))
      val rowRDD = rdd.map(t => Row(t._1, t._2, t._3))
//      val fields = StructField("id", IntegerType, false) :: List("name", "male").map(StructField(_, StringType, false))
      val fields = StructField("id", IntegerType, false) :: List("name", "male").map(StructField(_, StringType, false))
      val schema = StructType(fields)
      val df = spark.createDataFrame(rowRDD, schema)
      df.show
    }
}