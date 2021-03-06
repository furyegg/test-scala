package timeusage

import java.nio.file.Paths

import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.storage.StorageLevel

/** Main class */
object TimeUsage {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .config("spark.master", "local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /** Main function */
  def main(args: Array[String]): Unit = {
    timeUsageByLifePeriod()
  }

  def timeUsageByLifePeriod(): Unit = {
    val (columns, initDf) = read("atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
//    val finalDf = timeUsageGrouped(summaryDf)
//    val finalDf = timeUsageGroupedSql(summaryDf)
    val summaryDs = timeUsageSummaryTyped(summaryDf)
    val finalDf = timeUsageGroupedTyped(summaryDs)
    finalDf.show()
  }

  /** @return The read DataFrame along with its column names. */
  def read(resource: String): (List[String], DataFrame) = {
    val rdd = spark.sparkContext.textFile(fsPath(resource))

    val headerColumns = rdd.first().split(",").to[List]
    // Compute the schema based on the first line of the CSV file
    val schema = dfSchema(headerColumns)

    val data =
      rdd
        .mapPartitionsWithIndex((i, it) => if (i == 0) it.drop(1) else it) // skip the header line
        .map(_.split(",").to[List])
        .map(row)

    val dataFrame = spark.createDataFrame(data, schema)

    (headerColumns, dataFrame)
  }

  /** @return The filesystem path of the given resource */
  def fsPath(resource: String): String =
    Paths.get(getClass.getClassLoader.getResource(resource).toURI).toString

  /** @return The schema of the DataFrame, assuming that the first given column has type String and all the others
    *         have type Double. None of the fields are nullable.
    * @param columnNames Column names of the DataFrame
    */
  def dfSchema(columnNames: List[String]): StructType = {
    val fields = StructField(columnNames.head, StringType, false) :: columnNames.tail.map(StructField(_, DoubleType, false))
//    StructType(fields)
    DataTypes.createStructType(fields.toArray)
  }


  /** @return An RDD Row compatible with the schema produced by `dfSchema`
    * @param line Raw fields
    */
  def row(line: List[String]): Row = Row.merge(
    Row(line.head),
    Row(line.tail.map(_.toDouble).toArray: _*)
  )

  /** @return The initial data frame columns partitioned in three groups: primary needs (sleeping, eating, etc.),
    *         work and other (leisure activities)
    *
    * @see https://www.kaggle.com/bls/american-time-use-survey
    *
    * The dataset contains the daily time (in minutes) people spent in various activities. For instance, the column
    * “t010101” contains the time spent sleeping, the column “t110101” contains the time spent eating and drinking, etc.
    *
    * This method groups related columns together:
    * 1. “primary needs” activities (sleeping, eating, etc.). These are the columns starting with “t01”, “t03”, “t11”,
    *    “t1801” and “t1803”.
    * 2. working activities. These are the columns starting with “t05” and “t1805”.
    * 3. other activities (leisure). These are the columns starting with “t02”, “t04”, “t06”, “t07”, “t08”, “t09”,
    *    “t10”, “t12”, “t13”, “t14”, “t15”, “t16” and “t18” (those which are not part of the previous groups only).
    */
  def classifiedColumns(columnNames: List[String]): (List[Column], List[Column], List[Column]) = {
    def startWith(column: String, includes: List[String]): Boolean = includes match {
      case Nil => false
      case x :: xs => if (column.startsWith(x)) true else startWith(column, xs)
    }
    
    val primary = columnNames.filter(startWith(_, List("t01", "t03", "t11", "t1801", "t1803"))).map(new Column(_))
    val working = columnNames.filter(startWith(_, List("t05", "t1805"))).map(new Column(_))
    val leisure = columnNames.filter(startWith(_, List("t02", "t04", "t06", "t07", "t08", "t09", "t10", "t12", "t13", "t14", "t15", "t16", "t18")))
      .filter(!startWith(_, List("t1801", "t1803", "t1805")))
      .map(new Column(_))
    
    (primary, working, leisure)
  }

  /** @return a projection of the initial DataFrame such that all columns containing hours spent on primary needs
    *         are summed together in a single column (and same for work and leisure). The “teage” column is also
    *         projected to three values: "young", "active", "elder".
    *
    * @param primaryNeedsColumns List of columns containing time spent on “primary needs”
    * @param workColumns List of columns containing time spent working
    * @param otherColumns List of columns containing time spent doing other activities
    * @param df DataFrame whose schema matches the given column lists
    *
    * This methods builds an intermediate DataFrame that sums up all the columns of each group of activity into
    * a single column.
    *
    * The resulting DataFrame should have the following columns:
    * - working: value computed from the “telfs” column of the given DataFrame:
    *   - "working" if 1 <= telfs < 3
    *   - "not working" otherwise
    * - sex: value computed from the “tesex” column of the given DataFrame:
    *   - "male" if tesex = 1, "female" otherwise
    * - age: value computed from the “teage” column of the given DataFrame:
    *   - "young" if 15 <= teage <= 22,
    *   - "active" if 23 <= teage <= 55,
    *   - "elder" otherwise
    * - primaryNeeds: sum of all the `primaryNeedsColumns`, in hours
    * - work: sum of all the `workColumns`, in hours
    * - other: sum of all the `otherColumns`, in hours
    *
    * Finally, the resulting DataFrame should exclude people that are not employable (ie telfs = 5).
    *
    * Note that the initial DataFrame contains time in ''minutes''. You have to convert it into ''hours''.
    */
  def timeUsageSummary(
    primaryNeedsColumns: List[Column],
    workColumns: List[Column],
    otherColumns: List[Column],
    df: DataFrame
  ): DataFrame = {
    
    val workingStatusUDF = udf((value: Double) => {
      val telfs = value.toInt
      if (telfs >= 1 && telfs < 3) "working" else "not working"
    }, StringType)
  
    val sexUDF = udf((value: Double) => {
      val sex = value.toInt
      if (sex == 1) "male" else "female"
    }, StringType)
  
    val ageUDF = udf((value: Double) => {
      val age =  value.toInt
      if (age >= 15 && age <= 22) "young"
      else if (age >= 23 && age <= 55) "active"
      else "elder"
    }, StringType)
  
    val sumUDF = udf((r: Row) => {
      val values = for {
        i <- 0 until r.size
      } yield r.getDouble(i)
      values.sum
    }, DoubleType)
    
    val workingStatusProjection: Column = workingStatusUDF(df("telfs")).as("workingStatus")
    val sexProjection: Column = sexUDF(df("tesex")).as("sex")
    val ageProjection: Column = ageUDF(df("teage")).as("age")
    
    val primaryNeedsProjection: Column = sumUDF(struct(primaryNeedsColumns.toArray: _*)).as("primaryNeedsTime")
    val workProjection: Column = sumUDF(struct(workColumns.toArray: _*)).as("workTime")
    val otherProjection: Column = sumUDF(struct(otherColumns.toArray: _*)).as("otherTime")
    
    df
      .select(workingStatusProjection, sexProjection, ageProjection, primaryNeedsProjection, workProjection, otherProjection)
      .where($"telfs" <= 4) // Discard people who are not in labor force
      .persist(StorageLevel.MEMORY_AND_DISK_SER)
  }
  
  private def toHour = (mins: Double) => BigDecimal(mins / 60).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble

  /** @return the average daily time (in hours) spent in primary needs, working or leisure, grouped by the different
    *         ages of life (young, active or elder), sex and working status.
    * @param summed DataFrame returned by `timeUsageSumByClass`
    *
    * The resulting DataFrame should have the following columns:
    * - working: the “working” column of the `summed` DataFrame,
    * - sex: the “sex” column of the `summed` DataFrame,
    * - age: the “age” column of the `summed` DataFrame,
    * - primaryNeeds: the average value of the “primaryNeeds” columns of all the people that have the same working
    *   status, sex and age, rounded with a scale of 1 (using the `round` function),
    * - work: the average value of the “work” columns of all the people that have the same working status, sex
    *   and age, rounded with a scale of 1 (using the `round` function),
    * - other: the average value of the “other” columns all the people that have the same working status, sex and
    *   age, rounded with a scale of 1 (using the `round` function).
    *
    * Finally, the resulting DataFrame should be sorted by working status, sex and age.
    */
  def timeUsageGrouped(summed: DataFrame): DataFrame = {
    val grouped = summed.groupBy("workingStatus", "sex", "age")
    val df = grouped.agg(
      avg("primaryNeedsTime").as("primaryNeedsAvg"),
      avg("workTime").as("workAvg"),
      avg("otherTime").as("otherAvg"))
    
    val toHourUDF = udf(toHour, DoubleType);
    val primaryNeedsAvg = toHourUDF(df("primaryNeedsAvg")).as("primaryNeedsAvg")
    val workAvg = toHourUDF(df("workAvg")).as("workAvg")
    val otherAvg = toHourUDF(df("otherAvg")).as("otherAvg")
    df.select(df("workingStatus"), df("sex"), df("age"), primaryNeedsAvg, workAvg, otherAvg)
      .orderBy("workingStatus", "sex", "age")
  }

  /**
    * @return Same as `timeUsageGrouped`, but using a plain SQL query instead
    * @param summed DataFrame returned by `timeUsageSumByClass`
    */
  def timeUsageGroupedSql(summed: DataFrame): DataFrame = {
    val viewName = s"summed"
    summed.createOrReplaceTempView(viewName)
    spark.udf.register("toHour", toHour)
    spark.sql(timeUsageGroupedSqlQuery(viewName))
  }

  /** @return SQL query equivalent to the transformation implemented in `timeUsageGrouped`
    * @param viewName Name of the SQL view to use
    */
  def timeUsageGroupedSqlQuery(viewName: String): String =
    "select workingStatus, sex, age, toHour(avg(primaryNeedsTime)) primaryNeedsAvg, toHour(avg(workTime)) workAvg, toHour(avg(otherTime)) otherAvg " +
    "from summed group by workingStatus, sex, age " +
    "order by workingStatus, sex, age"

  /**
    * @return A `Dataset[TimeUsageRow]` from the “untyped” `DataFrame`
    * @param timeUsageSummaryDf `DataFrame` returned by the `timeUsageSummary` method
    *
    * Hint: you should use the `getAs` method of `Row` to look up columns and
    * cast them at the same time.
    */
  def timeUsageSummaryTyped(timeUsageSummaryDf: DataFrame): Dataset[TimeUsageRow] =
    timeUsageSummaryDf.map(r => TimeUsageRow(
      r.getAs("workingStatus"),
      r.getAs("sex"),
      r.getAs("age"),
      r.getAs("primaryNeedsTime"),
      r.getAs("workTime"),
      r.getAs("otherTime")
    ))

  /**
    * @return Same as `timeUsageGrouped`, but using the typed API when possible
    * @param summed Dataset returned by the `timeUsageSummaryTyped` method
    *
    * Note that, though they have the same type (`Dataset[TimeUsageRow]`), the input
    * dataset contains one element per respondent, whereas the resulting dataset
    * contains one element per group (whose time spent on each activity kind has
    * been aggregated).
    *
    * Hint: you should use the `groupByKey` and `typed.avg` methods.
    */
  def timeUsageGroupedTyped(summed: Dataset[TimeUsageRow]): Dataset[TimeUsageRow] = {
    import org.apache.spark.sql.expressions.scalalang.typed
    val grouped = summed.groupByKey(r => (r.working, r.sex, r.age))
    val ds = grouped.agg(
      typed.avg(_.primaryNeeds),
      typed.avg[TimeUsageRow](_.work),
      typed.avg[TimeUsageRow](_.other)
    )
  
    ds.map {
      case ((workingStatus, sex, age), primaryNeedsMin, workMin, otherMin) => TimeUsageRow(
        workingStatus,
        sex,
        age,
        toHour(primaryNeedsMin),
        toHour(workMin),
        toHour(otherMin)
      )
    }.orderBy("working", "sex", "age")
  }
}

/**
  * Models a row of the summarized data set
  * @param working Working status (either "working" or "not working")
  * @param sex Sex (either "male" or "female")
  * @param age Age (either "young", "active" or "elder")
  * @param primaryNeeds Number of daily hours spent on primary needs
  * @param work Number of daily hours spent on work
  * @param other Number of daily hours spent on other activities
  */
case class TimeUsageRow(
  working: String,
  sex: String,
  age: String,
  primaryNeeds: Double,
  work: Double,
  other: Double
)