package scalashop

import common._

object ParallelTest {
  
  private var uidCount = 0L
  private val x = new AnyRef()
  
  def getUniqueId(): Long = {
    uidCount = uidCount + 1
    uidCount
  }
  
  def doTask() = {
    val uids = for (i <- 0 until 10) yield getUniqueId()
    println(uids)
  }
  
  def main(args: Array[String]): Unit = {
    // parallel(doTask(), doTask())
    // parallel(doTask(), doTask(), doTask(), doTask())
    parallel(
      parallel(doTask(), doTask()),
      parallel(doTask(), doTask())
    )
  }
}
