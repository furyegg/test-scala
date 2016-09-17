package course3.week11

object ParallelismJVM1 {
  private var uidCount = 0L
  private val x = new AnyRef()
  
  def getUniqueId(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }
  
  def startThread() = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUniqueId()
        println(uids)
      }
    }
    t.start()
    t
  }
  
  def main(args: Array[String]): Unit = {
    startThread()
    startThread()
  }
}
