var uidCount = 0L

def getUniqueId(): Long = {
  uidCount = uidCount + 1
  uidCount
}

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      println("thread running...")
      val uids = for (i <- 0 until 10) yield getUniqueId()
      println(uids)
    }
  }
  // t.start()
  t
}

val t1 = startThread()
val t2 = startThread()
t1.start()
t2.start()
t1.join()
t2.join()