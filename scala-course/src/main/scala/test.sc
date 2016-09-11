val map = Map(("a"->1),("b"->2))
map.foreach(e => println(s"${e._1}->${e._2}"))
// .foreach((k, v) => println(s"${k}->${v}"))