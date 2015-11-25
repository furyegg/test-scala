// import ChecksumAccumulator.calculate

object Summer {
	def main(args: Array[String]): Unit = {
		for (season <- List("Spring", "Summer", "Winter")) {
			println(season + ":" + ChecksumAccumulator.calculate(season))
		}
	}
}