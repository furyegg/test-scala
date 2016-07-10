import scala.io.Source

val file = "d:/code.txt"

val lines = Source.fromFile(file).getLines().toList

def widthOfLineNum(line: String): Int = line.length.toString.length

val longestLine = lines.reduceLeft(
  (a, b) => if (a.length > b.length) a else b
)

def getPadding(line: String, longestLine: String): String = {
  val spaceCount = widthOfLineNum(longestLine) - widthOfLineNum(line)
  " " * spaceCount
}

for (line <- lines)
  println(getPadding(line, longestLine) + line.length + " | " + line)