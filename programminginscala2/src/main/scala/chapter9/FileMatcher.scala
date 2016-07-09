package chapter9

object FileMatcher {
	private def filesHere = (new java.io.File(".")).listFiles

	def filesEnding(query: String) =
		for (file <- filesHere;
			 if file.getName.endsWith(query))
			yield file

	def filesContaining(query: String) =
		for (file <- filesHere;
			 if file.getName.contains(query))
			yield file

	def filesRegex(query: String) =
		for (file <- filesHere;
			 if file.getName.matches(query))
			yield file

	def filesMatching(query: String,
					  matcher: (String, String) => Boolean) = {
		for (file <- filesHere;
			 if matcher(file.getName, query))
			yield file
	}

	def filesEnding2(query: String) = filesMatching(query, (fileName: String, query: String) => fileName.endsWith(query))
	def filesEnding3(query: String) = filesMatching(query, _.endsWith(_))

	private def filesMatching2(matcher: String => Boolean) =
		for (file <- filesHere;
			 if matcher(file.getName))
			yield file

	def filesEnding4(query: String) = filesMatching2(_.endsWith(query))
}