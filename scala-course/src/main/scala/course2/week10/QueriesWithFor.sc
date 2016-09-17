case class Book(title: String, authors: List[String])

val books: List[Book] = List(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

val list = for {
  b1 <- books
  b2 <- books
  if (b1.title < b2.title)
  a1 <- b1.authors
  a2 <- b2.authors
  if (a1 == a2)
} yield a1
list.toSet

books.flatMap(
  b1 =>
    books.withFilter(book => b1.title < book.title)
        .flatMap(
          b2 => b1.authors.flatMap(
            a1 => b2.authors.withFilter(author => a1 == author).map(a2 => a1)
          )
        )
)