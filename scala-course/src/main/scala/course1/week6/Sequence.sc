def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case Nil => None
  case list: List[Some[A]] => list match {
    case Nil => Some(Nil)
    case x :: xs => Some(sequence(xs).get :+ x.get)
  }
  case _ => None
}

def sequence2[A](l: List[Option[A]]): Option[List[A]] = l match {
  case Nil => Some(Nil)
  case h :: t => h match {
    case None => None
    case Some(head) => sequence2(t) match {
      case None => None
      case Some(list) => Some(head :: list)
    }
  }
}

def sequence3[A](l: List[Option[A]]): Option[List[A]] = (Option(List.empty[A]) /: l) {
  case(Some(sofar), Some(value)) => Some(value :: sofar);
  case(_, _) => None
}

val map = Map(1 -> "a")