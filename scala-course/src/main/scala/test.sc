type Set = Int => Boolean

def contains(s: Set, elem: Int): Boolean = s(elem)

def singletonSet(elem: Int): Set = x => elem == x

val s1 = singletonSet(1)
val s2 = singletonSet(2)
val s3 = singletonSet(2)
contains(s1, 1)
contains(s1, 10)
def union(s: Set, t: Set): Set = x => if (contains(s, x)) true else contains(t, x)
val sUnion = union(s1, s2)
contains(sUnion, 1)
contains(sUnion, 2)
contains(sUnion, 3)
def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)
val sInts = intersect(sUnion, s3)
contains(sInts, 1)
contains(sInts, 2)
def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)
val sDiff = diff(sUnion, s3)
contains(sDiff, 1)
contains(sDiff, 2)

def filter(s: Set, p: Int => Boolean): Set = x => contains(s, x) && p(x)
val sFlt = filter(sUnion, x => x > 1)
contains(sFlt, 1)
contains(sFlt, 2)