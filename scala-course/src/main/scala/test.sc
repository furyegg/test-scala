val arr1 = Array(1)
val (l, r) = arr1.splitAt(arr1.length / 2)
l.foldLeft("")((s, n) => s+","+n)
r.foldLeft("")((s, n) => s+","+n)