object example {

  case class BinTree[T](value: T, left: Option[BinTree[T]], right: Option[BinTree[T]])

  def topView[T](root: BinTree[T]): Seq[T] = {
    val records = traverse(root, 0, 0)
    val map = records.groupBy(_.dist) mapValues { s ⇒
      assert(s.nonEmpty)
      s.reduce((a, b) ⇒ if (a.depth < b.depth) a else b)
    }
    map.toSeq.sortBy(_._1).map(_._2.value)
  }
  
  private def traverse[T](tree: BinTree[T], offset: Int, depth: Int): Vector[Record[T]] = {
    val left = tree.left.fold(Vector.empty[Record[T]])(traverse(_, offset - 1, depth + 1))
    val right = tree.right.fold(Vector.empty[Record[T]])(traverse(_, offset + 1, depth + 1))
    Record(tree.value, offset, depth) +: (left ++ right)
  }

  case class Record[T](value: T, dist: Int, depth: Int)

  implicit class TreeBuilder[T](value: T) {
    def apply(left: BinTree[T], right: BinTree[T]) = BinTree(value, Some(left), Some(right))
    def apply(left: Unit, right: BinTree[T]) = BinTree(value, None, Some(right))
    def apply(left: BinTree[T], right: Unit) = BinTree(value, Some(left), None)
  }
  implicit def valueToTree[T](value: T): BinTree[T] = BinTree(value, None, None)


  val data = 3 (
    5 (
      1 (
        (),
        9
      ),
      4
    ),
    2 (
      6,
      7 (
        8,
        ()
      )
    )
  )

  val data2 = 3 (
    5 (
      (),
      1 (
        4 (
          9,
          ()
        ),
        ()
      )
    ),
    2 (
      6 (
        (),
        7 (
          (),
          8
        )
      ),
      ()
    )
  )
  
  assert(topView(data) == Seq(1, 5, 3, 2, 7))
  assert(topView(data2) == Seq(9, 5, 3, 2, 8))
}
