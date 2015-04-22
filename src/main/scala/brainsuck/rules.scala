package brainsuck

object MergeMoves extends Rule[Instruction] {
  override def apply(tree: Instruction) = tree.transformUp {
    case Move(n, Move(m, next)) => if (n + m == 0) next else Move(n + m, next)
  }
}

object MergeAdds extends Rule[Instruction] {
  override def apply(tree: Instruction) = tree.transformUp {
    case Add(n, Add (m, next)) => if (n + m == 0) next else Add(n + m, next)
  }
}

object Clears extends Rule[Instruction] {
  override def apply(tree: Instruction) = tree.transformUp {
    case Loop(Add(_, Halt), next) => Clear(next)
  }
}

object Scans extends Rule[Instruction] {
  override def apply(tree: Instruction) = tree.transformUp {
    case Loop(Move(n, Halt), next) => Scan(n, next)
  }
}

object MultisAndCopies extends Rule[Instruction] {
  override def apply(tree: Instruction) = tree.transform {
    case Loop(Add(-1, MoveAddPairs(seq, offset, Move(n, Halt))), next) if n == -offset =>
      seq.foldRight(Clear(next): Instruction) {
        case ((distance, 1), code) => Copy(distance, code)
        case ((distance, increment), code) => Multi(distance, increment, code)
      }
  }
}

object MoveAddPairs {
  type ResultType = (List[(Int, Int)], Int, Instruction)

  def unapply(tree: Instruction): Option[ResultType] = {
    def loop(tree: Instruction, offset: Int): Option[ResultType] = tree match {
      case Move(n, Add(m, inner)) =>
        loop(inner, offset + n).map { case (seq, finalOffset, next) =>
          ((offset + n, m) :: seq, finalOffset, next)
        }
      case inner => Some((Nil, offset, inner))
    }
    loop(tree, 0)
  }
}
