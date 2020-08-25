package brainsuck

sealed trait Instruction extends TreeNode[Instruction] {
  def next: Instruction
  def run(m: Machine): Unit
}

sealed trait LeafInstruction extends Instruction with LeafNode[Instruction] {
  self: Instruction =>
  def next: Instruction = this
}

sealed trait UnaryInstruction extends Instruction with UnaryNode[Instruction] {
  self: Instruction =>
  def next: Instruction = child
}

case object Halt extends LeafInstruction {
  override def run(m: Machine) = ()
}

case class Add(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = m.value += n
}

case class Move(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = m.pointer += n
}

case class Scan(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = while (m.value != 0) m.pointer += n
}

case class Out(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = print(m.value.toChar)
}

case class In(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = m.value = System.in.read()
}

case class Loop(body: Instruction, next: Instruction) extends Instruction {
  override def children = Seq(body, next)
  override protected def makeCopy(args: Seq[Instruction]) = copy(body = args.head, next = args.last)
  override def run(m: Machine) = while (m.value != 0) Instruction.untilHalt(body, m)
}

case class Clear(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = m.value = 0
}

case class Copy(offset: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = m.memory(m.pointer + offset) += m.value
}

case class Multi(offset: Int, n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(m: Machine) = m.memory(m.pointer + offset) += m.value * n
}

object Instruction {
  def untilHalt(code: Instruction, m: Machine): Unit = {
    var next = code
    while (next ne Halt) {
      next.run(m)
      next = next.next
    }
  }
}
