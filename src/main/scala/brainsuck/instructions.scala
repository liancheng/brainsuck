package brainsuck

sealed trait Instruction extends TreeNode[Instruction] {
  def next: Instruction
  def run(machine: Machine): Unit
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
  override def run(machine: Machine) = ()
}

case class Add(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.value += n
}

case class Move(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.pointer += n
}

case class Scan(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = {
    while (machine.value != 0) {
      machine.pointer += n
    }
  }
}

case class Out(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = print(machine.value.toChar)
}

case class In(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.value = System.in.read()
}

case class Loop(body: Instruction, next: Instruction) extends Instruction {
  override def children = Seq(body, next)
  override protected def makeCopy(args: Seq[Instruction]) = copy(body = args.head, next = args.last)
  override def run(machine: Machine) = {
    while (machine.value != 0) {
      Instruction.untilHalt(body, machine)
    }
  }
}

case class Clear(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.value = 0
}

case class Copy(offset: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.memory(machine.pointer + offset) += machine.value
}

case class Multi(offset: Int, n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.memory(machine.pointer + offset) += machine.value * n
}

object Instruction {
  def untilHalt(code: Instruction, machine: Machine): Unit = {
    var next = code
    while (next ne Halt) {
      next.run(machine)
      next = next.next
    }
  }
}
