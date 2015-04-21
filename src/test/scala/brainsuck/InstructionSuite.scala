package brainsuck

import org.scalatest.FunSuite

class InstructionSuite extends FunSuite with TestUtils {
  test("add") {
    checkExecution(
      makeMachine(0, Seq(0)),
      Add(1, Halt),
      makeMachine(0, Seq(1)))

    checkExecution(
      makeMachine(0, Seq(-1)),
      Add(1, Halt),
      makeMachine(0, Seq(0)))
  }

  test("move") {
    checkExecution(
      makeMachine(0, Seq(0)),
      Move(1, Halt),
      makeMachine(1, Seq(0, 0)))

    checkExecution(
      makeMachine(1, Seq(0, 0)),
      Move(-1, Halt),
      makeMachine(0, Seq(0, 0)))
  }

  test("scan") {
    checkExecution(
      makeMachine(0, Seq(1, 1, 0)),
      Scan(1, Halt),
      makeMachine(2, Seq(1, 1, 0)))

    checkExecution(
      makeMachine(0, Seq(1, 1, 1, 1, 0)),
      Scan(2, Halt),
      makeMachine(4, Seq(1, 1, 1, 1, 0)))

    checkExecution(
      makeMachine(2, Seq(0, 1, 1)),
      Scan(-1, Halt),
      makeMachine(0, Seq(0, 1, 1)))

    checkExecution(
      makeMachine(4, Seq(0, 1, 1, 1, 1)),
      Scan(-2, Halt),
      makeMachine(0, Seq(0, 1, 1, 1, 1)))
  }

  test("loop") {
    checkExecution(
      makeMachine(0, Seq(1, 1, 0)),
      Loop(Add(1, Move(1, Halt)), Halt),
      makeMachine(2, Seq(2, 2, 0)))
  }

  test("clear") {
    checkExecution(
      makeMachine(1, Seq(0, 3)),
      Clear(Halt),
      makeMachine(1, Seq(0, 0)))
  }
}
