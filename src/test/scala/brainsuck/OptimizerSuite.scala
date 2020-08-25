package brainsuck

import org.scalatest.funsuite.AnyFunSuite

class OptimizerSuite extends AnyFunSuite with TestUtils {
  test("contraction") {
    val check = checkExecutionWithOptimizer(contractionOptimizer) _

    check(
      "+-",
      Halt,
      makeMachine(0, Seq(0)),
      makeMachine(0, Seq(0))
    )

    check(
      "+++",
      Add(3, Halt),
      makeMachine(0, Seq(0)),
      makeMachine(0, Seq(3))
    )

    check(
      "---",
      Add(-3, Halt),
      makeMachine(0, Seq(0)),
      makeMachine(0, Seq(-3))
    )

    check(
      "++--++",
      Add(2, Halt),
      makeMachine(0, Seq(0)),
      makeMachine(0, Seq(2))
    )

    check(
      "<>",
      Halt,
      makeMachine(0, Seq(0)),
      makeMachine(0, Seq(0))
    )

    check(
      ">>>",
      Move(3, Halt),
      makeMachine(0, Seq(0)),
      makeMachine(3, Seq(0, 0, 0, 0))
    )

    check(
      "<<<",
      Move(-3, Halt),
      makeMachine(3, Seq(0, 0, 0, 0)),
      makeMachine(0, Seq(0))
    )

    check(
      ">><<>>",
      Move(2, Halt),
      makeMachine(0, Seq(0)),
      makeMachine(2, Seq(0, 0, 0))
    )
  }

  test("loop simplification") {
    val check = checkExecutionWithOptimizer(fullOptimizer) _

    check(
      "[<]",
      Scan(-1, Halt),
      makeMachine(2, Seq(0, 1, 1)),
      makeMachine(0, Seq(0, 1, 1))
    )

    check(
      "[>]",
      Scan(1, Halt),
      makeMachine(0, Seq(1, 1, 0)),
      makeMachine(2, Seq(1, 1, 0))
    )

    check(
      "[-]",
      Clear(Halt),
      makeMachine(0, Seq(3)),
      makeMachine(0, Seq(0))
    )

    check(
      "[->+<]",
      Copy(1, Clear(Halt)),
      makeMachine(0, Seq(3, 0)),
      makeMachine(0, Seq(0, 3))
    )

    check(
      "[-<<<+>>>]",
      Copy(-3, Clear(Halt)),
      makeMachine(3, Seq(0, 0, 0, 3)),
      makeMachine(3, Seq(3, 0, 0, 0))
    )

    check(
      "[-<<<->>>]",
      Multi(-3, -1, Clear(Halt)),
      makeMachine(3, Seq(0, 0, 0, 3)),
      makeMachine(3, Seq(-3, 0, 0, 0))
    )

    check(
      "[-<<<++>>>]",
      Multi(-3, 2, Clear(Halt)),
      makeMachine(3, Seq(0, 0, 0, 3)),
      makeMachine(3, Seq(6, 0, 0, 0))
    )

    check(
      "[-<<<-->>>]",
      Multi(-3, -2, Clear(Halt)),
      makeMachine(3, Seq(0, 0, 0, 3)),
      makeMachine(3, Seq(-6, 0, 0, 0))
    )

    check(
      "[->+<]+",
      Copy(1, Clear(Add(1, Halt))),
      makeMachine(0, Seq(3, 0)),
      makeMachine(0, Seq(1, 3))
    )

    check(
      "[->+>+<<]",
      Copy(1, Copy(2, Clear(Halt))),
      makeMachine(0, Seq(3)),
      makeMachine(0, Seq(0, 3, 3))
    )

    check(
      "[->>>+++++<<<]",
      Multi(3, 5, Clear(Halt)),
      makeMachine(0, Seq(2)),
      makeMachine(0, Seq(0, 0, 0, 10))
    )

    check(
      "[->>++>+<<<]",
      Multi(2, 2, Copy(3, Clear(Halt))),
      makeMachine(0, Seq(2)),
      makeMachine(0, Seq(0, 0, 4, 2))
    )

    check(
      "++[-<<++>->]>",
      Add(2, Multi(-2, 2, Multi(-1, -1, Clear(Move(1, Halt))))),
      makeMachine(2, Seq(0, 0, 0)),
      makeMachine(3, Seq(4, -2, 0))
    )

    check(
      "[-<>++]",
      Clear(Halt),
      makeMachine(0, Seq(-4)),
      makeMachine(0, Seq(0))
    )

    check(
      "[->+>---<<]",
      Copy(1, Multi(2, -3, Clear(Halt))),
      makeMachine(0, Seq(2)),
      makeMachine(0, Seq(0, 2, -6))
    )

    check(
      "[->>>+<<<]",
      Copy(3, Clear(Halt)),
      makeMachine(0, Seq(3)),
      makeMachine(0, Seq(0, 0, 0, 3))
    )
  }

  test("move-add pairs") {
    def parse(code: String) = contractionOptimizer(BrainsuckParser(code))

    parse(">+>>++") match {
      case MoveAddPairs((1, 1) :: (3, 2) :: Nil, 3, Halt) =>
      case _                                              => fail()
    }

    parse("") match {
      case MoveAddPairs(Nil, 0, Halt) =>
      case _                          => fail()
    }

    parse(">-") match {
      case MoveAddPairs((1, -1) :: Nil, 1, Halt) =>
      case _                                     => fail()
    }

    parse(">+<++") match {
      case MoveAddPairs((1, 1) :: (0, 2) :: Nil, 0, Halt) =>
      case _                                              => fail()
    }

    parse("<<++>-") match {
      case MoveAddPairs((-2, 2) :: (-1, -1) :: Nil, -1, Halt) =>
      case _                                                  => fail()
    }
  }
}
