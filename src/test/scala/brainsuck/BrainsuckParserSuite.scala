package brainsuck

import org.scalatest.funsuite.AnyFunSuite

class BrainsuckParserSuite extends AnyFunSuite {

  test("parse") {
    def check(source: String)(code: => Instruction): Unit = {
      assert(BrainsuckParser(source) === code)
    }

    check("+")(Add(1, Halt))
    check("+>")(Add(1, Move(1, Halt)))
    check("+<>")(Add(1, Move(-1, Move(1, Halt))))
    check("+[-]>")(Add(1, Loop(Add(-1, Halt), Move(1, Halt))))
    check(".,")(Out(In(Halt)))
  }
}
