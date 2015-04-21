package brainsuck

import org.scalatest.FunSuite

class TreeNodeSuite extends FunSuite {
  test("transformUp") {
    assertResult(Add(1, Add(2, Add(3, Halt)))) {
      Add(5, Add(4, Add(3, Halt))) transformUp {
        case Add(_, Add(n, next)) => Add(n - 1, Add(n, next))
      }
    }
  }

  test("transformDown") {
    assertResult(Add(1, Add(2, Add(3, Halt)))) {
      Add(1, Add(1, Add(1, Halt))) transformDown {
        case Add(n, Add(_, next)) => Add(n, Add((n + 1).toByte, next))
      }
    }
  }
}
