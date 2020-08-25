package brainsuck

import scala.util.parsing.combinator.RegexParsers

class BrainsuckParser extends RegexParsers {
  def apply(input: String) =
    parseAll(instructions, input) match {
      case Success(compiled, _) => compiled
      case failureOrError       => sys.error(failureOrError.toString)
    }

  def instructions: Parser[Instruction] =
    instruction.* ^^ {
      case seq => seq.foldRight(Halt: Instruction)(_ apply _)
    }

  def instruction: Parser[Instruction => Instruction] =
    (
      "+" ^^^ { Add(1, _) }
        | "-" ^^^ { Add(-1, _) }
        | "<" ^^^ { Move(-1, _) }
        | ">" ^^^ { Move(1, _) }
        | "." ^^^ { Out(_) }
        | "," ^^^ { In(_) }
        | "[" ~> instructions <~ "]" ^^ {
          case body => Loop(body, _)
        }
    )
}

object BrainsuckParser {
  def apply(input: String) = (new BrainsuckParser)(input)
}
