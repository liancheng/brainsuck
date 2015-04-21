package brainsuck

import scala.util.parsing.combinator.RegexParsers

class BrainsuckParser extends RegexParsers {
  def apply(input: String) = parseAll(instructions, input) match {
    case Success(compiled, _) => compiled
    case failureOrError => sys.error(failureOrError.toString)
  }

  def instructions: Parser[Instruction] = instruction.* ^^ {
    case seq => seq.foldRight(Halt: Instruction)(_ apply _)
  }

  def instruction: Parser[Instruction => Instruction] =
    ( "+" ^^^ { (next: Instruction) => Add(1, next) }
    | "-" ^^^ { (next: Instruction) => Add(-1, next) }
    | "<" ^^^ { (next: Instruction) => Move(-1, next) }
    | ">" ^^^ { (next: Instruction) => Move(1, next) }
    | "." ^^^ { (next: Instruction) => Out(next) }
    | "," ^^^ { (next: Instruction) => In(next) }
    | "[" ~> instructions <~ "]" ^^ { case body => (next: Instruction) => Loop(body, next) }
    )
}

object BrainsuckParser {
  def apply(input: String) = (new BrainsuckParser)(input)
}
