package day10

import syntax.*

import scala.io.Source

def stream(): LazyList[String] =
  Source
    .fromResource("day10.txt")
    .getLines()
    .to(LazyList)

enum CloseSymbol(val errorValue: Int, val autocompleteValue: Int):
  case Paren extends CloseSymbol(3, 1)
  case Square extends CloseSymbol(57, 2)
  case Curly extends CloseSymbol(1197, 3)
  case Angle extends CloseSymbol(25137, 4)

object CloseSymbol:
  def fromChar(c: Char): CloseSymbol = c match
    case ')' => CloseSymbol.Paren
    case ']' => CloseSymbol.Square
    case '}' => CloseSymbol.Curly
    case '>' => CloseSymbol.Angle

enum OpenSymbol(val matching: CloseSymbol):
  case Paren extends OpenSymbol(CloseSymbol.Paren)
  case Square extends OpenSymbol(CloseSymbol.Square)
  case Curly extends OpenSymbol(CloseSymbol.Curly)
  case Angle extends OpenSymbol(CloseSymbol.Angle)

object OpenSymbol:
  def fromChar(c: Char): OpenSymbol = c match
    case '(' => OpenSymbol.Paren
    case '[' => OpenSymbol.Square
    case '{' => OpenSymbol.Curly
    case '<' => OpenSymbol.Angle

def parseSymbol(c: Char): Either[OpenSymbol, CloseSymbol] = c match
  case '(' | '[' | '{' | '<' => Left(OpenSymbol.fromChar(c))
  case ')' | ']' | '}' | '>' => Right(CloseSymbol.fromChar(c))

def solveLine(line: String): Either[List[OpenSymbol], Int] =
  line.foldLeft[Either[List[OpenSymbol], Int]](Left(List.empty[OpenSymbol])) {
    (state, char) =>
      state match
        case Left(stack) =>
          parseSymbol(char) match
            case Left(os) => Left(os :: stack)
            case Right(cs) =>
              stack match
                case Nil => Right(cs.errorValue)
                case h :: t =>
                  if h.matching == cs then Left(t)
                  else Right(cs.errorValue)
        case Right(o) => Right(o)
  }

def errorPoints(solution: Either[List[OpenSymbol], Int]): Int =
  solution.fold(_ => 0, identity)

def calculateAutocomplete(points: List[Int]): Long =
  points.foldLeft(0L)(5 * _ + _)

def autocompletePoints(solution: Either[List[OpenSymbol], Int]): Long =
  solution.fold(
    _.map(_.matching.autocompleteValue) |> calculateAutocomplete,
    _ => 0L
  )

@main
def part1(): Unit =
  val result = stream().map(solveLine).map(errorPoints).sum
  println(result)

@main
def part2(): Unit =
  val result = stream()
    .map(solveLine)
    .map(autocompletePoints)
    .filterNot(_ == 0)
    .toVector
    .sorted
  val middle = result.size / 2
  println(result(middle))
