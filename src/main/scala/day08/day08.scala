package day08

import syntax.*

import scala.io.Source

@main
def part1(): Unit =
  val interesting = Set(2, 3, 4, 7)
  val lines = Source.fromResource("day08.txt").getLines().toVector
  lines.map { line =>
    val Array(_, right) = line.split(" \\| ")
    val output = right.split(" ")
    output.count(_.length |> interesting)
  }.sum |> println

case class InputLine(input: Set[String], output: Vector[String])

def stream(): LazyList[InputLine] =
  Source
    .fromResource("day08.txt")
    .getLines()
    .map { ln =>
      val Array(rawIn, rawOut) = ln.split(" \\| ")
      val input = rawIn.split(" ").map(_.sorted).toSet
      val output = rawOut.split(" ").map(_.sorted).toVector
      InputLine(input, output)
    }
    .to(LazyList)

def stringUnion(s1: String, s2: String): String =
  (s1.toSet union s2.toSet).mkString.sorted

def solve(line: InputLine): Int =
  // The number 1 has 2 segments and is unique
  val one = line.input.find(_.length == 2).get

  // The number 4 has 4 segments as is unique
  val four = line.input.find(_.length == 4).get

  // The number 7 has 3 segments and is unique
  val seven = line.input.find(_.length == 3).get

  // The number 8 has 7 segments and is unique
  val eight = line.input.find(_.length == 7).get

  // Numbers 2, 3 and 5 have 5 segments each
  val twoThreeFive = line.input.filter(_.length == 5)

  // Numbers 6, 9 and 0 have 6 segments each
  val sixNineZero = line.input.filter(_.length == 6)

  // 2 and 5 superimposed give 8 and thus 3 can be determined
  // by subtracting 2 and 5 from {2, 3, 5}
  val three =
    (for
      x <- twoThreeFive
      y <- twoThreeFive if y != x
      u = stringUnion(x, y) if u == eight
    yield twoThreeFive.find(t => t != x && t != y).get).head

  // 2 and 5 remain ambiguous
  val twoFive = twoThreeFive - three

  // 3 and 5 superimposed give 9, thus 9 can be determined
  val nine =
    (for
      x <- twoFive
      u = stringUnion(x, three) if sixNineZero(u)
    yield u).head

  // 6 and 0 remain ambiguous
  val sixZero = sixNineZero - nine

  // 5 and 9 superimposed give 9, thus 2 can be determined
  val two =
    (for
      x <- twoFive
      u = stringUnion(x, nine) if u == eight
    yield x).head

  // 5 = {2, 5} - 2
  val five = (twoFive - two).head

  // 1 and 6 superimposed give 8, thus 6 can be determined
  val six =
    (for
      x <- sixZero
      u = stringUnion(x, one) if u == eight
    yield x).head

  // 0 = {6, 0} - 6
  val zero = (sixZero - six).head

  // map strings to digits
  val mapping = Map(
    one -> 1,
    two -> 2,
    three -> 3,
    four -> 4,
    five -> 5,
    six -> 6,
    seven -> 7,
    eight -> 8,
    nine -> 9,
    zero -> 0
  )

  // form the final output
  line.output.map(mapping).mkString.toInt

@main
def part2(): Unit =
  println(stream().map(solve).sum)
