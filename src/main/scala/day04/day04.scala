package day04

import syntax.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

type Combination = Set[Int]

case class Bingo(sum: Int)

object Bingo:
  def fromBoard(cs: Set[Set[Int]]): Bingo =
    val sum = cs.flatten.sum
    Bingo(sum)

case class Board(private val cs: Set[Combination]):
  def attempt(n: Int): Board | Bingo =
    val ncs = cs.map(_.filterNot(_ == n))
    if ncs.exists(_.isEmpty) then Bingo.fromBoard(ncs) else Board(ncs)

object Board:
  def fromRaw(raw: Vector[Vector[Int]]): Board =
    val rows = raw.map(_.toSet)
    val cols =
      for j <- 0 until raw(0).length
      yield (for i <- 0 until raw.length
      yield raw(i)(j)).toSet
    Board(rows.toSet union cols.toSet)

case class Result(bingo: Bingo, lastNumber: Int, count: Int)

def playBingo(board: Board, numbers: Vector[Int]): Option[Result] =
  @tailrec
  def loop(board: Board, numbers: List[Int], count: Int): Option[Result] =
    numbers match
      case Nil => None
      case n :: t =>
        board.attempt(n) match
          case nb: Board    => loop(nb, t, count + 1)
          case bingo: Bingo => Some(Result(bingo, n, count))

  loop(board, numbers.toList, 1)

def program(cmp: Vector[Result] => Result): Unit =
  val lines = Source.fromResource("day04.txt").getLines() ++ Iterator("")
  val numbers = lines.next().split(",").map(_.toInt).toVector
  lines.next()

  var bestResult: Option[Result] = None

  val buffer = mutable.Buffer.empty[Vector[Int]]
  while lines.hasNext do
    val ln = lines.next()
    if ln == "" then
      val raw = buffer.toVector
      buffer.clear()
      val board = Board.fromRaw(raw)
      val result = playBingo(board, numbers)
      for res <- result do
        bestResult match
          case None     => bestResult = result
          case Some(br) => bestResult = Some(cmp(Vector(br, res)))
    else buffer += ln.split("\\s+").filterNot(_.isEmpty).toVector.map(_.toInt)

  bestResult match
    case None => ()
    case Some(res) =>
      val out = res.bingo.sum * res.lastNumber
      println(out)

@main
def part1(): Unit =
  program(_.minBy(_.count))

@main
def part2(): Unit =
  program(_.maxBy(_.count))
