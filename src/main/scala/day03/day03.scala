package day03

import fp._
import syntax._

import scala.annotation.tailrec
import scala.io.Source

def stream(): LazyList[String] =
  Source
    .fromResource("day03.txt")
    .getLines()
    .to(LazyList)

enum Bit:
  case Zero
  case One

  def toInt: Int = this match
    case Zero => 0
    case One => 1

object Bit:
  def fromChar(c: Char): Bit = c match
    case '0' => Zero
    case '1' => One

type Count = Map[Bit, Int]

object Count:
  def fromBit(b: Bit): Count = Map(b -> 1)
  
opaque type BitString = Vector[Bit]

extension (bs: BitString) def hasBitAt(b: Bit)(i: Int): Boolean =
  bs(i) == b

def combineRows[A: Monoid](r1: Vector[A], r2: Vector[A]): Vector[A] =
  (r1 zip r2).map(_ |+| _)

def countBits(input: Set[BitString]): Vector[Count] =
  input.map(_.map(Count.fromBit).toVector).reduce(combineRows)

def parseBinaryString(s: BitString): Int =
  s.reverse.zipWithIndex.foldLeft(0) { case (acc, (b, i)) =>
    acc + b.toInt * math.pow(2, i).toInt
  }

def gamma(cs: Vector[Count]): Int =
  cs.map(_.maxBy((_, v) => v)._1) |> parseBinaryString

def epsilon(cs: Vector[Count]): Int =
  cs.map(_.minBy((_, v) => v)._1) |> parseBinaryString

given Ordering[(Bit, Int)] with
  def compare(p1: (Bit, Int), p2: (Bit, Int)): Int =
    val diff = p1._2 - p2._2
    if diff == 0 then p1._1.toInt - p2._1.toInt
    else diff

def oxygenGeneratorRating(set: Set[BitString]): Int =
  @tailrec
  def loop(set: Set[BitString], i: Int): BitString =
    if set.size == 1 then set.head
    else
      val counts = countBits(set)
      val b = counts(i).max._1
      loop(set.filter(_.hasBitAt(b)(i)), i + 1)

  loop(set, 0) |> parseBinaryString

def co2ScrubberRating(set: Set[BitString]): Int =
  @tailrec
  def loop(set: Set[BitString], i: Int): BitString =
    if set.size == 1 then set.head
    else
      val counts = countBits(set)
      val b = counts(i).min._1
      loop(set.filter(_.hasBitAt(b)(i)), i + 1)

  loop(set, 0) |> parseBinaryString

@main
def part1(): Unit =
  val bits = stream().map(_.map(Bit.fromChar).toVector).toSet |> countBits
  val res = gamma(bits) * epsilon(bits)
  println(res)

@main
def part2(): Unit =
  val set = stream().map(_.map(Bit.fromChar).toVector).toSet
  val res = oxygenGeneratorRating(set) * co2ScrubberRating(set)
  println(res)
