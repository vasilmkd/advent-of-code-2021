package day07

import scala.io.Source

def input(): Vector[Int] =
  Source
    .fromResource("day07.txt")
    .getLines()
    .map(_.split(",").map(_.toInt).toVector)
    .reduce(_ ++ _)

def diff1(n: Int, position: Int): Long =
  math.abs(n - position).toLong

def diff2(n: Int, position: Int): Long =
  val m = math.abs(n - position).toLong
  (m * (m + 1)) / 2

def totalFuel(
    crabs: Vector[Int],
    position: Int,
    diff: (Int, Int) => Long
): Long =
  crabs.map(n => diff(n, position)).sum

def program(diff: (Int, Int) => Long): Unit =
  val crabs = input()
  val max = crabs.max
  val result = (0 until max).map(totalFuel(crabs, _, diff)).min
  println(result)

@main
def part1(): Unit =
  program(diff1)

@main
def part2(): Unit =
  program(diff2)
