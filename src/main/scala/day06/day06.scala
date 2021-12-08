package day06

import fp.Monoid.given
import syntax.*

import scala.io.Source

def input(): Vector[Int] =
  Source
    .fromResource("day06.txt")
    .getLines()
    .map(_.split(",").map(_.toInt).toVector)
    .foldLeft(Vector.empty)(_ ++ _)

type State = Map[Int, Long]

def initial(input: Vector[Int]): State =
  input.foldLeft(Map.empty) { (acc, n) =>
    acc |+| Map(n -> 1L)
  }

def simulateDay(state: State): State =
  state.foldLeft(Map.empty) {
    case (acc, (0, n)) => acc |+| Map(6 -> n, 8 -> n)
    case (acc, (m, n)) => acc |+| Map((m - 1) -> n)
  }

def simulateNDays(state: State, n: Int): State =
  (0 until n).map(_ => simulateDay).reduce(_ andThen _)(state)

def program(days: Int): Unit =
  val init = initial(input())
  val fstate = simulateNDays(init, days)
  val result = fstate.values.sum
  println(result)

@main
def part1(): Unit =
  program(80)

@main
def part2(): Unit =
  program(256)
