package day01

import syntax._

import scala.io.Source

def stream(): LazyList[Int] =
  Source.fromResource("day01.txt").getLines().map(_.toInt).to(LazyList)

def countPairwiseIncreases(l: LazyList[Int]): Int =
  l.sliding(2)
    .collect {
      case Seq(d1, d2) => if d2 > d1 then 1 else 0
    }
    .sum

def sumTriples(l: LazyList[Int]): LazyList[Int] =
  l.sliding(3).map(_.sum).to(LazyList)

@main
def part1(): Unit =
  val res = stream() |> countPairwiseIncreases
  println(res)

@main
def part2(): Unit =
  val res = stream() |> sumTriples |> countPairwiseIncreases
  println(res)
