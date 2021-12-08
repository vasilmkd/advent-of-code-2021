package day05

import scala.io.Source

case class Point(x: Int, y: Int)

object Point:
  def parse(s: String): Point =
    s.split(",").map(_.toInt) match
      case Array(x, y) => Point(x, y)

case class Line private (start: Point, end: Point):
  def isHorizontal: Boolean =
    start.y == end.y

  def isVertical: Boolean =
    start.x == end.x

object Line:
  def parse(s: String): Line =
    s.split(" -> ") match
      case Array(first, second) =>
        val Vector(start, end) = Vector(Point.parse(first), Point.parse(second)).sortBy(p => 1000 * p.x + p.y)
        Line(start, end)
        

def stream(): LazyList[Line] =
  Source
    .fromResource("day05.txt")
    .getLines()
    .map(Line.parse)
    .to(LazyList)

@main
def part1(): Unit =
  val board = Array.ofDim[Int](1000, 1000)
  stream().filter(l => l.isHorizontal || l.isVertical).foreach { line =>
    for
      x <- line.start.x to line.end.x
      y <- line.start.y to line.end.y
    do
      board(y)(x) += 1
  }

  val result = board.map(_.count(_ >= 2)).sum
  println(result)

@main
def part2(): Unit =
  val board = Array.ofDim[Int](1000, 1000)
  stream().foreach { line =>
    if line.isHorizontal || line.isVertical then
      for
        x <- line.start.x to line.end.x
        y <- line.start.y to line.end.y
      do
        board(y)(x) += 1
    else
      var y = line.start.y
      val offset = if line.start.y < line.end.y then 1 else -1
      for x <- line.start.x to line.end.x do
        board(y)(x) += 1
        y += offset
  }

  val result = board.map(_.count(_ >= 2)).sum
  println(result)
