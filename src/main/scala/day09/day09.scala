package day09

import syntax.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

def heightMap(): IArray[IArray[Int]] =
  val blah = Source
    .fromResource("day09.txt")
    .getLines()
    .map(_.map(_.toString.toInt).toArray |> IArray.apply)
    .toList
  IArray(blah: _*)

def validPoint(hmap: IArray[IArray[Int]])(x: Int, y: Int): Boolean =
  x >= 0 && x < hmap.length && y >= 0 && y < hmap(0).length

def adjacent(hmap: IArray[IArray[Int]])(x: Int, y: Int): Vector[(Int, Int)] =
  Vector((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
    .filter(validPoint(hmap).tupled)

def checkPoint(hmap: IArray[IArray[Int]])(x: Int, y: Int): Boolean =
  val pv = hmap(x)(y)
  adjacent(hmap)(x, y).map(hmap(_)(_)).forall(pv < _)

@main
def part1(): Unit =
  val hmap = heightMap()
  val ps =
    for
      x <- (0 until hmap.length)
      y <- (0 until hmap(0).length) if checkPoint(hmap)(x, y)
    yield hmap(x)(y) + 1
  println(ps.sum)

def bfs(hmap: IArray[IArray[Int]])(x: Int, y: Int): Int =
  @tailrec
  def loop(queue: Queue[(Int, Int)], visited: Set[(Int, Int)], acc: Int): Int =
    queue match
      case hd +: tl =>
        val (x, y) = hd
        if hmap(x)(y) == 9 then loop(tl, visited + (x -> y), acc)
        else if visited(hd) then loop(tl, visited + (x -> y), acc)
        else
          val adj = adjacent(hmap)(x, y).filterNot(visited)
          loop(tl ++ adj, visited + (x -> y), acc + 1)
      case _ => acc

  loop(Queue((x, y)), Set.empty, 0)

@main
def part2(): Unit =
  val hmap = heightMap()
  val ps =
    for
      x <- (0 until hmap.length)
      y <- (0 until hmap(0).length) if checkPoint(hmap)(x, y)
    yield (x, y)

  val result = ps.map(bfs(hmap).tupled).sorted.reverse.take(3).product
  println(result)
