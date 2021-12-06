package day02

import scala.io.Source

enum Command:
  case Forward(n: Int)
  case Down(n: Int)
  case Up(n: Int)

import Command.*

def stream(): LazyList[Command] =
  Source
    .fromResource("day02.txt")
    .getLines()
    .map(parseCommand)
    .to(LazyList)

def parseCommand(line: String): Command =
  line.split(" ") match
    case Array("forward", n) => Forward(n.toInt)
    case Array("down", n)    => Down(n.toInt)
    case Array("up", n)      => Up(n.toInt)

case class State1(horizontal: Int, depth: Int)

def interpret1(state: State1, command: Command): State1 =
  val State1(horizontal, depth) = state
  command match
    case Forward(n) => State1(horizontal + n, depth)
    case Down(n)    => State1(horizontal, depth + n)
    case Up(n)      => State1(horizontal, depth - n)

case class State2(horizontal: Int, depth: Int, aim: Int)

def interpret2(state: State2, command: Command): State2 =
  val State2(horizontal, depth, aim) = state
  command match
    case Forward(n) => State2(horizontal + n, depth + aim * n, aim)
    case Down(n)    => State2(horizontal, depth, aim + n)
    case Up(n)      => State2(horizontal, depth, aim - n)

@main
def part1(): Unit =
  val State1(horizontal, depth) = stream().foldLeft(State1(0, 0))(interpret1)
  println(horizontal * depth)

@main
def part2(): Unit =
  val State2(horizontal, depth, aim) =
    stream().foldLeft(State2(0, 0, 0))(interpret2)
  println(horizontal * depth)
