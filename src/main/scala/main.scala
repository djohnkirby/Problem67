import triangle.*

import scala.::
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

val path = List(Direction.Left, Direction.Right, Direction.Right)

val bogusPath = path ++ path
val bogusPath2 = List(Direction.Right)

def parseBottomLine(line: String): List[Node] = {
  val tokens = line.split(" ") //Snooch to the nooch
  tokens.map(x => Node(value = x.toInt, left = None, right = None)).toList
}

def parseNonBottomLine(line: String, lowerLine: List[Node]): List[Node] = {
  val tokens = line.split(" ") //Snoochie noochie little boochies
  tokens.zipWithIndex.map(
    (x, index) => {
      Node(x.toInt, left = Some(lowerLine(index)), right = Some(lowerLine(index +1)))
    }
  ).toList
}

def parseTriangle(filename: String): Option[Node] = {
  val source = Source.fromFile(filename)
  val layers = source.getLines().toSeq.reverse
  val bottomLine = parseBottomLine(layers.head)
  val foo = layers.tail.foldLeft(bottomLine)((lowerLine, line) => {
    parseNonBottomLine(line, lowerLine)
  })
  source.close()
  foo.headOption
}

def parseTriangleCooler(filename: String): List[List[Int]] = {
  val source = Source.fromFile(filename)
  val layers = source.getLines().toSeq.reverse
  layers.map(line => {
    line.split(" ").map(_.toInt).toList
  }).toList
}

def findPathMaximumCooler(invertedTriangle: List[List[Int]]): Int = {
  if invertedTriangle.isEmpty then
    0
  else if (!invertedTriangle.tail.isEmpty) {
    val bottomRow = invertedTriangle.head
    if invertedTriangle.tail.isEmpty then bottomRow.head else
      val penultimateRow = invertedTriangle.tail.head
      val newPenultimateRow = penultimateRow.zipWithIndex.map((node, index) => {
        Math.max(node + bottomRow(index), node + bottomRow(index + 1))
      })
      findPathMaximumCooler(newPenultimateRow :: invertedTriangle.tail.tail)
  } else {
    invertedTriangle.head.head
  }
}

def generateAllPossiblePathsForRowCount(n: Int): List[List[Direction]] =
 generateAllPossiblePathsForRowCount(n, List(List(Direction.Right), List(Direction.Left)))

@tailrec
def generateAllPossiblePathsForRowCount(n: Int, pathsForNMinusOne: List[List[Direction]]): List[List[Direction]] = {
  n match
    case 0 => List.empty
    case 1 => List.empty
    case 2 => pathsForNMinusOne
    case _ => generateAllPossiblePathsForRowCount(n-1, appendAllPossiblePaths(pathsForNMinusOne))
}

def appendAllPossiblePaths(paths: List[List[Direction]]) = paths.map(path =>  Direction.Right :: path) ++ paths.map(path => Direction.Left :: path)


def findRowCount(root: Option[Node]): Int = {
  root.fold(0)( node =>
    1 + findRowCount(node.left)
  )
}

def findMaxPath(root: Option[Node]): Int = {
  val rowCount = findRowCount(root)
  val possiblePaths = generateAllPossiblePathsForRowCount(rowCount)
  possiblePaths.map(path => sumPath(root, path)).flatMap(_.toOption).max
}

def printLayerAndRecurse(layer: List[Node]): Unit = {
  println(layer.map(_.value.toString).mkString(" "))
  layer.head.left.map( left => printLayerAndRecurse(left :: layer.map(_.right.get)))
}

def printTriangle(root: Option[Node]): Unit = {
  root.map(node => printLayerAndRecurse(List(node)))
  ()
}



@main
def main(): Unit = {
  val triangle = parseTriangleCooler("0067_triangle.txt")
  println(findPathMaximumCooler(triangle))
}