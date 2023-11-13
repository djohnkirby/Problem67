import scala.annotation.tailrec

case object triangle {

  case class Node(value: Int, left: Option[Node], right: Option[Node])

  enum Direction:
    case Left, Right

  enum PathDoesNotExistError:
    case PathDoesNotReachBaseOfTriangle
    case PathExtendsBeyondBaseOfTriangle

  def sumPath(root: Option[Node], path: List[Direction]): Either[PathDoesNotExistError, Int] = {
    (root, path) match
      case (None, x::xs) => Left(PathDoesNotExistError.PathExtendsBeyondBaseOfTriangle)
      case _ => root.fold(Right(0))(node => sumPath(node, path))
  }

  def sumPath(node: Node, path: List[Direction]): Either[PathDoesNotExistError, Int] = {
    path match
      case x :: xs =>
        x match
          case Direction.Left => sumPath(node.left, xs).map(total => total + node.value)
          case Direction.Right => sumPath(node.right, xs).map(total => total + node.value)
      case _ => {
        (node.left, node.right) match
          case (None, None) => Right(node.value)
          case (_, _) => Left(PathDoesNotExistError.PathDoesNotReachBaseOfTriangle)
      }
  }

  def printPath(root: Option[Node], path: List[Direction]): Either[PathDoesNotExistError, Unit] = {
    (root, path) match
      case (None, x :: xs) => Left(PathDoesNotExistError.PathExtendsBeyondBaseOfTriangle)
      case _ => root.fold(Right(()))(node => printPath(node, path))
  }

  def printPath(node: Node, path: List[Direction]): Either[PathDoesNotExistError, Unit] = {
    println(node.value)
    path match
      case x :: xs =>
        x match
          case Direction.Left => printPath(node.left, xs)
          case Direction.Right => printPath(node.right, xs)
      case _ => {
        (node.left, node.right) match
          case (None, None) => Right(())
          case (_, _) => Left(PathDoesNotExistError.PathDoesNotReachBaseOfTriangle)
      }
  }
}