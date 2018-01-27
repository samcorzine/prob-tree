object Main extends App {
  case class InvalidKeyException(private val message: String = "",
                            private val cause: Throwable = None.orNull)
                      extends Exception(message, cause)

  abstract class BinaryTree {
    def isEmpty: Boolean
    def size: Int
    def put(newkey: Float, newvalue: String): BinaryTree
  }

  case object EmptyTree extends BinaryTree {
    def isEmpty = true
    def isValid = true
    val size = 0
    def put(newkey: Float, newvalue: String): NonEmptyTree = {
      return new NonEmptyTree(size=1, left=EmptyTree, right=EmptyTree, key=newkey, value=newvalue)
    }
    def get(searchkey: Float): = throw new InvalidKeyException()
  }

  case class NonEmptyTree(val size: Int, val left: BinaryTree, val right: BinaryTree, val key: Float, val value: String) extends BinaryTree {
    def isEmpty = false
    def put(newkey: Float, newvalue: String): NonEmptyTree = {
      if (newkey == key) return NonEmptyTree(size=size, left=left, right=right, key=key, value=newvalue)
      val correct_side = if(newkey < key) "left" else "right"
      val total_subnodes = left.size + right.size
      val heavy_side = if(left.size * 0.666 > total_subnodes)  "left" else if (right.size * 0.666 > total_subnodes) "right" else "neither"
      val heavy_correct_side = correct_side == heavy_side
      val side_to_put = if(heavy_side != correct_side) correct_side else correct_side match{
        case "left" => "right"
        case "right" => "left"
      }
      val new_subtree = left.put(newkey, newvalue)
      return side_to_put match {
        case "left" => new NonEmptyTree(size=size + 1, left=left.put(newkey, newvalue), right=right, key=key, value=value)
        case "right" => new NonEmptyTree(size=size + 1, left=left, right=right.put(newkey, newvalue), key=key, value=value)
      }
    }
    def get(searchkey: Float): String = {
      if (searchkey == key) return value
      val correct_side = if(newkey < key) "left" else "right"
      return correct_side match {
        case "left" => if left.get(searchkey)
        }
      }
    }
  }
}
