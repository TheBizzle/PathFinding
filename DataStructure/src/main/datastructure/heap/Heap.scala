package datastructure.heap

import annotation.tailrec
import java.lang.IllegalStateException
import Heap._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/11/11
 * Time: 3:50 PM
 */
class Heap[T : Manifest] protected[datastructure] (ordering: (T, T) => Int, elemArr: Array[Option[T]]) {

  val orderProp = ordering
  var heapArr   = elemArr // Need to fix this var-age somehow (maybe) (nope)

  def this(ordering: (T, T) => Int) {
    this(ordering, Array.fill[Option[T]](BaseArrSize)(Default))
  }

  def clear() {
    heapArr = Array.fill[Option[T]](heapArr.size)(Default)
  }

  def insert(elem: T) {
    insertAtEnd(elem)
    heapUp(size - 1)
  }

  def remove() : T = {
    val retVal    = heapArr(0).get
    val lastIndex = size - 1
    heapArr(0) = heapArr(lastIndex)
    heapArr(lastIndex) = Default
    heapDown(0)
    retVal
  }

  def peek = heapArr(0)

  def size    = elems.size
  def isEmpty = elems.isEmpty
  def toSeq   = elems.toSeq
  def toList  = elems.toList

  override def clone() = new Heap[T](ordering, heapArr.clone())

  def exists(p: T => Boolean) = elems exists p

  def foreach[U](f: (T => U)) {
    elems foreach f
  }

  private def elems = heapArr.flatten

  private def insertAtEnd(elem: T) {
    val arrSize = size
    if (arrSize == heapArr.size) increaseArrSize()
    heapArr(arrSize) = Option(elem)
  }

  private def increaseArrSize() {
    val newArr = Array.fill[Option[T]](heapArr.size * LoadDiminishFactor)(Default)
    heapArr = arrTransfer(newArr, heapArr)
  }

  private def arrTransfer(newArr: Array[Option[T]], originalArr: Array[Option[T]]) : Array[Option[T]] = {
    0 until originalArr.size foreach (i => newArr(i) = originalArr(i))
    newArr
  }

  @tailrec
  private def heapUp(elemIndex: Int) {
    if (elemIndex != 0) {
      val parentIndex = parentIndexOf(elemIndex)
      if (isBetterNode(elemIndex, parentIndex)) {
        swap(elemIndex, parentIndex)
        heapUp(parentIndex)
      }
    }
  }

  private def parentIndexOf(index: Int) = ((index - 1) / 2).floor.toInt

  private def swap(startIndex: Int,  endIndex: Int) {
    val temp = heapArr(endIndex)
    heapArr(endIndex) = heapArr(startIndex)
    heapArr(startIndex) = temp
  }

  @tailrec
  private def heapDown(elemIndex: Int) {
    if (!isLeaf(elemIndex)) {
      val childIndex = findBestChildIndex(elemIndex)
      if (isBetterNode(childIndex, elemIndex)) {
        swap(childIndex, elemIndex)
        heapDown(childIndex)
      }
    }
  }

  private def isLeaf(index: Int)  = (size == 0) || (depthOf(index) == depthOf(size - 1)) || (firstChildIndexOf(index) > (size - 1))
  private def depthOf(index: Int) = log2(index + 1).floor.toInt

  private def log2(num: Int) : Double = {
    import scala.math.log
    log(num) / log(2)
  }

  private def findBestChildIndex(parentIndex: Int) : Int = {

    val firstIndex  = firstChildIndexOf(parentIndex)
    val secondIndex = firstIndex + 1

    if (heapArr(firstIndex).isEmpty)
      throw new IllegalStateException("What did you do to my heap?!")
    else if ((heapArr(secondIndex).isEmpty) || isBetterChild(firstIndex, secondIndex))
      firstIndex
    else
      secondIndex

  }

  private def firstChildIndexOf(parentIndex: Int)              = (2 * parentIndex) + 1
  private def isBetterChild(firstIndex: Int, secondIndex: Int) = compareNodes(firstIndex, secondIndex) >= 0
  private def isBetterNode(firstIndex: Int, secondIndex: Int)  = compareNodes(firstIndex, secondIndex) > 0
  private def compareNodes(firstIndex: Int, secondIndex: Int)  = orderProp(heapArr(firstIndex).get, heapArr(secondIndex).get)

}

object Heap {
  protected[datastructure] val BaseArrSize = 10
  protected[datastructure] val Default = None
  private val LoadDiminishFactor = 2
}
