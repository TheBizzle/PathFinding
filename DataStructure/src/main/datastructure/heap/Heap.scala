package datastructure.heap

import annotation.tailrec
import java.lang.IllegalStateException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/11/11
 * Time: 3:50 PM
 */
class Heap[T : Manifest] protected[datastructure] (ordering: (T, T) => Int, elemArr: Array[Option[T]]) {

    val orderProp = ordering
    var heapArr = elemArr     // Need to fix this var-age somehow (maybe) (nope)

    def this(ordering: (T, T) => Int) {
        this(ordering, new Array[Option[T]](Heap.BaseArrSize))
        initializeArr()
    }

    def clear() {
        heapArr = new Array[Option[T]](heapArr.size)
        initializeArr()
    }

    def insert(elem: T) {
        insertAtEnd(elem)
        heapUp(size - 1)
    }

    private def insertAtEnd(elem: T) {
        val arrSize = size
        if (arrSize == heapArr.size) increaseArrSize()
        heapArr(arrSize) = Some(elem)
    }

    private def increaseArrSize() {
        val newArr = new Array[Option[T]](heapArr.size * Heap.LoadDiminishFactor)
        initializeArr(newArr, None)
        heapArr = arrTransfer(newArr, heapArr)
    }

    private def arrTransfer(newArr: Array[Option[T]], originalArr: Array[Option[T]]) : Array[Option[T]] = {
        @tailrec def arrTransferHelper(newArr: Array[Option[T]], originalArr: Array[Option[T]], originalSize: Int, counter: Int) : Array[Option[T]] = {
            if (counter < originalSize) {
                newArr(counter) = originalArr(counter)
                arrTransferHelper(newArr, originalArr, originalSize, counter + 1)
            }
            else
                newArr
        }
        arrTransferHelper(newArr, originalArr, originalArr.size, 0)
    }

    @tailrec
    private def heapUp(elemIndex: Int) {
        if (elemIndex != 0) {
            val parentIndex = parentIndexOf(elemIndex)
            if (isBetterNode(elemIndex, parentIndex)) { swap(elemIndex, parentIndex); heapUp(parentIndex) }
        }
    }

    private def parentIndexOf(index: Int) : Int = {
        ((index-1)/2).floor.toInt
    }

    private def swap(startIndex: Int,  endIndex: Int) {
        val temp = heapArr(endIndex)
        heapArr(endIndex) = heapArr(startIndex)
        heapArr(startIndex) = temp
    }

    def remove() : T = {
        val retVal = heapArr(0).get
        val lastIndex = size - 1
        heapArr(0) = heapArr(lastIndex)
        heapArr(lastIndex) = None
        heapDown(0)
        retVal
    }

    @tailrec
    private def heapDown(elemIndex: Int) {
        if (!isLeaf(elemIndex)) {
            val childIndex = findBestChildIndex(elemIndex)
            if (isBetterNode(childIndex, elemIndex)) { swap(childIndex, elemIndex); heapDown(childIndex) }
        }
    }

    private def isLeaf(index: Int) : Boolean = {
        ((size == 0) || (depthOf(index) == depthOf(size - 1)) || (firstChildIndexOf(index) > (size - 1)))
    }

    private def depthOf(index: Int) : Int = {
        log2(index + 1).floor.toInt
    }

    private def log2(num: Int) : Double = {
        import scala.math.log
        log(num) / log(2)
    }

    private def findBestChildIndex(parentIndex: Int) : Int = {

        val firstIndex = firstChildIndexOf(parentIndex)
        val secondIndex = firstIndex + 1

        if (heapArr(firstIndex) == None) throw new IllegalStateException("What did you do to my heap?!")
        else if ((heapArr(secondIndex) == None) || isBetterChild(firstIndex, secondIndex)) firstIndex
        else secondIndex
        
    }

    private def firstChildIndexOf(parentIndex: Int) : Int = {
        (2 * parentIndex) + 1
    }

    private def isBetterChild(firstIndex: Int, secondIndex: Int) : Boolean = {
        compareNodes(firstIndex, secondIndex) >= 0
    }

    private def isBetterNode(firstIndex: Int, secondIndex: Int) : Boolean = {
        compareNodes(firstIndex, secondIndex) > 0
    }

    private def compareNodes(firstIndex: Int, secondIndex: Int) : Int = {
        orderProp(heapArr(firstIndex).get, heapArr(secondIndex).get)
    }

    def peek : Option[T] = {
        heapArr(0)
    }

    // Sadly, it's preferable to just run this every time we want the size, rather than
    // juggling 'size' vals on the Heap reconstruction that occurs after each array resize
    def size : Int = {
        @tailrec def sizeHelper(arr: Array[Option[T]], currentSize: Int) : Int = {
            if ((arr.size > currentSize) && (!arr(currentSize).isEmpty))
                sizeHelper(arr, currentSize + 1)
            else
                currentSize
        }
        sizeHelper(heapArr, 0)
    }

    def isEmpty : Boolean = {
        size == 0
    }

    def foreach[U](f: (T => U)) {
        val tempArr = heapArr.foldRight(List[T]()){ case (x, acc) => if (x != None) x.get :: acc else acc }
        tempArr.foreach(f)
    }

    protected def initializeArr() {
        initializeArr(heapArr, None)
    }

    private def initializeArr(arr: Array[Option[T]], initVal: Option[T]) {
        for (i <- 0 until arr.size) {
            arr(i) = initVal
        }
    }

    def toList : List[T] = {
        heapArr.filter(_ != None).map(_.get).toList
    }

    override def clone() : Heap[T] = {
        new Heap[T](ordering, heapArr.clone())
    }

}

object Heap {
    protected[datastructure] val BaseArrSize = 10
    private val LoadDiminishFactor = 2
}
