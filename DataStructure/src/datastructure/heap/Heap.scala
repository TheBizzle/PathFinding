package datastructure.heap

import annotation.tailrec

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

    def insert(elem: T) {
        insertAtEnd(elem)
        heapUp(size - 1)
    }

    private def insertAtEnd(elem: T) {
        val arrSize = size
        if (arrSize >= heapArr.size) increaseArrSize()
        heapArr(arrSize) = Some(elem)
    }

    private def increaseArrSize() {
        val newArr = new Array[Option[T]](heapArr.size * Heap.LoadDiminishFactor)
        initializeArr(newArr, None)
        heapArr = arrTransfer(newArr, heapArr)
    }

    private def arrTransfer(newArr: Array[Option[T]], originalArr: Array[Option[T]]) : Array[Option[T]] = {
        @tailrec def arrTransferHelper(newArr: Array[Option[T]], originalArr: Array[Option[T]], originalSize: Int, counter: Int) : Array[Option[T]] = {
            if (counter < (originalSize - 1)) {
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
        val parentIndex = parentIndexOf(elemIndex)
        if ((elemIndex != 0) && isBetter(elemIndex, parentIndex)) { swap(elemIndex, parentIndex); heapUp(parentIndex) }
    }

    private def parentIndexOf(index: Int) : Int = {
        ((index-1)/2).floor.toInt
    }

    private def swap(startIndex: Int,  endIndex: Int) {
        val temp = heapArr(endIndex)
        heapArr(endIndex) = heapArr(startIndex)
        heapArr(startIndex) = temp
    }

    def remove() : Option[T] = {
        val retVal = heapArr(0)
        if (retVal == None) throw new NoSuchElementException
        val myLast = size - 1
        heapArr(0) = heapArr(myLast)
        heapArr(myLast) = None
        heapDown(0)
        retVal
    }

    @tailrec
    private def heapDown(elemIndex: Int) {
        val myLast = size - 1
        if (elemIndex < myLast) {
            val childIndex = findBestChildIndex(elemIndex)
            if (isBetter(childIndex, elemIndex)) { swap(elemIndex, childIndex); heapDown(childIndex) }
        }
    }

    private def findBestChildIndex(parentIndex: Int) : Int = {
        val first = (2 * parentIndex) + 1
        val second = first + 1
        if (isBetter(first, second)) first else second
    }

    private def isBetter(first: Int, second: Int) : Boolean = {
        val myLast = size - 1
        if ((first > myLast) || (second > myLast) || (heapArr(first) == None)) false
        else if (heapArr(second) == None) true
        else orderProp(heapArr(first).get, heapArr(second).get) > 0
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
        val tempArr = heapArr.foldRight(List[T]())( (x, acc) => if (x != None) x.get :: acc else acc )
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

//    // For debugging right now (write correctly later)
//    override def toString : String = {
//
//        var outStr = ""
//        var counter = 0
//        var elem = heapArr(counter)
//
//        while (!elem.isEmpty) {
//            outStr += elem.asInstanceOf[PriorityCoordinate].priority + ","
//            counter += 1
//            elem = heapArr(counter)
//        }
//
//        "[" + outStr + "]"
//
//    }
//
//    // For debugging (not actually all that awesome)
//    def toAwesomeString : String = {
//        "[" + orderedListMaker.foldLeft("")((acc, x) => x.priority + "," + acc) + "]"
//    }
//
//    // For debugging
//    private def orderedListMaker : List[PriorityCoordinate] = {
//        var outList = List[PriorityCoordinate]()
//        val clonedHeap = clone()
//        while (!clonedHeap.isEmpty) {
//            outList = clonedHeap.remove().asInstanceOf[PriorityCoordinate] :: outList
//        }
//        outList.reverse
//    }

    override def clone() : Heap[T] = {
        new Heap[T](ordering, heapArr.clone())
    }

}

object Heap {
    protected[datastructure] val BaseArrSize = 10
    private val LoadDiminishFactor = 2
}