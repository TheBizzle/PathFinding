package datastructure.priorityqueue

import datastructure.heap.Heap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 11/11/11
 * Time: 3:48 PM
 */

class PriorityQueue[T : Manifest] protected (ordering: (T, T) => Int, elemArr: Array[Option[T]]) extends Heap[T](ordering, elemArr) {

    // Sloppy.  Is there a better way to do this?
    def this(ordering: (T, T) => Int) {
        this(ordering, new Array[Option[T]](Heap.BaseArrSize))
        initializeArr()
    }

    def enqueue(elem: T) {
        insert(elem)
    }

    def dequeue() : Option[T] = {
        remove()
    }

    override def clone() : PriorityQueue[T] = {
        new PriorityQueue[T](orderProp, heapArr.clone())
    }

}
