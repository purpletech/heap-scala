package coursera.algorithms.homework.week6
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.StringBuilder;
object Heap {
  def heapify[T](a: ArrayBuffer[T], f: (T, T) => Boolean): Unit = {
    val count = a.length;
    val start: Int = (count - 2) / 2; //Starting from last parent
    for (i <- start until -1 by -1) siftDown(a, i, count - 1)(f);
  }
  def heapSort[T](a: ArrayBuffer[T], f: (T, T) => Boolean): Unit = {
    heapify(a, f);
    (a.length-1 to (0, -1)).foreach( i => {
      swap(a, 0, i)
      siftDown(a, 0, i)(f);
    })

  }
  def createHeap[T](a: ArrayBuffer[T], f: (T, T) => Boolean): Heap[T] = {
    val heap = new Heap[T](f);
    val count = a.length;
    heapify(a, f);
    heap.nodes = a;
    heap.count = count;
    heap;
  }
  private def siftDown[T](a: ArrayBuffer[T], pidx: Int, max: Int)(f: (T, T) => Boolean): Unit = { //Sift down
    var pt = pidx; //Track node needs to be swapped with node at pidx
    val lc = 2 * pidx + 1; //Left child
    if (lc <= max && f(a(pt), a(lc))) pt = lc;
    val rc = lc + 1; //Right child
    if (rc <= max && f(a(pt), a(rc))) pt = rc;
    if (pt != pidx) {
      swap(a, pt, pidx);
      siftDown(a, pt, max)(f); // Sift down nodes below pt
    }
  }
  private def swap[T](a: ArrayBuffer[T], i: Int, j: Int): Unit = {
    var tmp: T = a(i);
    a(i) = a(j);
    a(j) = tmp;
  }
}
import Heap._
class Heap[T](f: (T, T) => Boolean) {
  private var nodes: ArrayBuffer[T] = ArrayBuffer[T]();
  private var count: Int = 0;
  def size = { count; }

  def insert(e: T): Unit = {
    if (count < nodes.length) {
      nodes(count) = e;
    } else {
      nodes += e;
    }
    count += 1;
    bubbleUp(nodes, count - 1); //Maintain heap
  }
  def extract(): T = {
    val x = nodes(0);
    swap(nodes, 0, count - 1);
    count -= 1; //reduce heap size
    siftDown(nodes, 0, count - 1)(f)
    x;
  }
  def peek(): Option[T] = {
    if (count > 0) Option(nodes(0));
    else None;
  }
  def delete(e: T): Unit = {
    var found: Boolean = false;
    var i: Int = 0;
    while ((!found) && i < count) {
      if (nodes(i).equals(e)) {
        found = true;
      }
      i += 1;
    }
    if (found) {
      val index = i - 1;
      val x = nodes(index);
      swap(nodes, index, count - 1);
      count -= 1;
      siftDown(nodes, 0, count - 1)(f);
    }

  }
  private def bubbleUp(a: ArrayBuffer[T], idx: Int): Unit = {
    val parent: Int = (idx - 1) / 2
    if (parent >= 0) {
      if (f(a(parent), a(idx))) {
        swap(a, parent, idx);
        bubbleUp(a, parent);
      }
    }
  }

  def printMe(): Unit = {
    var ls = (math.log(count) / math.log(2)).floor.toInt; //depth of tree
    for (i <- 0 to ls) {
      var s = new StringBuilder("");
      val lmin = math.pow(2, i).toInt - 1;
      var lmax = math.pow(2, i + 1).toInt - 2;
      if (lmax >= count - 1) lmax = count - 1;
      for (j <- lmin to lmax) {
        s.append(nodes(j).toString + "\t");
      }
      s.append("\n");
      print(s.toString);
    }
  }
}