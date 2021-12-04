import DataStructure.BinaryTree
import Traits.Action
import TypeBuilders.IntegerBuilder

import java.util
import java.util.Vector
import scala.io.StdIn

object Main extends App {
  def timeTesting(binaryTree: BinaryTree[Int], size: Int) : Unit = {
    println("===============================================================")

    var lastTime = System.currentTimeMillis
    while (binaryTree.size != size) {
      binaryTree.add(typeBuilder.create())
    }
    println(s"Tardo un momento en agregar  ${binaryTree.size} nodos: " +
      s"${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    println(s"subnodo arbol izquierdo: ${binaryTree.leftSubtreeDepth} | " +
      s"subnodo derecho: ${binaryTree.rightSubtreeDepth}")

    lastTime = System.currentTimeMillis
    binaryTree.balance()
    println(s"\nsegundos segundo: ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    println(s"nodo izquierdo profundidad: ${binaryTree.leftSubtreeDepth} | " +
      s"nodo derecho profundidad: ${binaryTree.rightSubtreeDepth}")


    lastTime = System.currentTimeMillis
    binaryTree.forEach(new Action[binaryTree.Node] {
      override def doWith(someObject: binaryTree.Node): Unit = {
        someObject.getValue
      }
    })
    println(s"\nsegundos  : ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    lastTime = System.currentTimeMillis
    for (i <- 0 until binaryTree.size) {
      binaryTree.findByIndex(i)
    }
    println(s"\nMe tomó unos segundos encontrar todos los nodos: ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    while (binaryTree.size != 0) {
      binaryTree.deleteByIndex(0)
    }
    println(s"\nSe necesitaron unos segundos para eliminar todos los nodos: ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    println("===============================================================\n")
  }

  def printTree(binaryTree: BinaryTree[Int]) : Unit = {
    binaryTree.forEach(println)
  }

  var typeBuilder = new IntegerBuilder()
  var binaryTree = new BinaryTree[Int](typeBuilder.getComparator())

  timeTesting(binaryTree, 1000)
  timeTesting(binaryTree, 5000)
  timeTesting(binaryTree, 10000)
}
