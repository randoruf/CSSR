package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.{Probablistic, EquivalenceClass}
import org.scalatest.{FlatSpec, Matchers, BeforeAndAfter}

class LeafTests extends FlatSpec with Matchers with BeforeAndAfter {
  var tree:Tree = null

  before {
    AlphabetHolder.alphabet = Alphabet("abc".toCharArray)
    tree = Tree()
  }

  "updateDistribution" should "update distributions for observing the _next_ values in history" in {
    val leaf = Leaf("a", tree, EquivalenceClass())
    leaf.updateDistribution('b')
    assertProbabalisticDetails(leaf, 1, Array(0,1,0))

    leaf.updateDistribution('b')
    assertProbabalisticDetails(leaf, 2, Array(0,2,0))

    leaf.updateDistribution('c')
    assertProbabalisticDetails(leaf, 3, Array(0,2,1))

    leaf.updateDistribution('a')
    assertProbabalisticDetails(leaf, 4, Array(1,2,1))
  }

  def assertProbabalisticDetails(probablistic: Probablistic, total:Double, dist:Array[Double]):Unit = {
    probablistic.totalCounts          should be (total)
    probablistic.frequency.toArray    should contain theSameElementsInOrderAs dist
    probablistic.distribution.toArray should contain theSameElementsInOrderAs dist.map(_/total)
  }

  behavior of "addChild"

  it should "run updateDistribution while adding a child" in {
    val leaf = tree.root
    leaf.addChild('b')
    assertProbabalisticDetails(leaf, 1, Array(0,1,0))
    leaf.children should have size 1

    leaf.addChild('a')
    assertProbabalisticDetails(leaf, 2, Array(1,1,0))
    leaf.children should have size 2

    leaf.children.head.addChild('a')
    assertProbabalisticDetails(leaf.children.head, 1, Array(1,0,0))
    leaf.children.head.children should have size 1
  }

  it should "not introduce children with the same observed value" in {
    val leaf = tree.root
    leaf.addChild('b')
    assertProbabalisticDetails(leaf, 1, Array(0,1,0))
    leaf.children should have size 1

    leaf.addChild('b')
    assertProbabalisticDetails(leaf, 2, Array(0,2,0))
    leaf.children should have size 1

    leaf.addChild('a')
    assertProbabalisticDetails(leaf, 3, Array(1,2,0))
    leaf.children should have size 2

    leaf.addChild('a')
    assertProbabalisticDetails(leaf, 4, Array(2,2,0))
    leaf.children should have size 2
  }
}
