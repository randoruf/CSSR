package com.typeclassified.hmm.cssr.shared

import breeze.linalg.DenseVector
import com.typeclassified.hmm.cssr.parse.{Alphabet, AlphabetHolder}

trait Probablistic {
  protected val size: Int = AlphabetHolder.alphabet.map.size
  var frequency: DenseVector[Double] = DenseVector.zeros(size)
  var distribution: DenseVector[Double] = DenseVector.zeros(size)
  var totalCounts: Double = 0
}
