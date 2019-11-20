package com.markblokpoel.lanag

case class Generation(population: Population, generation: Int) {
  def meanAmbiguity: BigDecimal =
    (for (ag <- population.agents)
      yield ag.originalLexicon.meanAmbiguity()).sum / population.size
}
