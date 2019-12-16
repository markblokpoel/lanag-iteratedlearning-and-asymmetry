package com.markblokpoel.lanag.ilasymmetry

import com.markblokpoel.lanag.ambiguityhelps.{
  RSA1ShotAgent,
  RSA1ShotInteraction
}

import scala.util.Random

case class Population(agents: List[RSA1ShotAgent]) {
  def fitnessOf(agent: RSA1ShotAgent,
                rounds: Int,
                interactionGroupSize: Int): Double = {
    if (agents.contains(agent)) {
      val agentSubset = Random.shuffle(agents).take(interactionGroupSize)
      val successRates = for (interlocutor <- agentSubset) yield {
        val interaction =
          RSA1ShotInteraction(agent, interlocutor, maxTurns = rounds)
        val results = interaction.runAndCollectData
        val successRate = results.interaction
          .count(d => d.success) / results.interaction.length.toDouble
        successRate
      }
      successRates.sum / successRates.length.toDouble
    } else
      0.0
  }

  def asymmetries(agent: RSA1ShotAgent): List[Double] =
    for (other <- agents.filterNot(_ == agent))
      yield agent.originalLexicon.asymmetryWith(other.originalLexicon)

  def map[A](f: RSA1ShotAgent => A): List[A] = agents.map(f)

  def size: Int = agents.length

}
