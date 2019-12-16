package com.markblokpoel.lanag.ilasymmetry.parallel

import com.markblokpoel.lanag.ambiguityhelps.{
  RSA1ShotAgent,
  RSA1ShotInteraction
}

import scala.util.Random

case class EvoAgent(agent: RSA1ShotAgent, others: List[RSA1ShotAgent]) {
  def fitnessOf(agent: RSA1ShotAgent,
                rounds: Int,
                interactionGroupSize: Int): Double = {
    val agentSubset = Random.shuffle(others).take(interactionGroupSize)
    val successRates = for (interlocutor <- agentSubset) yield {
      val interaction =
        RSA1ShotInteraction(agent, interlocutor, maxTurns = rounds)
      val results = interaction.runAndCollectData
      val successRate = results.interaction
        .count(d => d.success) / results.interaction.length.toDouble
      successRate
    }
    successRates.sum / successRates.length.toDouble
  }

  def asymmetries(agent: RSA1ShotAgent): List[Double] =
    others.map(other =>
      agent.originalLexicon.asymmetryWith(other.originalLexicon))
}
