package com.markblokpoel.lanag

import com.markblokpoel.lanag.ambiguityhelps.{
  RSA1ShotAgent,
  RSA1ShotInteraction
}

case class Population(agents: List[RSA1ShotAgent]) {
  def fitnessOf(agent: RSA1ShotAgent, rounds: Int): Double = {
    if (agents.contains(agent)) {
      val successRates = for (interlocutor <- agents) yield {
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

  def map[A](f: RSA1ShotAgent => A): List[A] = agents.map(f)

  def size: Int = agents.length

}
