package com.markblokpoel.lanag

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.core.{ContentSignal, ReferentialIntention}
import com.markblokpoel.lanag.rsa.Lexicon
import com.markblokpoel.lanag.util.RNG
import org.apache.spark.SparkContext
@SerialVersionUID(100L)
case class MutationMatrix(vocabularySize: Int,
                          contextSize: Int,
                          order: Int,
                          k: Int,
                          sampleSize: Int,
                          allPossibleAgents: List[RSA1ShotAgent],
                          q: Map[(RSA1ShotAgent, RSA1ShotAgent), Double])

case object MutationMatrix {
  type AgentPair = (RSA1ShotAgent, RSA1ShotAgent)
  type Datum = Seq[Seq[(ContentSignal, ReferentialIntention)]]

  def apply(vocabularySize: Int,
            contextSize: Int,
            order: Int,
            sparkContext: SparkContext,
            k: Int = 5,
            sampleSize: Int = 10): MutationMatrix = {
    val allPossibleAgents: List[RSA1ShotAgent] =
      allBooleanPermutations(contextSize * vocabularySize)
        .map(d =>
          new RSA1ShotAgent(Lexicon(vocabularySize, contextSize, d), order))

    val datum: Datum = for (_ <- 0 until sampleSize)
      yield generateObservations(vocabularySize, contextSize, k)

    val allSpeakerLearnerPairs: List[AgentPair] =
      for (speaker <- allPossibleAgents; learner <- allPossibleAgents)
        yield (speaker, learner)

    val pairDatum: List[(AgentPair, Datum)] =
      allSpeakerLearnerPairs.map(pair => pair -> datum)

    val q = sparkContext
      .parallelize(pairDatum)
      .map(b => {
        val (agentPair, datum) = b
        val (speaker, learner) = agentPair

        val transitionProbability = datum
          .map(d => {
            d.map(obs => {
                val (signal, intention) = obs
                val speakerP = speaker.asSpeaker.inferredLexicon(
                  signal.content.getOrElse(-1),
                  intention.content.getOrElse(-1))
                val learnerP = learner.asSpeaker.inferredLexicon(
                  signal.content.getOrElse(-1),
                  intention.content.getOrElse(-1))
                speakerP * learnerP
              })
              .foldLeft(1.0)(_ * _)
          })
          .sum
        agentPair -> transitionProbability
      })

    val qNorm1 =
      for (ag1 <- allPossibleAgents) yield {
        val row = for (ag2 <- allPossibleAgents) yield q((ag1, ag2))
        val sum = row.sum
        if (sum > 0)
          (allPossibleAgents zip row.map(_ / sum))
            .map(a => (ag1, a._1) -> a._2)
            .toMap
        else
          (allPossibleAgents zip row.map(_ => 0.0))
            .map(a => (ag1, a._1) -> a._2)
            .toMap
      }

    val qNorm2 = qNorm1.flatten.toMap

    MutationMatrix(vocabularySize,
                   contextSize,
                   order,
                   k,
                   sampleSize,
                   allPossibleAgents,
                   qNorm2)
  }

  private def allBooleanPermutations(length: Int): List[Vector[Double]] = {
    if (length == 1) List(Vector(1.0), Vector(0.0))
    else {
      val outp = for (lex <- allBooleanPermutations(length - 1))
        yield List(Vector(1.0) ++ lex, Vector(0.0) ++ lex)
      outp.flatten
    }
  }

  private def generateObservations(
      vocabularySize: Int,
      contextSize: Int,
      k: Int): Seq[(ContentSignal, ReferentialIntention)] =
    for (_ <- 0 until k)
      yield
        (ContentSignal(RNG.nextInt(vocabularySize)),
         ReferentialIntention(RNG.nextInt(contextSize)))
}
