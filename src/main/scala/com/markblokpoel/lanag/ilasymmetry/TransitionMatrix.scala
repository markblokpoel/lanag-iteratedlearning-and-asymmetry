package com.markblokpoel.lanag.ilasymmetry

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.core.{ContentSignal, ReferentialIntention}
import com.markblokpoel.lanag.math.Distribution
import com.markblokpoel.lanag.rsa.Lexicon
import com.markblokpoel.lanag.util.RNG

case class TransitionMatrix(vocabularySize: Int,
                            contextSize: Int,
                            order: Int,
                            k: Int,
                            sampleSize: Int,
                            errorRate: Double) {
  require(order >= 0, s"order must be greater than or equal to 0, but is $order.")
  assert(vocabularySize > 0, s"vocabularySize must be greater than 0, but is $vocabularySize.")
  assert(contextSize > 0, s"contextSize must be greater than 0 but is $contextSize.")
  assert(k > 0, s"k must be greater than 0, but is $k")
  assert(sampleSize > 0, s"sampleSize must be greater than 0 but is $sampleSize.")
  assert(errorRate >= 0 && errorRate <= 1.0, s"errorRate must be between 0.0 and 1.0 (inclusive), but is $errorRate.")

  type Datum = Seq[Seq[(ContentSignal, ReferentialIntention)]]

  val length: Int = math.pow(2, (vocabularySize * contextSize).toDouble).toInt

  private val lexiconIndices = (0 until length).toStream

  val allPossibleLexicons: Vector[Lexicon] = lexiconIndices.map(idxToLexicon).toVector

  val allPossibleAgents: Vector[RSA1ShotAgent] = allPossibleLexicons.map(new RSA1ShotAgent(_, order))

  val datum: Datum = for(_ <- 0 until sampleSize) yield generateObservations(vocabularySize, contextSize, k)

  val q: Stream[Vector[Double]] = for(speakerIdx <- lexiconIndices) yield {
    (for(learnerAgent <- allPossibleAgents) yield {
      val speaker = new RSA1ShotAgent(allPossibleLexicons(speakerIdx), order)
      val learner = learnerAgent
      datum.map(prob(_, speaker, learner)).sum
    })
  }

  val zeroDistribution: Distribution[RSA1ShotAgent] =
    Distribution(allPossibleAgents, Vector.tabulate(length)(_ => BigDecimal(0)))

  def apply(speaker: RSA1ShotAgent, learner: RSA1ShotAgent): Double =
    q(lexiconToIdx(speaker.originalLexicon))(lexiconToIdx(learner.originalLexicon))

  def getLearnerDistribution(speaker: RSA1ShotAgent): Distribution[RSA1ShotAgent] = {
    val d = q(lexiconToIdx(speaker.originalLexicon)).map(BigDecimal(_))
    Distribution[RSA1ShotAgent](allPossibleAgents, d)
  }

  private def idxToLexicon(idx: Int): Lexicon = {
    val str = idx.toBinaryString
    val bits = (0 until vocabularySize * contextSize - str.length).map(_ => "0").foldLeft("")(_ + _) + str
    Lexicon(vocabularySize, contextSize, bits.map(b => Integer.parseInt(b.toString).toDouble).toVector)
  }

  def lexiconToIdx(lexicon: Lexicon): Int = {
    require(lexicon.data.forall(v => v==0.0 || v==1.0), s"lexicon is graded, cannot convert to binary string.")
    Integer.parseInt(lexicon.data.map(_.intValue).mkString(""), 2)
  }

  private def generateObservations(vocabularySize: Int,
                                   contextSize: Int,
                                   k: Int): Seq[(ContentSignal, ReferentialIntention)] =
    for (_ <- 0 until k)
      yield
        (ContentSignal(RNG.nextInt(vocabularySize)),
          ReferentialIntention(RNG.nextInt(contextSize)))

  private def mutate(signal: ContentSignal, vocabularySize: Int): ContentSignal = {
    if (signal.isDefined) {
      val signalId = signal.content.get
      val randomSignalId = (signalId + 1 + RNG.nextInt(vocabularySize - 1)) % vocabularySize
      ContentSignal(randomSignalId)
    } else {
      ContentSignal(None)
    }
  }

  private def prob(observations: Seq[(ContentSignal, ReferentialIntention)],
                   speaker: RSA1ShotAgent, learner: RSA1ShotAgent): Double =
    observations.map(obs => {
      val (signal, intention) =
        if (RNG.nextProbability < errorRate) obs
        else (mutate(obs._1, vocabularySize), obs._2)
      val speakerP = speaker.asSpeaker.inferredLexicon(signal.content.get, intention.content.get)
      val learnerP = learner.asSpeaker.inferredLexicon(signal.content.get, intention.content.get)
      speakerP * learnerP
    }).foldLeft(1.0)(_ * _)
}
