package com.markblokpoel.lanag

import java.io.{
  File,
  FileInputStream,
  FileOutputStream,
  ObjectInputStream,
  ObjectOutputStream,
  PrintWriter
}

import com.markblokpoel.lanag.util.SparkSimulation

object PrototypingMain extends App {
  val sm = SparkSimulation(local = false, cores = 0)

  val vocabularySize = 3
  val contextSize = 3
  val order = 1

  val mm = MutationMatrix(
    vocabularySize = vocabularySize,
    contextSize = contextSize,
    order = order,
    sparkContext = sm.context,
    k = 10,
    sampleSize = 250
  )

  val oos = new ObjectOutputStream(
    new FileOutputStream(
      s"output/q-v$vocabularySize-c$contextSize-o$order.obj"))
  oos.writeObject(mm)
  oos.close()

  val ois = new ObjectInputStream(
    new FileInputStream(s"output/q-v$vocabularySize-c$contextSize-o$order.obj"))
  val mmRead = ois.readObject.asInstanceOf[MutationMatrix]
  ois.close()

  val pw = new PrintWriter(
    new File(s"output/q-v$vocabularySize-c$contextSize-o$order.csv"))
  for (ag1 <- mmRead.allPossibleAgents)
    pw.println(
      (for (ag2 <- mmRead.allPossibleAgents)
        yield mmRead.q((ag1, ag2))).mkString(";"))
  pw.close()

//  for(ag1 <- mmRead.allPossibleAgents)
//    println((for(ag2 <- mmRead.allPossibleAgents) yield mmRead.q((ag1, ag2))).mkString(";"))

}
