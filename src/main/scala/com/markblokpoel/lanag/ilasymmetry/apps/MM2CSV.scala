package com.markblokpoel.lanag.ilasymmetry.apps

import java.io.{File, FileInputStream, ObjectInputStream, PrintWriter}

import com.markblokpoel.lanag.ilasymmetry.MutationMatrix

object MM2CSV extends App {

  val ois = new ObjectInputStream(new FileInputStream("data/q-v3-c3-o1.ser"))
  val mm = ois.readObject.asInstanceOf[MutationMatrix]
  ois.close()

  val pw = new PrintWriter(new File("output/q-v3-c3-o1-matrix.csv"))
  for(row <- mm.q)
    pw.println(row.mkString(";"))
  pw.close()

}
