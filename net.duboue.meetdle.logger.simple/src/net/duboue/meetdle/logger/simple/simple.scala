/*
 *   This file is part of Meetdle.
 *   Meetdle is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU Affero General Public License as 
 *   published by the Free Software Foundation, either version 3 of 
 *   the License, or (at your option) any later version.
 *
 *   Meetdle is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Affero General Public License for more details.
 *   
 *   You should have received a copy of the GNU Affero General Public 
 *   License along with Meetdle.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * A logger that uses a file in disk to store all transactions. Only makes sense for a personal deployment of a few polls.
 *  
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */
package net.duboue.meetdle.logger.simple

import net.duboue.meetdle.core._
import scala.collection.mutable.{ Map, HashMap, Buffer, ArrayBuffer }
import java.io.{ File, PrintWriter, FileWriter, IOException }

case class SimpleLogger(folder: File) extends TransactionLogger {

  def pollFile(poll: Int) =
    if (poll < 100)
      new File(folder, poll + ".log")
    else
      new File(new File(folder, (poll % 100).toString), poll + ".log")

  def replay(poll: Int): Iterable[Transaction] = {
    val f = pollFile(poll)
    if (!f.exists)
      return Nil
    else
      return scala.io.Source.fromFile(f).getLines.map(_.replaceAll("\n", "")).map(Transaction(_)).toIterable
  }

  def log(poll: Int, tr: Transaction) = {
    try {
      val f = pollFile(poll)
      if (!f.getParentFile.exists)
        f.getParentFile.mkdirs
      val pw = new PrintWriter(new FileWriter(f, true))
      pw.println(tr.toString)
      pw.close
    } catch {
      case e: IOException => logger.error(e);
    }
  }

  def contains(poll: Int) = pollFile(poll).exists

  def allPolls: Iterable[Int] = {
    val r: Buffer[Int] = ArrayBuffer()
    def process(root: File): Unit = {
      for (f <- folder.listFiles) {
        if (f.getName.endsWith(".log"))
          r += f.getName.replaceAll(".log", "").toInt
        else if (f.isDirectory)
          process(f)
      }
    }
    process(folder)
    return r
  }
}
