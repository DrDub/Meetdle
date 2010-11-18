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

  // number of sub folders. Note: all IDs should have at least this number of characters.
  private val LEVELS = 2

  def pollFile(poll: String) =
    if (poll.length() < LEVELS)
      new File(folder, poll + ".log")
    else
      poll.substring(0, LEVELS).foldLeft(folder)((x, c) => new File(x, "" + c))

  def memoFile(poll: String) =
    new File(pollFile(poll).getName.replaceAll(".log", ".memo"))

  def replay(poll: String): Iterable[Transaction] = {
    val f = pollFile(cleanPollStr(poll))
    try {
      if (f.exists)
        return scala.io.Source.fromFile(f).getLines.map(_.replaceAll("\n", "")).map(Transaction(_)).toIterable
    } catch {
      case e => logger.error(e);
    }
    return Nil;
  }

  def log(poll: String, tr: Transaction) = {
    try {
      val f = pollFile(cleanPollStr(poll))
      if (!f.getParentFile.exists)
        f.getParentFile.mkdirs
      val pw = new PrintWriter(new FileWriter(f, true))
      pw.println(tr.toString)
      pw.close
    } catch {
      case e => logger.error(e);
    }
  }

  def contains(poll: String) = pollFile(cleanPollStr(poll)).exists

  def allPolls: Iterable[String] = {
    val r: Buffer[String] = ArrayBuffer()
    def process(root: File): Unit = {
      for (f <- folder.listFiles) {
        if (f.getName.endsWith(".log"))
          r += f.getName.replaceAll(".log", "")
        else if (f.isDirectory)
          process(f)
      }
    }
    process(folder)
    return r
  }

  def memo(poll: String): String = {
    val f = memoFile(cleanPollStr(poll))
    try {
      if (f.exists)
        return scala.io.Source.fromFile(f).foldLeft("")((s, c) => s + c)
    } catch {
      case e => logger.error(e);
    }
    return null
  }

  def setMemo(poll: String, memo: String) = {
    val f = memoFile(cleanPollStr(poll))
    try {
      if (!f.getParentFile.exists)
        f.getParentFile.mkdirs
      val pw = new PrintWriter(new FileWriter(f));
      pw.print(memo);
      pw.close
    } catch {
      case e => logger.error(e);
    }
  }
}
