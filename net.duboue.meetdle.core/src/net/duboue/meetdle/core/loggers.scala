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
 * Different transaction loggers for Meetdle. More interesting ones will be defined in separate projects.
 * 
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */

package net.duboue.meetdle.core

import scala.collection.mutable.{ Map, HashMap, Buffer, ArrayBuffer }

/**
 * The logger signature type.
 */
abstract class TransactionLogger {
	
  val logger = grizzled.slf4j.Logger("net.duboue.meetdle")
	
  def allPolls: Iterable[Int]

  def replay(poll: Int): Iterable[Transaction]

  def log(poll: Int, tr: Transaction)

  def contains(poll: Int): Boolean
}

/**
 * A logger that keeps everything in memory.
 */
object MemoryLogger extends TransactionLogger {
  private val tsPerPoll: Map[Int, Buffer[Transaction]] = new HashMap[Int, Buffer[Transaction]]()

  def replay(poll: Int): Iterable[Transaction] =
    (if (tsPerPoll.contains(poll)) tsPerPoll(poll) else Nil)

  def log(poll: Int, tr: Transaction) = {
    if (!tsPerPoll.contains(poll))
      tsPerPoll += poll -> new ArrayBuffer[Transaction]()
    tsPerPoll(poll) += tr
  }

  def contains(poll: Int) = tsPerPoll.contains(poll)

  def allPolls = tsPerPoll.keySet.toList
}
