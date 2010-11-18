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
		
  def cleanPollStr(poll: String) = poll.replaceAll("[^A-Za-z0-9]", "");
	
  val logger = grizzled.slf4j.Logger("net.duboue.meetdle")
	
  def allPolls: Iterable[String]

  def replay(poll: String): Iterable[Transaction]

  def log(poll: String, tr: Transaction)

  def contains(poll: String): Boolean
  
  // one memo per poll, used to store extra info as set by the interface
  // e.g., the ID of the admin URL
  def setMemo(poll: String, memo: String): Unit

  // this method result is undefined if !contains(poll)
  def memo(poll: String): String
}

/**
 * A logger that keeps everything in memory.
 */
object MemoryLogger extends TransactionLogger {
  private val tsPerPoll: Map[String, Buffer[Transaction]] = new HashMap[String, Buffer[Transaction]]()
  private val memoPerPoll: Map[String, String] = new HashMap[String,String]()

  def replay(poll: String): Iterable[Transaction] =
    (if (tsPerPoll.contains(poll)) tsPerPoll(poll) else Nil)

  def log(poll: String, tr: Transaction) = {
    if (!tsPerPoll.contains(poll))
      tsPerPoll += poll -> new ArrayBuffer[Transaction]()
    tsPerPoll(poll) += tr
  }

  def contains(poll: String) = tsPerPoll.contains(poll)

  def allPolls = tsPerPoll.keySet.toList
  
  def setMemo(poll: String, memo: String) = {
	  memoPerPoll(poll) = memo
  }
  
  def memo(poll: String) = memoPerPoll(poll)
}
