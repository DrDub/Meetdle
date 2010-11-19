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
 * Engine for Meetdle.
 * 
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */

package net.duboue.meetdle.core

import java.security.MessageDigest
/**
 * This engine relies heavily in the action log. It will replay all actions to fetch the current state of a poll.
 */
class Engine(logger: TransactionLogger) {

  val lock = new Object
  val md5 = MessageDigest.getInstance("MD5")

  def getLogger = logger

  def contains(poll: String) = logger.contains(poll)

  def apply(poll: String): Poll = {
    return logger.replay(poll).foldLeft(emptyPoll(poll))((p, t) => t.execute(p))
  }

  private def emptyPoll(poll: String): Poll = {
    val now = java.lang.System.currentTimeMillis
    return Poll(poll, "", "", Nil, Nil, Nil, now, now, -1)
  }

  @throws(classOf[net.duboue.meetdle.core.MalformedTransactionException])
  def execute(tr: Transaction) {
    val poll = if (contains(tr.poll)) this(tr.poll) else emptyPoll(tr.poll)
    tr.execute(poll)
    // success? log
    logger.log(tr.poll, tr)
  }

  def newPoll(base: String, title: String, description: String): String = {
    var poll = base;

    lock.synchronized {
      var c = 0;
      do {
        c += 1;
        if (c > 10) // to avoid looping forever
          poll = poll + " "
        poll = hash(poll).substring(0, poll.length)
      } while (logger.contains(poll));
      logger.log(poll, TrCreatePoll(poll, title, description))
    }

    return poll;
  }

  def hash(s: String): String = {
    lock.synchronized {
      md5.reset()
      md5.update(s.getBytes)
      return md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft("")(_ + _)
    }
  }

}