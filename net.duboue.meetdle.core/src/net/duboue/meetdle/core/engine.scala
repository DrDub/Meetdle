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

/**
 * This engine relies heavily in the action log. It will replay all actions to fetch the current state of a poll.
 */
class Engine(logger: TransactionLogger) {

  def getLogger = logger

  def contains(poll: Int) = logger.contains(poll)

  def apply(poll: Int): Poll = {
    return logger.replay(poll).foldLeft(emptyPoll(poll))((p, t) => t.execute(p))
  }

  private def emptyPoll(poll: Int): Poll = {
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

}