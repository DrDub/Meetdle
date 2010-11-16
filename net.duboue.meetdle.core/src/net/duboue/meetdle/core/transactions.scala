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
 * Transactions for the Meetdle application.
 * 
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */

package net.duboue.meetdle.core

object TransactionCounter {
  private var counter: Int = 0;

  def inc: Int = {
    val ret = counter;
    counter = counter + 1;
    return ret;
  }
}

class MalformedTransactionException(msg: String) extends java.lang.Exception(msg)

// these transactions might profit from a fleshed out framework, using discovery 
// (e.g., http://bmc.github.com/classutil/) but it is only FOUR transactions, 
// so I won't sweat it. Adding new fields to an existing object it is a lot of 
// work, though
abstract sealed class Transaction {
  val logger = grizzled.slf4j.Logger("net.duboue.meetdle")

  val transactionId = TransactionCounter.inc

  val poll: Int

  def toTokens: List[String]

  @throws(classOf[net.duboue.meetdle.core.MalformedTransactionException])
  def execute(p: Poll): Poll

  override def toString: String = {
    return toTokens.foldLeft("")((x, y) => x + "\t" + y).substring(1)
  }
}

object Transaction {
  def apply(str: String): Transaction = apply(str.split("\\t").toList)

  def apply(tokens: List[String]): Transaction = {
    tokens match {
      case "CreatePoll" :: poll :: title :: description :: Nil =>
        return TrCreatePoll(poll.toInt, title, description)

      case "AddParticipant" :: poll :: alias :: Nil =>
        return TrAddParticipant(poll.toInt, alias)

      case "ModifySelection" :: poll :: alias :: index :: selection :: Nil =>
        return TrModifySelection(poll.toInt, alias, index.toInt, selection)

      case "ModifyOption" :: poll :: index :: dimensions :: dimensionValues :: sel :: Nil => {
        def processList(s: String): List[String] = s.split(" ").toList.map((s) => s.replaceAll("&#32;", " "))
        return TrModifyOption(poll.toInt, index.toInt,
          processList(dimensions), processList(dimensionValues), processList(sel))
      }
      case _ =>
        throw new MalformedTransactionException("Can't parse: " + tokens)
    }
  }
}

case class TrCreatePoll(poll: Int, title: String, description: String) extends Transaction {
  def toTokens = "CreatePoll" :: poll.toString :: title :: description :: Nil

  def execute(p: Poll): Poll = {
    val now = java.lang.System.currentTimeMillis()
    return Poll(poll, title, description, p.options, p.participants, p.selected, p.datePosted, now, transactionId)
  }
}

// add or modify, modify if index already present, if not present, it gets added at the end
// UI sorts by different (known) dimensions
case class TrModifyOption(poll: Int, index: Int,
  dimensions: List[String], dimensionValues: List[String], selectInto: List[String]) extends Transaction {

  private def listToString(l: List[String]) = l.map((s) => s.replaceAll(" ", "&#32;")).reduceLeft(_ + " " + _)
  def toTokens = "ModifyOption" :: poll.toString :: index.toString ::
    listToString(dimensions) :: listToString(dimensionValues) :: listToString(selectInto) :: Nil

  def execute(p: Poll): Poll = {
    val now = java.lang.System.currentTimeMillis()
    if (p.revision == -1)
      throw new MalformedTransactionException("Unknown poll " + poll)

    val entry = Option(OptionClass(dimensions, SelectionClass(selectInto)), dimensionValues)
    def newOptions: List[Option] = {
      if (p.options.length > index) {
        val (l1, l2) = p.options.splitAt(index)
        return l1 ::: List(entry) ::: l2.tail
      } else
        return p.options ::: List(entry)
    }
    def newSelections: List[Tuple3[Participant, Option, Selection]] = {
      if (p.options.length > index) {
        val oldOption = p.options(index)
        var fixed: List[Tuple3[Participant, Option, Selection]] = Nil;

        for (t <- p.selected) {
          if (t._2.equals(oldOption)) {
            if (t._3.from.equals(entry.dimensionsFrom.selectInto))
              fixed = fixed ::: List(Tuple3(t._1, entry, Selection(t._3.selected, entry.dimensionsFrom.selectInto)))
            else
              logger.info("Change in option at index " + index + " dropped non-compatible selection by participant " + t._1.alias);
          } else
            fixed = fixed ::: List(t)
        }
        return fixed;
      } else
        return p.selected;
    }

    return Poll(poll, p.title, p.description, newOptions,
      p.participants, newSelections, p.datePosted, now, transactionId)
  }
}

case class TrAddParticipant(poll: Int, alias: String) extends Transaction {
  def toTokens = "AddParticipant" :: poll.toString :: alias :: Nil

  def execute(p: Poll): Poll = {
    val now = java.lang.System.currentTimeMillis()
    if (p.revision == -1)
      throw new MalformedTransactionException("Unknown poll " + poll)

    if (!p.participants.find((x) => x.alias.equals(alias)).isEmpty)
      throw new MalformedTransactionException("A participant with that alias already exists")

    return Poll(poll, p.title, p.description, p.options,
      p.participants ::: List(Participant(alias)), p.selected, p.datePosted, now, transactionId)
  }
}

case class TrModifySelection(poll: Int, alias: String, index: Int, selection: String) extends Transaction {
  def toTokens = "ModifySelection" :: poll.toString :: alias :: index.toString :: selection :: Nil

  def execute(p: Poll): Poll = {
    val now = java.lang.System.currentTimeMillis()
    if (p.revision == -1)
      throw new MalformedTransactionException("Unknown poll " + poll)

    p.participants.find((x) => x.alias.equals(alias)) match {
      case None => throw new MalformedTransactionException("Unknown participant")
      case Some(participant) =>
        if (index > p.options.length)
          throw new MalformedTransactionException("Unknown option")
        val option = p.options(index)
        if (!option.dimensionsFrom.selectInto.selections.contains(selection))
          throw new MalformedTransactionException("Unknown selection")
        val newEntry = List((participant, option, Selection(selection, option.dimensionsFrom.selectInto)))

        val (l0, l1) = p.selected.partition((x) => x._1 == participant && x._2.equals(option))
        val l2 = if (l1.isEmpty) l1 else l1.tail;
        return Poll(poll, p.title, p.description, p.options,
          p.participants, l0 ::: newEntry ::: l2, p.datePosted, now, transactionId)
    }

  }
}
