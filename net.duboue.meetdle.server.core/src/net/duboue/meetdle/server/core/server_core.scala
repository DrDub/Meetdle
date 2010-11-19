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
 * A base filter to be reused by other servers implementing some views.
 *  
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */
package net.duboue.meetdle.server.core

import scala.collection._
import scala.util.parsing.json.Parser
import scala.xml.NodeSeq

import org.scalatra.ScalatraFilter
import net.duboue.meetdle.core._

abstract class MeetdleFilter extends ScalatraFilter {

  //TODO this should be move to implicit
  val logger: TransactionLogger;

  val engine: Engine; // engine = new Engine(logger); but need implict to work

  val lock = new Object

  val rnd = new java.util.Random

  def pollSubmitSuccess(poll: String, pollAdmin: String): NodeSeq;

  def pollSubmitError(reason: String): NodeSeq;

  def prefSubmitError(reason: String): NodeSeq;

  def showPoll(poll: Poll): NodeSeq;

  get("/:poll/") {
    val poll = params.get("poll").getOrElse("")
    if (engine.contains(poll))
      showPoll(engine(poll))
    else
      halt(401, "Poll not found.")
  }

  get("/submit") {
    // to be able to use return
    def doIt(): NodeSeq = {
      // validate parameters
      val title: String = params.get("title").getOrElse("")
      val description = params.get("description").getOrElse("")
      val numberOfOptions = params.get("numberOfOptions").getOrElse("0").toInt
      if (numberOfOptions <= 0)
        return pollSubmitError("No options specified")
      var options: List[Tuple3[List[String], List[String], List[String]]] = Nil
      for (i <- List.range(0, numberOfOptions)) {
        val numberOfDimensions = params.get("numberOfDimensions" + i).getOrElse("0").toInt
        if (numberOfDimensions > 0) {
          val dimensions = List.range(0, numberOfDimensions).map(x => params.get(new String("dim" + i + "_" + x))).flatten
          val values = List.range(0, numberOfDimensions).
            map(x => params.get(new String("val" + i + "_" + x))).flatten.
            map(_.trim).filter(_.length > 0)
          if (dimensions.length == values.length) {
            // else, ignore option
            val numberOfSelections = params.get("numberOfSelections" + i).getOrElse("0").toInt
            if (numberOfSelections > 0 && !dimensions.isEmpty) {
              val selections = List.range(0, numberOfSelections).map(x => params.get(new String("sel" + i + "_" + x))).flatten
              if (!selections.isEmpty && !values.isEmpty)
                options = options ::: List(Tuple3(dimensions, values, selections))
            }
          }
        }
      }
      if (options.isEmpty)
        return pollSubmitError("No options specified")
      // create poll
      val poll = engine.newPoll("" + rnd.nextInt, title, description)
      try {
        for (o <- options)
          engine.execute(TrModifyOption(poll, Int.MaxValue, o._1, o._2, o._3))
      } catch {
        case e => return pollSubmitError(e.toString)
      }
      // create management access
      val pollAdmin = engine.hash("" + rnd.nextInt + poll).substring(0, poll.length)
      engine.getLogger.setMemo(poll, pollAdmin)

      // signal success
      pollSubmitSuccess(poll, pollAdmin)
    };
    doIt()
  }

  get("/:poll/:admin/") {
    val poll = params.get("poll").getOrElse("")
    if (!engine.contains(poll))
      halt(401, "Poll not found.")
    else {
      val p = engine(poll)
      val admin = params.get("admin").getOrElse("")
      if (!admin.equals(engine.getLogger.memo(poll)))
        halt(401, "Wrong administration link.")
      else
        <html><body>
                <h1>More info coming soon.</h1>
                {
                  for (tr <- engine.getLogger.replay(poll))
                    yield <pre>{ tr.toString }</pre>
                }
              </body></html>
    }
  }

  get("/:poll/submit") {
    // to be able to use return
    def doIt(): NodeSeq = {
      val poll = params.get("poll").getOrElse("")
      if (!engine.contains(poll))
        return prefSubmitError("Unknown poll '" + poll + "'")
      val participant = params.get("participant").getOrElse("")
      if (!engine.contains(poll))
        return prefSubmitError("Must specify a participant")
      try {
        val fullPoll = engine(poll)
        if (!fullPoll.participants.map(_.alias).contains(participant))
          engine.execute(TrAddParticipant(poll, participant))
        for (i <- List.range(0, fullPoll.options.length)) {
          params.get("sel" + i) match {
            case Some(value) => engine.execute(TrModifySelection(poll, participant, i, value))
            case None => /* ignore */
          }
        }
        showPoll(engine(poll))
      } catch {
        case e => return prefSubmitError(e.toString)
      }
    }
    doIt()
  }

  // modify options
  get("/:poll/:admin/submit") {
    // to be able to use return
    def doIt(): NodeSeq = {
      val poll = params.get("poll").getOrElse("")
      if (!engine.contains(poll))
        return prefSubmitError("Unknown poll '" + poll + "'")
      if (engine.getLogger.memo(poll) != params.get("admin").getOrElse(""))
        return prefSubmitError("Unknown admin subfolder")

      val optionNumber = params.get("option").getOrElse("-1").toInt
      if (optionNumber < 0)
        return prefSubmitError("Must specify an option")
      try {
        val fullPoll = engine(poll)

        val numberOfDimensions = params.get("numberOfDimensions").getOrElse("0").toInt
        if (numberOfDimensions <= 0)
          return prefSubmitError("Invalid number of dimensions")

        val dimensions = List.range(0, numberOfDimensions).map(x => params.get(new String("dim" + x))).flatten
        val values = List.range(0, numberOfDimensions).map(x => params.get(new String("val" + x))).flatten
        if (dimensions.length != values.length)
          return pollSubmitError("Dimensions and values mismatch, dimensions " + dimensions.length + " values " + values.length)
        val numberOfSelections = params.get("numberOfSelections").getOrElse("0").toInt
        val selections = List.range(0, numberOfSelections).map(x => params.get(new String("sel" + x))).flatten

        engine.execute(TrModifyOption(poll, optionNumber, dimensions, values, selections))

        return showPoll(engine(poll))
      } catch {
        case e => return prefSubmitError(e.toString)
      }
    }

    doIt()
  }

}
