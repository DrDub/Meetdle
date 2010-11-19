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
 * Vanilla CGI servers. Given how straightforward they are, most of their HTML is included here. They should work well on Lynx.
 *  
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */
package net.duboue.meetdle.server.standalone

import scala.collection._
import scala.util.parsing.json.Parser

import org.scalatra.ScalatraFilter
import net.duboue.meetdle.core._
import net.duboue.meetdle.server.core._
import net.duboue.meetdle.logger.simple.SimpleLogger

// this class is a vanilla CGI with no interaction
class PlainMeetdleFilter extends MeetdleFilter {
  val logger = new SimpleLogger(new java.io.File("."))
  val engine = new Engine(logger);

  def pollSubmitSuccess(poll: String, pollAdmin: String) = {
    val fullPoll = "http://" + request.getServerName + ":" + request.getServerPort + "/plain/" + poll + "/"
    val fullAdmin = "http://" + request.getServerName + ":" + request.getServerPort + "/plain/" + poll + "/" + pollAdmin;

    <html><head>
            <title>Meetdle -- Libre to Decide</title>
            <link rel="stylesheet" href="style.css" type="text/css"/>
          </head><body>
                   <h1>Sucess!</h1>
                   <ul>
                     <li>Participant link:<a href={ fullPoll }>{ fullPoll }</a></li>
                     <li>Admin link:<a href={ fullAdmin }>{ fullAdmin }</a></li>
                   </ul>
                 </body></html>
  }

  def pollSubmitError(reason: String) =
    <html><head>
            <title>Meetdle -- Libre to Decide</title>
            <link rel="stylesheet" href="style.css" type="text/css"/>
          </head><body>
                   <h1>Error</h1>
                   <p>{ reason }</p>
                 </body></html>

  def prefSubmitError(reason: String) = pollSubmitError(reason)

  def showPoll(poll: Poll) = {
    val totals = new mutable.HashMap[String /*option*/ , mutable.HashMap[String, Int]]();
    def optToStr(o: MOption): String = o.dimensionsFrom.dimensions.zip(o.dimensionValues).toString
    for (o <- poll.options)
      totals += optToStr(o) -> new mutable.HashMap[String, Int]();

    <html><head>
            <title>Meetdle -- Libre to Decide</title>
            <link rel="stylesheet" href="style.css" type="text/css"/>
          </head><body>
                   <h1>{ poll.title }</h1>
                   <p>{ poll.description }</p>
                   <form action="submit">
                     <table>
                       <tr>
                         <th>Participant</th>{
                           for (o <- poll.options)
                             yield <th>{ o.dimensionsFrom.dimensions.zip(o.dimensionValues).map(t => t._1 + "=" + t._2).reduceLeft(_ + " " + _) }</th>
                         }
                       </tr>{
                         for (p <- poll.participants)
                           yield <tr>
                                   <td>{ p.alias }</td>
                                   {
                                     for (o <- poll.options)
                                       yield <td>
                                               { // see if the participant selected this option
                                                 poll.selected.find(t => t._1.equals(p) && t._2.equals(o)) match {
                                                   case Some(Tuple3(pp, oo, s)) =>
                                                     <span>{
                                                       val m = totals(optToStr(o));
                                                       m += s.selected -> (m.get(s.selected).getOrElse(0) + 1);
                                                       s.selected
                                                     }</span>
                                                   case None => <span></span>
                                                 }
                                               }
                                             </td>
                                   }
                                 </tr>
                       }
                       <tr><td colspan={ (poll.options.length + 1).toString }><hr/></td></tr>
                       <tr>
                         <td>
                           Your name:
                           <input name="participant"/>
                           <input type="submit"/>
                           <input type="hidden" name="poll" value={ poll.id }/>
                         </td>
                         {
                           for ((o, i) <- poll.options.zipWithIndex)
                             yield <td>
                                     <table>
                                       {
                                         for (s <- o.dimensionsFrom.selectInto.selections)
                                           yield <tr><td>{ s }</td><td><input type="radio" name={ "sel" + i } value={ s }></input></td></tr>
                                       }
                                     </table>
                                   </td>
                         }
                       </tr>
                       <tr><td colspan={ (poll.options.length + 1).toString }><hr/></td></tr>
                       <tr>
                         <td><b>Totals</b></td>
                         {
                           for (o <- poll.options)
                             yield <td>
                                     <table>
                                       {
                                         val oStr = optToStr(o)
                                         for (s <- o.dimensionsFrom.selectInto.selections)
                                           yield <tr><td>{ s }</td><td>{ totals(oStr).get(s).getOrElse(0) }</td></tr>
                                       }
                                     </table>
                                   </td>
                         }
                       </tr>
                     </table>
                   </form>
                   <hr/>
                   <p>
                     Meetdle is Free Software and its
                     <a href="http://github.com/DrDub/Meetdle/">source code</a>
                     is available under the terms of the Affero GNU Public License version 3 (or later).<br/>
                     Copyright (C) 2010
                     <a href="http://www.duboue.net/pablo">Pablo Ariel Duboue</a>
                   </p>
                 </body></html>

  }

}

// this server allows for a certain level of interaction