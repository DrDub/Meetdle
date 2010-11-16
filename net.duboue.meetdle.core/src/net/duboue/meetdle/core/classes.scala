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
 * Classes for the Meetdle application.
 * 
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */

package net.duboue.meetdle.core

case class Participant(alias: String)

// e.g., yes, no, maybe
case class SelectionClass(selections: List[String]) {
  def equals(o: SelectionClass) = selections.equals(o.selections)
}

// e.g., yes, from [yes, no, maybe]
case class Selection(selected: String, from: SelectionClass) {
  val logger = grizzled.slf4j.Logger("net.duboue.meetdle")
  if (!from.selections.contains(selected))
    logger.debug("Impossible selection '" + selected + "', possible selections: " + from.selections)
}

// e.g., day, time
case class OptionClass(dimensions: List[String], selectInto: SelectionClass) {
  def equals(o: OptionClass) = dimensions.equals(o.dimensions) && selectInto.equals(o.selectInto)
}

// e.g., [day, time, [yes, no, maybe]], ["mon", "3pm"]
case class Option(dimensionsFrom: OptionClass, dimensionValues: List[String]) {
  def equals(o: Option) = dimensionsFrom.equals(o.dimensionsFrom) && dimensionValues.equals(o.dimensionValues)
}

// poll is immutable, each time something changes, a new poll is created with the 
// same ID and the old one is discarded

// the OptionClasses come from options[].dimensionsFrom
// the SelectionClasses come from options[].dimensionsFrom.selectionInto
case class Poll(id: Int, title: String, description: String,
  options: List[Option], participants: List[Participant],
  selected: List[Tuple3[Participant, Option, Selection]],
  datePosted: Long,
  dateModified: Long,
  revision: Int)
