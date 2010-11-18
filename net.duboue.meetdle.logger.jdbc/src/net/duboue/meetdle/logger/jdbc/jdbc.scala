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
 * A JDBC transaction logger.
 * 
 * The DB has to have a table Transactions with fields POLL VARCHAR(20) and TRSTR VARCHAR(2048) and
 * a table Memos with field POLL VARCHAR(20) and MEMO VARCHAR(2048)
 *   
 *  Licensed under AGPLv3+.
 *  
 *  Copyright (C) 2010 Pablo Duboue.
 *  pablo.duboue@gmail.com
 */
package net.duboue.meetdle.logger.jdbc

import net.duboue.meetdle.core._
import scala.collection.mutable.{ Set, HashSet, Buffer, ArrayBuffer }
import java.sql.{ Connection, DriverManager, ResultSet }
import java.net.{ URLEncoder, URLDecoder }

case class JdbcLogger(driver: String, connStr: String) extends TransactionLogger {

  // Load the driver
  // driver, e.g. "com.mysql.jdbc.Driver"
  Class.forName(driver).newInstance

  // establish the connection
  // connStr, e.g. "jdbc:mysql://localhost:3306/dbname?user=luser&password=s3cr3t"
  def conn = DriverManager.getConnection(connStr)

  private def encode(s: String) = URLEncoder.encode(s, "UTF-8")
  private def decode(s: String) = URLDecoder.decode(s, "UTF-8")

  def replay(poll: String): Iterable[Transaction] = {
    val p = cleanPollStr(poll)
    val r: Buffer[Transaction] = ArrayBuffer()
    val conn = this.conn
    try {
      val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
      val rs = statement.executeQuery("SELECT TrStr FROM Transactions WHERE Poll = '" + p + "'")
      while (rs.next)
        r += Transaction(decode(rs.getString(0)))
    } finally {
      conn.close
    }
    return r
  }

  def log(poll: String, tr: Transaction) = {
    val p = cleanPollStr(poll)
    val conn = this.conn
    try {
      conn.createStatement.executeUpdate("INSERT INTO TrStr VALUES('" + p + "','" + encode(tr.toString) + "')")
    } finally {
      conn.close
    }
  }

  def contains(poll: String): Boolean = {
    val p = cleanPollStr(poll)
    val conn = this.conn
    try {
      return conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY).
        executeQuery("SELECT Poll FROM Transactions WHERE Poll = '" + p + "'").next
    } finally {
      conn.close
    }
  }

  def allPolls: Iterable[String] = {
    val r: Set[String] = HashSet()
    val conn = this.conn
    try {
      val rs = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY).
        executeQuery("SELECT Poll FROM Transactions")
      while (rs next)
        r += rs.getString(0)
    } finally {
      conn.close
    }
    return r
  }

  def memo(poll: String): String = {
    val r: Buffer[Transaction] = ArrayBuffer()
    val p = cleanPollStr(poll)
    val conn = this.conn
    try {
      val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
      val rs = statement.executeQuery("SELECT Memo FROM Memos WHERE Poll = '" + p + "'")
      if (rs.next)
        return decode(rs.getString(0))
      return null;
    } finally {
      conn.close
    }
  }

  def setMemo(poll: String, memo: String) = {
    val p = cleanPollStr(poll)
    val conn = this.conn
    try {
      conn.createStatement.executeUpdate("DELETE FROM Memos WHERE Poll = '" + p + "')")
      conn.createStatement.executeUpdate("INSERT INTO Memos VALUES(" + p + ",'" + encode(memo) + "')")
    } finally {
      conn.close
    }
  }
}