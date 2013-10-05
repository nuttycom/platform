/*
 *  ____    ____    _____    ____    ___     ____ 
 * |  _ \  |  _ \  | ____|  / ___|  / _/    / ___|        Precog (R)
 * | |_) | | |_) | |  _|   | |     | |  /| | |  _         Advanced Analytics Engine for NoSQL Data
 * |  __/  |  _ <  | |___  | |___  |/ _| | | |_| |        Copyright (C) 2010 - 2013 SlamData, Inc.
 * |_|     |_| \_\ |_____|  \____|   /__/   \____|        All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the 
 * GNU Affero General Public License as published by the Free Software Foundation, either version 
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See 
 * the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this 
 * program. If not, see <http://www.gnu.org/licenses/>.
 *
 */
package com.precog.yggdrasil

sealed trait TableSize {
  def maxSize: Long
  def lessThan (other: TableSize): Boolean = maxSize < other.maxSize
  def +(other: TableSize): TableSize
  def *(other: TableSize): TableSize
}

object TableSize {
  def apply(size: Long): TableSize = ExactSize(size)
  def apply(minSize: Long, maxSize: Long): TableSize =
    if (minSize != maxSize) EstimateSize(minSize, maxSize) else ExactSize(minSize)
}

case class ExactSize(minSize: Long) extends TableSize {
  val maxSize = minSize
  
  def +(other: TableSize) = other match {
    case ExactSize(n) => ExactSize(minSize + n)
    case EstimateSize(n1, n2) => EstimateSize(minSize + n1, minSize + n2)
    case UnknownSize => UnknownSize
    case InfiniteSize => InfiniteSize
  }
  
  def *(other: TableSize) = other match {
    case ExactSize(n) => ExactSize(minSize * n)
    case EstimateSize(n1, n2) => EstimateSize(minSize * n1, minSize * n2)
    case UnknownSize => UnknownSize
    case InfiniteSize => InfiniteSize
  }
}

case class EstimateSize(minSize: Long, maxSize: Long) extends TableSize {
  def +(other: TableSize) = other match {
    case ExactSize(n) => EstimateSize(minSize + n, maxSize + n)
    case EstimateSize(n1, n2) => EstimateSize(minSize + n1, maxSize + n2)
    case UnknownSize => UnknownSize
    case InfiniteSize => InfiniteSize
  }
  
  def *(other: TableSize) = other match {
    case ExactSize(n) => EstimateSize(minSize * n, maxSize * n)
    case EstimateSize(n1, n2) => EstimateSize(minSize * n1, maxSize * n2)
    case UnknownSize => UnknownSize
    case InfiniteSize => InfiniteSize
  }
}

case object UnknownSize extends TableSize {
  val maxSize = Long.MaxValue
  def +(other: TableSize) = UnknownSize
  def *(other: TableSize) = UnknownSize
}

case object InfiniteSize extends TableSize {
  val maxSize = Long.MaxValue
  def +(other: TableSize) = InfiniteSize
  def *(other: TableSize) = InfiniteSize
}


