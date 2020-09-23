/*
 * Copyright (C) 2018 Samuel Thiriot
 *                    Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.method.microlcs

import org.openmole.core.context.Variable
import org.openmole.tool.random.RandomProvider

trait Condition[T] {
  def attributeName: String
  def matches(v: T): Boolean
  def accepts(v: Variable[T]): Boolean = (v.prototype.simpleName == attributeName) && matches(v.value)
  def acceptsUnsafe(v: Variable[_]) = v match {
    case vcasted: Variable[T] ⇒ accepts(vcasted)
    case _                    ⇒ throw new IllegalArgumentException("expecting another type for " + this)
  }
  def subsums(other: Condition[T]): Boolean
  def subsumsUnsafe(other: Condition[_]): Boolean = other match {
    case otherCasted: Condition[T] ⇒ subsums(otherCasted)
    case _                         ⇒ throw new IllegalArgumentException("Can only compare similar conditions")
  }
  def generalityIndice: Int

}

abstract class ConditionOneValue[T] extends Condition[T] {
  def refValue: T
}

abstract class ConditionInSet[T](val matchingValues: Set[T]) extends Condition[T] {
  //val matchingValues: Set[T]

  def isWildCard: Boolean = matchingValues.isEmpty

  override def toString(): String = attributeName + {

    if (matchingValues.isEmpty) {
      " = #"
    }
    else if (matchingValues.size == 1) {
      " = " + matchingValues.head
    }
    else {
      "id in {" + matchingValues.map(_.toString).mkString(",") + "}"
    }
  }

}

/**
 * The condition macthing the Id of an entity.
 * It matches a set of id. If the set is empty, then everything is matched.
 * If the set contains values, only the entities having the ids of the set are matched.
 */
/*
class ConditionId(matchingValues: Set[Int]) extends ConditionInSet[Int](matchingValues) {

  def this(id: Int) = this(Set(id))
  def this() = this(Set[Int]())

  override def attributeName: String = "id"

  override def matches(v: Int): Boolean = matchingValues.isEmpty || matchingValues.contains(v)

  override def subsums(other: Condition[Int]): Boolean = other match {
    case oo: ConditionId ⇒ matchingValues.isEmpty || (!oo.matchingValues.isEmpty && oo.matchingValues.subsetOf(matchingValues))
    case _               ⇒ false // TODO warn ???
  }

  // TODO is that relevant ???
  override def generalityIndice = if (matchingValues.isEmpty) 2 else if (matchingValues.size == 1) 0 else 1

}
*/

abstract class WildCard[T] extends Condition[T] {
  override def matches(v: T): Boolean = true
  override def toString(): String = { attributeName + "==#" }
  override def subsums(other: Condition[T]) = true
  override def generalityIndice = 2
}

abstract class LowerThanNumCondition[T: Numeric] extends ConditionOneValue[T] {
  override def matches(v: T): Boolean = { implicitly[Numeric[T]].toDouble(v) <= implicitly[Numeric[T]].toDouble(refValue) }
  override def toString(): String = { attributeName + "<=" + refValue }
  override def subsums(other: Condition[T]) = other match {
    case lt: LowerThanNumCondition[T]  ⇒ (implicitly[Numeric[T]].toDouble(refValue) >= implicitly[Numeric[T]].toDouble(lt.refValue))
    case _: GreaterThanNumCondition[T] ⇒ false
    case eq: EqualToCondition[T]       ⇒ (implicitly[Numeric[T]].toDouble(refValue) >= implicitly[Numeric[T]].toDouble(eq.refValue))
    case _: WildCard[T]                ⇒ false
    case _                             ⇒ false // TODO warn ???
  }
  override def generalityIndice = 1
}

abstract class GreaterThanNumCondition[T: Numeric] extends ConditionOneValue[T] {
  override def matches(v: T): Boolean = { implicitly[Numeric[T]].toDouble(v) >= implicitly[Numeric[T]].toDouble(refValue) }
  override def toString(): String = { attributeName + ">=" + refValue }
  override def subsums(other: Condition[T]) = other match {
    case lt: GreaterThanNumCondition[T] ⇒ implicitly[Numeric[T]].toDouble(refValue) <= implicitly[Numeric[T]].toDouble(lt.refValue)
    case _: LowerThanNumCondition[T]    ⇒ false
    case eq: EqualToCondition[T]        ⇒ (implicitly[Numeric[T]].toDouble(refValue) <= implicitly[Numeric[T]].toDouble(eq.refValue))
    case _: WildCard[T]                 ⇒ false
    case _                              ⇒ false // TODO warn ???
  }
  override def generalityIndice = 1
}

abstract class EqualToCondition[T] extends ConditionOneValue[T] {
  override def matches(v: T): Boolean = (v == refValue)
  override def toString(): String = { attributeName + "==" + refValue }
  override def subsums(other: Condition[T]) = other match {
    case _: GreaterThanNumCondition[T] ⇒ false
    case _: LowerThanNumCondition[T]   ⇒ false
    case eq: EqualToCondition[T]       ⇒ refValue == eq.refValue
    case _: WildCard[T]                ⇒ false
    case _                             ⇒ false // TODO warn ???
  }
  override def generalityIndice = 0
}

case class WildCardIntCondition(attributeName: String) extends WildCard[Int]
case class LowerThanIntCondition(attributeName: String, refValue: Int) extends LowerThanNumCondition[Int]
case class GreaterThanIntCondition(attributeName: String, refValue: Int) extends GreaterThanNumCondition[Int]
case class EqualToIntCondition(attributeName: String, refValue: Int) extends EqualToCondition[Int]

case class WildCardFloatCondition(attributeName: String) extends WildCard[Double]
case class LowerThanFloatCondition(attributeName: String, refValue: Double) extends LowerThanNumCondition[Double]
case class GreaterThanFloatCondition(attributeName: String, refValue: Double) extends GreaterThanNumCondition[Double]
case class EqualToFloatCondition(attributeName: String, refValue: Double) extends EqualToCondition[Double]

case class WildCardBoolCondition(attributeName: String) extends WildCard[Boolean]
case class EqualToBoolCondition(attributeName: String, refValue: Boolean) extends EqualToCondition[Boolean]

object Condition {

  /**
   * Factory to create Conditions for various variable types
   */
  def apply[T](v: Variable[T], rng: scala.util.Random): Condition[_] = v.value match {
    case value: Int     ⇒ createIntegerCondition(v.asInstanceOf[Variable[Int]], rng)
    case value: Double  ⇒ createFloatCondition(v.asInstanceOf[Variable[Double]], rng)
    case value: Boolean ⇒ createBoolCondition(v.asInstanceOf[Variable[Boolean]], rng)
    // TODO ???
    case _              ⇒ throw new IllegalArgumentException("Sorry, unable to create a condition for value " + v.value)
  }

  // TODO receive maxid and initialize rules with the capture of many entities ?
  /*
   * Creates a random condition on id matching a given individual; will either
   * generate a wildcard on id, or a condition matching exactly this entity
   */
  /*
  def createIdCondition(id: Int, rng: scala.util.Random): ConditionId = {
    if (rng.nextDouble() < 0.5) {
      new ConditionId()
    }
    else {
      new ConditionId(id)
    }
  }
  */

  def createIntegerCondition(v: Variable[Int], rng: scala.util.Random): Condition[Int] = {
    val r: Int = rng.nextInt(100)
    if (r <= 25) { LowerThanIntCondition(v.prototype.simpleName, v.value + 1) }
    else if (r <= 50) { GreaterThanIntCondition(v.prototype.simpleName, v.value - 1) }
    else if (r <= 60) { EqualToIntCondition(v.prototype.simpleName, v.value) }
    else { WildCardIntCondition(v.prototype.simpleName) }
  }

  def createFloatCondition(v: Variable[Double], rng: scala.util.Random): Condition[Double] = {
    val r: Int = rng.nextInt(100)
    if (r <= 25) { LowerThanFloatCondition(v.prototype.simpleName, v.value + 1) }
    else if (r <= 50) { GreaterThanFloatCondition(v.prototype.simpleName, v.value - 1) }
    else if (r <= 60) { EqualToFloatCondition(v.prototype.simpleName, v.value) }
    else { WildCardFloatCondition(v.prototype.simpleName) }
  }

  def createBoolCondition(v: Variable[Boolean], rng: scala.util.Random): Condition[Boolean] = {
    val r: Int = rng.nextInt(100)
    if (r <= 60) { EqualToBoolCondition(v.prototype.simpleName, v.value) }
    else { WildCardBoolCondition(v.prototype.simpleName) }
  }

  // TODO create and mutate String

  def integersBefore(maxId: Int, rng: scala.util.Random): Set[Int] = {
    val count = rng.nextInt(maxId)
    rng.shuffle((0 until maxId).toList).take(count).toSet
  }

  /*
  def mutateId(c: ConditionId, maxId: Int, rng: scala.util.Random): ConditionId = {
    if (c.matchingValues.isEmpty) {
      // there was no value; let's match a random one !
      new ConditionId(rng.nextInt(maxId))
      //new ConditionId(integersBefore(maxId, rng))
    }
    else {
      val rd = rng.nextDouble()
      // there were ids
      // we might change of id
      if (rd <= 0.8) {
        // we might remove one id over 2
        new ConditionId(rng.nextInt(maxId))
      }
      else {
        // we might switch to wildcard ?
        new ConditionId()
      }
      /*
      if (rd <= 0.3) {
        // we might remove one id over 2
        new ConditionId(c.matchingValues.filter(_ ⇒ rng.nextDouble() <= 0.5))
      }
      else if (rd <= 0.6) {
        // we might add some ids ?
        new ConditionId(c.matchingValues ++ integersBefore(maxId, rng))
      }
      else {
        // we might switch to wildcard ?
        new ConditionId()
      }*/
    }
  }
  */

  def mutateInt(c: Condition[Int], min: Int, max: Int, rng: scala.util.Random): Condition[Int] = {

    val refVal = c match {
      case ov: ConditionOneValue[Int] ⇒ ov.refValue
      case _                          ⇒ min + 1 + rng.nextInt(max - min - 2) // TODO what is a good value ???
    }
    val refName = c.attributeName

    val r: Int = rng.nextInt(100)
    if (r <= 25) {
      LowerThanIntCondition(refName, refVal)
    }
    else if (r <= 50) {
      GreaterThanIntCondition(refName, refVal)
    }
    else if (r <= 60) {
      EqualToIntCondition(refName, refVal)
    }
    else {
      WildCardIntCondition(refName)
    }

  }

  def mutateDouble(c: Condition[Double], min: Double, max: Double, rng: scala.util.Random): Condition[Double] = {

    val refVal = c match {
      case ov: ConditionOneValue[Double] ⇒ ov.refValue
      case _                             ⇒ rng.nextDouble() * (max - min) + min
    }
    val refName = c.attributeName

    val r: Int = rng.nextInt(100)
    if (r <= 25) {
      LowerThanFloatCondition(refName, refVal)
    }
    else if (r <= 50) {
      GreaterThanFloatCondition(refName, refVal)
    }
    else if (r <= 60) {
      EqualToFloatCondition(refName, refVal)
    }
    else {
      WildCardFloatCondition(refName)
    }

  }

  def mutateBoolean(c: Condition[Boolean], rng: scala.util.Random): Condition[Boolean] = c match {

    case w: WildCardBoolCondition ⇒ EqualToBoolCondition(c.attributeName, rng.nextBoolean())
    case eq: EqualToBoolCondition ⇒
      if (rng.nextBoolean()) {
        EqualToBoolCondition(c.attributeName, !eq.refValue)
      }
      else {
        WildCardBoolCondition(c.attributeName)
      }
  }

  def mutate(c: Condition[_], min: Double, max: Double)(implicit rng: RandomProvider): Condition[_] = c match {
    case LowerThanIntCondition(_, _) | GreaterThanIntCondition(_, _) | EqualToIntCondition(_, _) | WildCardIntCondition(_) ⇒ mutateInt(c.asInstanceOf[Condition[Int]], min.toInt, max.toInt, rng())
    case LowerThanFloatCondition(_, _) | GreaterThanFloatCondition(_, _) | EqualToFloatCondition(_, _) | WildCardFloatCondition(_) ⇒ mutateDouble(c.asInstanceOf[Condition[Double]], min, max, rng())
    case EqualToBoolCondition(_, _) | WildCardBoolCondition(_) ⇒ mutateBoolean(c.asInstanceOf[Condition[Boolean]], rng())
    case _ ⇒ throw new IllegalArgumentException("oops, we are not able to mutate gene " + c)
  }

  /**
   * If possible, simplifies the given condition by returning a novel Condition of a similar type and True.
   * Else returns the same condition  and false.
   */
  def simplify(c: Condition[_], minVal: Double, maxVal: Double): (Condition[_], Boolean) = c match {

    // x <= 100 with x in [...:100] => x = #
    case LowerThanIntCondition(a, v) if v == maxVal ⇒ (WildCardIntCondition(a), true)
    case LowerThanFloatCondition(a, v) if v == maxVal ⇒ (WildCardFloatCondition(a), true)

    // x <= 2 with x in [2:...] => x == 2
    case LowerThanIntCondition(a, v) if v == minVal ⇒ (EqualToIntCondition(a, v), true)
    case LowerThanFloatCondition(a, v) if v == minVal ⇒ (EqualToFloatCondition(a, v), true)

    // x >= 2 with x in [2:...] => x = #
    case GreaterThanIntCondition(a, v) if v == minVal ⇒ (WildCardIntCondition(a), true)
    case GreaterThanFloatCondition(a, v) if v == minVal ⇒ (WildCardFloatCondition(a), true)

    // x >= 100 with x in [1:100] => x = 100
    case GreaterThanIntCondition(a, v) if v == maxVal ⇒ (EqualToIntCondition(a, v), true)
    case GreaterThanFloatCondition(a, v) if v == maxVal ⇒ (EqualToFloatCondition(a, v), true)

    // don't change anything
    case _ ⇒ (c, false)
  }

}

