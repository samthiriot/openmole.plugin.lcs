/*
 * Copyright (C) 2018 Samuel Thiriot
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

import org.openmole.core.context.{ Context, Val }
import org.openmole.core.expansion.FromContext
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.domain.Bounds
import org.openmole.core.workflow.sampling._
import org.openmole.core.workspace.TmpDirectory
import org.openmole.tool.random.RandomProvider

object MicroGenes {

  sealed trait Gene[T] {

    val prototype: Val[T]

    def makeRandomValue(context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService): T
    def makeRandomValue(refValue: T, context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService): T

  }

  case class BooleanGene(val prototype: Val[Boolean]) extends Gene[Boolean] {

    override def makeRandomValue(context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService) = rng().nextBoolean()
    override def makeRandomValue(refValue: Boolean, context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService) = refValue

  }

  case class IntegerGene(val prototype: Val[Int], val min: FromContext[Int], val max: FromContext[Int]) extends Gene[Int] {

    override def makeRandomValue(context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService) = {
      val minV = min.from(context)
      val maxV = max.from(context)
      (minV + rng().nextInt(maxV - minV))
    }

    override def makeRandomValue(refValue: Int, context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService) = {
      // TODO
      val minV = min.from(context)
      val maxV = max.from(context)
      (minV + rng().nextInt(maxV - minV))
    }

  }

  case class DoubleGene(val prototype: Val[Double], val min: FromContext[Double], val max: FromContext[Double]) extends Gene[Double] {

    override def makeRandomValue(context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService) = {
      val minV = min.from(context)
      val maxV = max.from(context)
      (minV + rng().nextDouble() * (maxV - minV))
    }

    override def makeRandomValue(refValue: Double, context: Context)(implicit rng: RandomProvider, tmpDirectory: TmpDirectory, fileService: FileService) = {
      // TODO
      val minV = min.from(context)
      val maxV = max.from(context)
      (minV + rng().nextDouble() * (maxV - minV))
    }

  }

  /*
  implicit def factorIsSequenceOfEnumeration[D, T](f: Factor[D, Array[T]])(implicit fix: Fix[D, Array[T]]) =
    SequenceOfEnumeration(f.prototype, fix.apply(f.domain).toVector)
  */

  implicit def factorIsDoubleGene[D](f: Factor[D, Double])(implicit bounded: Bounds[D, Double]) =
    DoubleGene(f.value, bounded.min(f.domain), bounded.max(f.domain))

  implicit def factorIsIntegerGene[D](f: Factor[D, Int])(implicit bounded: Bounds[D, Int]) =
    IntegerGene(f.value, bounded.min(f.domain), bounded.max(f.domain))

  /* TODO sequences
  implicit def factorIsSequenceOfDouble[D](f: Factor[D, Array[Double]])(implicit bounded: Bounds[D, Array[Double]], sized: Sized[D]) =
    SequenceOfDouble(f.prototype, bounded.min(f.domain), bounded.max(f.domain), sized(f.domain))

  implicit def factorIsSequenceOfInt[D](f: Factor[D, Array[Int]])(implicit bounded: Bounds[D, Array[Int]], sized: Sized[D]) =
    SequenceOfInt(f.prototype, bounded.min(f.domain), bounded.max(f.domain), sized(f.domain))
  */

  implicit def factorIsBooleanGene[D](f: Factor[D, Boolean]) =
    BooleanGene(f.value)

  /*
  def doublesToGene(v:Val[Double], values: Array[Double]) =
    DoubleGene(v.simpleName, values.min, values.max)

  def integerToGene(v:Val[Int], values: Array[Int]) =
    IntegerGene(v.simpleName, values.min, values.max)

  def booleanToGene(v:Val[Boolean], values: Array[Boolean]) =
    BooleanGene
  */

}