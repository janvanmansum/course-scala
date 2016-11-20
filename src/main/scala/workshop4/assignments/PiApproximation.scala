/**
 * Copyright (C) 2016 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package workshop4.assignments

import rx.lang.scala.Observable

import scala.language.implicitConversions
import scala.util.Random

// see https://en.wikipedia.org/wiki/Approximations_of_%CF%80#Summing_a_circle.27s_area
// TODO make custom text instead of refering to Wikipedia
object PiApproximation {

  /**
   * Convert a `Boolean` to an `Int`. If `true` then 1 else 0.
   *
   * @param b the boolean to be converted
   * @return the integer conversion
   */
  def booleanToInt(b: Boolean): Int = if (b) 1 else 0

  /**
   * Groups the number of successful hits with the total number of hits
   *
   * @param successes number of successful hits
   * @param total total number of hits
   */
  case class Hits(successes: Int, total: Int)
  object Hits {
    def empty: Hits = Hits(0, 0)
  }

  def random: Observable[Double] = {
    Observable.defer(Observable.just(Random.nextDouble())).repeat
  }

  def main(args: Array[String]): Unit = {

    val groupsOfTwo: Observable[Seq[Double]] = ???

    val insideCircle: Observable[Boolean] = ???

    val hits: Observable[Hits] = ???

    val piApprox: Observable[Double] = ???

    val oneMillionApproximations: Observable[Double] = ???

    oneMillionApproximations.subscribe(println(_), _.printStackTrace(), () => println("done"))
  }
}
