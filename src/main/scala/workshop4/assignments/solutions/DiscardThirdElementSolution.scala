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
package workshop4.assignments.solutions

import rx.lang.scala.Observable

import scala.concurrent.duration.DurationInt
import scala.io.StdIn
import scala.language.postfixOps

trait DiscardThirdElementSolution {

  // comment and uncomment the choices below to see the differences

  /**
   * Given an `Observable[T]` called `obs`, returns an `Observable[T]` that skips every third element that is emitted by `obs`.
   *
   * @param obs the input `Observable`
   * @tparam T the type of elements in the `Observable`
   * @return an `Observable` that skips every third element emitted by `obs`
   */
  def discardThirdBuffer[T](obs: Observable[T]): Observable[T] = {
    obs.tumblingBuffer(3)
      .flatMapIterable(_.take(2))
//      .flatMapIterable(_.dropRight(1))
  }

  /**
   * Given an `Observable[T]` called `obs`, returns an `Observable[T]` that skips every third element that is emitted by `obs`.
   *
   * @param obs the input `Observable`
   * @tparam T the type of elements in the `Observable`
   * @return an `Observable` that skips every third element emitted by `obs`
   */
  def discardThirdWindow[T](obs: Observable[T]): Observable[T] = {
    obs.tumbling(3)
      .flatMap(_.take(2))
//      .flatMap(_.dropRight(1))
  }
}

object DiscardThirdElementWithBuffer extends App with DiscardThirdElementSolution {

  println("discard every third element using buffer")

  discardThirdBuffer(Observable.from(1 to 20))
    .subscribe(println(_), e => e.printStackTrace(), () => println("done"))
}

object DiscardThirdElementWithBufferAndInterval extends App with DiscardThirdElementSolution {

  println("discard every third element using buffer")

  discardThirdBuffer(Observable.interval(1 second).take(20))
    .subscribe(println(_), e => e.printStackTrace(), () => { println("done"); System.exit(0) })

  StdIn.readLine()
}

object DiscardThirdElementWithWindow extends App with DiscardThirdElementSolution {

  println("discard every third element using window")

  discardThirdWindow(Observable.from(1 to 20))
    .subscribe(println(_), e => e.printStackTrace(), () => println("done"))
}

object DiscardThirdElementWithWindowAndInterval extends App with DiscardThirdElementSolution {

  println("discard every third element using window")

  discardThirdWindow(Observable.interval(1 second).take(20))
    .subscribe(println(_), e => e.printStackTrace(), () => { println("done"); System.exit(0) })

  StdIn.readLine()
}
