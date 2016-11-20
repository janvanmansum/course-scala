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

object Fibonacci extends App {

  def fibonacci: Observable[Int] = {
    Observable.just(0)
      .repeat
      .scan((0, 1)) { case ((pp, p), _) => (p, pp + p) }
      .map(_._2)
  }

  fibonacci.take(10).subscribe(i => println(i))
//  fibonacci.takeWhile(n => n < 100).subscribe(i => println(i))
//  fibonacci.filter(n => n % 2 == 0).takeWhile(n => n < 1000).subscribe(i => println(i))
}
