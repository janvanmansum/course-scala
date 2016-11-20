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
package workshop4.assignments.solutions.RxEcosystem

import com.github.davidmoten.rx.slf4j.Logging
import com.github.davidmoten.rx.slf4j.Logging.Level
import rx.lang.scala.{Observable, Subscriber}
import rx.lang.scala.JavaConverters._
import rx.lang.scala.ImplicitFunctionConversions._

object Example1 extends App {

  Observable.from(11 until 3011)
    .lift((subscriber: Subscriber[Int]) => {
      Logging.logger()
        .showValue()
        .showCount()
        .every(1000)
        .showMemory()
        .log()
        .call(subscriber.asJavaSubscriber)
        .asScalaSubscriber
    })
    .subscribe()
}

object Example2 extends App {

  def logger(subscriber: Subscriber[Int]): Subscriber[Int] = {
    // make a logger builder and give it a name
    Logging.logger("my logger")
      // prints a counter of the onNext events (with a custom text if you want...)
      .showCount("onNext counter")
      // start logging at 5th elements
      .start(5)
      // finish logging after 20 elements
      .finish(20)
      // only log every 3rd element
      .every(3)
      // only log when it's an even number
      .when((i: Int) => i % 2 == 0)
      .showCount("filtered count")
      // set the log level and message of an onCompleted event
      .onCompleted(Level.DEBUG)
      .onCompleted("the stream has completed")
      // set log level of an onError event
      .onError(Level.WARN)
      // set log level and formatting for onNext events
      .onNext(Level.INFO)
      .onNextFormat("this is event number %s")
      // set log level for an subscription event
      .subscribed(Level.DEBUG)
      // set log level and message for an unsubscribe event
      .unsubscribed(Level.ERROR)
      .unsubscribed("Noooo!!! Don't unsubscribe from me!")
      // create the actual logger
      .log()
      // conversion between RxJava and RxScala
      .call(subscriber.asJavaSubscriber)
      .asScalaSubscriber
  }

  Observable.from(1 until 100)
    .lift(logger)
    .map(i => 2 * i)
    .subscribe(x => println(s">>> $x"))
}
