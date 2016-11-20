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

import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets

import rx.observables.StringObservable
import rx.lang.scala.JavaConverters._
import rx.lang.scala.Observable
import rx.observables.StringObservable.UnsafeFunc0

import scala.language.implicitConversions

object RxJavaStringFromInputStream extends App {

  /*
    StringObservable.from() reads from an InputStream by taking chunks in memory and processing them before getting the next chunk.
    By default a chunk is 8 * 1024 bytes, but you can give your own size as an extra parameter.

    Remember to close the InputStream after you're done!
   */

  val fis = new FileInputStream(new File(getClass.getResource("/workshop4/LorumIpsum.txt").toURI))
  val bytes = StringObservable.from(fis)

  bytes.asScala
    .map(new String(_))
    .subscribe(x => println(s">>> CHUNK: $x\n\n\n"))

  fis.close()
}

object RxJavaStringUsing extends App {

  /*
    StringObservable.using gives you the ability to automatically close resources at onError on onCompleted
    Due to the combination of Java8 and Scala_2.11, we need to define some function conversions ourselves
    or import them from the RxScala library. This is fixed in Scala_2.12, where these are no longer necessary.

    The example below does the same as in RxJavaStringFromInputStream above, but does the resource closing by itself.
   */

  import rx.lang.scala.ImplicitFunctionConversions._

  implicit def funcToUnsafeFunc0[T](fun: () => T): UnsafeFunc0[T] = new UnsafeFunc0[T] {
    def call(): T = fun()
  }

  lazy val fis = new FileInputStream(new File(getClass.getResource("/workshop4/LorumIpsum.txt").toURI))
  val bytes = StringObservable.using[Array[Byte], FileInputStream](() => fis, (in: FileInputStream) => StringObservable.from(in))

  bytes.asScala
    .map(new String(_))
    .subscribe(x => println(s">>> CHUNK: $x\n\n\n"))
}

object RxJavaStringDecode extends App {

  /*
    StringObservable.decode lets you automatically decode the byte arrays into Strings using a particular encoding.
    The encodings can either be given by String, Charset or CharsetDecoder.

    StringObservable.encode (not listed here) does the opposite: it takes an Observable[String] and returns an Observable[Array[Byte]].
   */

  import rx.lang.scala.ImplicitFunctionConversions._

  implicit def funcToUnsafeFunc0[T](fun: () => T): UnsafeFunc0[T] = new UnsafeFunc0[T] {
    def call(): T = fun()
  }

  lazy val fis = new FileInputStream(new File(getClass.getResource("/workshop4/LorumIpsum.txt").toURI))
  val bytes = StringObservable.using[Array[Byte], FileInputStream](() => fis, (in: FileInputStream) => StringObservable.from(in))
  val string = StringObservable.decode(bytes, StandardCharsets.UTF_8)

  string.asScala
    .subscribe(x => println(s">>> CHUNK: $x\n\n\n"))
}

object RxJavaStringConcat extends App {

  /*
    StringObservable.stringConcat creates one String out of many Strings by concatenating them.
    Following up on RxJavaStringDecode above, the code below combines all the the decoded chunks,
    loading the whole input file into memory and emitting it as one element in an onNext.
   */

  import rx.lang.scala.ImplicitFunctionConversions._

  implicit def funcToUnsafeFunc0[T](fun: () => T): UnsafeFunc0[T] = new UnsafeFunc0[T] {
    def call(): T = fun()
  }

  lazy val fis = new FileInputStream(new File(getClass.getResource("/workshop4/LorumIpsum.txt").toURI))
  val bytes = StringObservable.using[Array[Byte], FileInputStream](() => fis, (in: FileInputStream) => StringObservable.from(in))
  val string = StringObservable.decode(bytes, StandardCharsets.UTF_8)
  val concat = StringObservable.stringConcat(string)

  concat.asScala
    .subscribe(println(_))
}

object RxJavaStringSplit extends App {

  /*
    StringObservable.split takes a regular expression and splits the stream of Strings into separate Strings
    based on this regular expression. Note that this even goes in between two onNext events, as shown below!

    Note that this example shows a downside of mixing RxScala and RxJava. `strings` is an RxScala Observable
    and therefore converts to a RxJava Observable[_ >: String] rather than Observable[String]. This has to
    do with covariance and contravariance and requires an extra cast to the actual String type for the
    StringObservable.split to accept it as an argument.
   */

  val strings = Observable.just("Lorum ip", "sum dolo", "r sit am", "et, cons")
  val jStrings: rx.Observable[String] = strings.asJava.cast(classOf[String])
  val words = StringObservable.split(jStrings, " ")

  words.asScala
    .subscribe(word => println(s"word: $word"))
}

object RxJavaStringJoin extends App {

  /*
    StringObservable.join concatenates the various Strings in this Observable, but puts a separator
    in between them.
   */

  val strings = Observable.just("abc", "def", "ghi")
  val jStrings: rx.Observable[String] = strings.asJava.cast(classOf[String])
  val joined = StringObservable.join(jStrings, " - ")

  joined.asScala
    .subscribe(println(_))
}

object RxJavaStringByLine extends App {

  /*
    StringObservable.byLine provides the ability to split a text on newline characters, resulting
    in an Observable[String] where each String is a single line.

    In the code below we follow up on earlier examples and split the text by newline characters.
    Because the text in the file contains empty lines, we filter them out, such that only the
    nonempty lines remain.
   */

  import rx.lang.scala.ImplicitFunctionConversions._

  implicit def funcToUnsafeFunc0[T](fun: () => T): UnsafeFunc0[T] = new UnsafeFunc0[T] {
    def call(): T = fun()
  }

  lazy val fis = new FileInputStream(new File(getClass.getResource("/workshop4/LorumIpsum.txt").toURI))
  val bytes = StringObservable.using[Array[Byte], FileInputStream](() => fis, (in: FileInputStream) => StringObservable.from(in))
  val string = StringObservable.decode(bytes, StandardCharsets.UTF_8)
  val lines = StringObservable.byLine(string)

  lines.asScala
    .filterNot(_.isEmpty)
    .subscribe(line => println(s"line: $line"))
}

object RxJavaStringByCharacter extends App {

  /*
    StringObservable.byCharacter creates an Observable where each onNext event contains a String with
    a single character.
   */

  import rx.lang.scala.ImplicitFunctionConversions._

  implicit def funcToUnsafeFunc0[T](fun: () => T): UnsafeFunc0[T] = new UnsafeFunc0[T] {
    def call(): T = fun()
  }

  lazy val fis = new FileInputStream(new File(getClass.getResource("/workshop4/LorumIpsum.txt").toURI))
  val bytes = StringObservable.using[Array[Byte], FileInputStream](() => fis, (in: FileInputStream) => StringObservable.from(in))
  val string = StringObservable.decode(bytes, StandardCharsets.UTF_8)
  val chars = StringObservable.byCharacter(string)

  chars.asScala
    .subscribe(character => println(s"character: $character\n"))
}
