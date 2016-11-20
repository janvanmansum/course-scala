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
package workshop3.assignments

object OperatorsAsFolds {

  def size[T](list: List[T]): Int = ???

  def filter[T](list: List[T])(predicate: T => Boolean): List[T] = ???

  def map[T, S](list: List[T])(f: T => S): List[S] = ???

  def forall[T](list: List[T])(predicate: T => Boolean): Boolean = ???

  def exists[T](list: List[T])(predicate: T => Boolean): Boolean = ???

  def reverse[T](list: List[T]): List[T] = ???

  def find[T](list: List[T])(predicate: T => Boolean): Option[T] = ???
}

object SomeOtherImplementations {

  def map[A, B](list: List[A])(f: A => B): List[B] = ???

  def apply[A, B](list: List[A], fs: List[A => B]): List[B] = ???

  def flatten[A](listOfLists: List[List[A]]): List[A] = ???

  def runningSum(list: List[Int]): List[Int] = ???

  def runningAverage(list: List[Double], n: Int): List[Double] = ???
}
