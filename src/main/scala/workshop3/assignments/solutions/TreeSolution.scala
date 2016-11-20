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
package workshop3.assignments.solutions

import scala.util.{Failure, Success, Try}

sealed abstract class Tree[A] {

	def map[B](f: A => B): Tree[B]

	def flatMap[B](f: A => Tree[B]): Tree[B]

	def zipWith[B](other: Tree[B]): Try[Tree[(A, B)]]
}

case class Leaf[A](a: A) extends Tree[A] {
	def map[B](f: A => B): Tree[B] = Leaf(f(a))

	def flatMap[B](f: A => Tree[B]): Tree[B] = f(a)

	def zipWith[B](other: Tree[B]): Try[Tree[(A, B)]] = {
		other match {
			case Leaf(b) => Success(Leaf((a, b)))
			case b => Failure(new IllegalArgumentException(s"this is a Leaf, while other is not ($b)"))
		}
	}
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
	def map[B](f: A => B): Tree[B] = Branch[B](left.map(f), right.map(f))

	def flatMap[B](f: A => Tree[B]): Tree[B] = Branch(left.flatMap(f), right.flatMap(f))

	def zipWith[B](other: Tree[B]): Try[Tree[(A, B)]] = {
		other match {
			case l@Leaf(_) => Failure(new IllegalArgumentException(s"this is a branch while other is not ($l)"))
			case Branch(l, r) =>
				for {
					ll <- left.zipWith(l)
					rr <- right.zipWith(r)
				} yield Branch(ll, rr)
		}
	}
}
