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

case class Postcard(msg: String)

object PostcardAssignment extends App {

	val cities = List(
		"San Francisco", "Los Angeles",
		"Las Vegas", "San Diego",
		"Dallas", "Houston",
		"Chicago", "New York City",
		"Philadelphia")
	val relatives = List("Grandma", "Grandpa", "Aunt Dottie", "Dad")
	val travellers = List("Alice", "Bob")

	def sendPostcardsImperative: List[Postcard] = {
		var postcardList: List[Postcard] = List()

		for (h <- 0 until travellers.length) {
			val traveller = travellers(h)
			val sender = s"$traveller (your favorite)"
			for (i <- 0 until relatives.length) {
				val relative = relatives(i)
				if (relative.startsWith("G")) {
					for (j <- 0 until cities.length) {
						val theCity = cities(j)
						postcardList ::= Postcard(s"Dear $relative, Wish you were here in $theCity! Love, $sender")
					}
				}
			}
		}

		postcardList
	}

	def sendPostcardsFunctional: List[Postcard] = ???

	def sendPostcardsForComprehension: List[Postcard] = ???

	// tests:
	println(
		"""
			|======================
			| Imperative postcards
			|======================
			|""".stripMargin)
	sendPostcardsImperative.foreach(println)

	println(
		"""
			|======================
			| Functional postcards
			|======================
			|""".stripMargin)
	sendPostcardsFunctional.foreach(println)

	println(
		"""
			|=============================
			| For Comprehension postcards
			|=============================
			|""".stripMargin)
	sendPostcardsForComprehension.foreach(println)

	println(
		"""
			|=============
			| Equivalence
			|=============
			|""".stripMargin)
	println(s"imperative and functional have the same set of postcards: ${sendPostcardsImperative.toSet == sendPostcardsFunctional.toSet}")
	println(s"imperative and for comprehension have the same set of postcards: ${sendPostcardsImperative.toSet == sendPostcardsForComprehension.toSet}")
}
