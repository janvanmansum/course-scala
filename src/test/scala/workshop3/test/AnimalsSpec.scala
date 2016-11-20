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
package workshop3.test

import org.scalatest.{FlatSpec, Matchers}
import workshop3.assignments.solutions.{AnimalsSolution => Animals}

class AnimalsSpec extends FlatSpec with Matchers {

	val animalSays = Map("Dog" -> "Woof", "Cat" -> "Meow", "Cow" -> "Mooh")
	val asksForFood = Map("Woof" -> "Meat", "Meow" -> "Fish", "Mooh" -> "Grass")

	val animals = new Animals(animalSays, asksForFood)

	animalSays.foreach { case (animal, sound) =>
			"whatSays" should s"return Option($sound) when the input is $animal" in {
				animals.whatSays(animal) shouldBe Option(sound)
			}

			"WHAT_SAYS" should s"return a capatilized version of $sound with input $animal" in {
				animals.WHAT_SAYS(animal) shouldBe Option(sound.toUpperCase)
			}
	}

	"whatSays" should "return None if the animal is unknown" in {
		animalSays.keys.foreach(animal => animals.whatSays(s"me and $animal") shouldBe None)
	}

	"WHAT_SAYS" should "return None if the animal is unknown" in {
		animalSays.keys.foreach(animal => animals.WHAT_SAYS(s"me and $animal") shouldBe None)
	}

	"whatEats" should "return Meat on input Dog" in {
		animals.whatEats("Dog") shouldBe Some("Meat")
	}

	it should "return Fish on input Cat" in {
		animals.whatEats("Cat") shouldBe Some("Fish")
	}

	it should "return Grass on input Cow" in {
		animals.whatEats("Cow") shouldBe Some("Grass")
	}

	it should "return None if the animal is unknown" in {
		animalSays.keys.foreach(animal => animals.whatEats(s"me and $animal") shouldBe None)
	}
}
