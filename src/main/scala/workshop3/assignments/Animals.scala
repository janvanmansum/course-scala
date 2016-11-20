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

// Animal, Sound and Food are defined in package.scala

class Animals(animalSays: Map[Animal, Sound], asksForFood: Map[Sound, Food]) {

	// returns an Option of the sound an animal makes
	def whatSays(animal: Animal): Option[Sound] = ???

	// returns an Option of the sound (in CAPITAL) an animal makes
	def WHAT_SAYS(animal: Animal): Option[Sound] = ???

	// returns an Option of the food an animal eats
	def whatEats(animal: Animal): Option[Food] = ???

	// returns a List of statements "<animal> eats <food>" based on the animals listed in the input.
	def whatEat(animals: List[Animal]): List[String] = ???

	// return a List of statements "<animal> says <sound> and eats <food>" based on the animals listed in the input.
	def whatSaysAndEats(animals: List[Animal]): List[String] = ???

	// returns an Option of the food (in CAPITAL) an animal eats
	def WHAT_EATS(animal: Animal): Option[Food] = ???
}
