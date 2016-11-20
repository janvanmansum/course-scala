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

abstract class ImperativeDinner {

	def getCharcoal: Charcoal
	def getLighterFluid: LighterFluid
	def getMeat: Meat
	def lightBBQ(c: Charcoal, lf: LighterFluid): Fire
	def seasonMeat(m: Meat): Steak
	def grill(s: Steak, f: Fire): Dinner

	// this method never returns null!!!
	def orderPizza: Dinner = "I ordered a pizza"

	def prepareDinner: Dinner = {
		val charcoal = getCharcoal
		val lighterFluid = getLighterFluid
		val meat = getMeat

		val hopefullyDinner =
			if (charcoal != null && lighterFluid != null && meat != null) {
				val bbq = lightBBQ(charcoal, lighterFluid)
				if (bbq != null) {
					val steak = seasonMeat(meat)
					if (steak != null) {
						grill(steak, bbq)
					}
					else null
				}
				else null
			}
			else null

		if (hopefullyDinner == null) orderPizza else hopefullyDinner
	}
}

abstract class HigherOrderDinner {

	// this method never returns null!!!
	def orderPizza: Dinner = "I ordered a pizza"

	def prepareDinner: Dinner = ???
}

abstract class DinnerWithForComprehension {

	// this method never returns null!!!
	def orderPizza: Dinner = "I ordered a pizza"

	def prepareDinner: Dinner = ???
}
