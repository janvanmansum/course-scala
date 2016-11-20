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

sealed abstract class Employee
case class Developer(name: String, salary: Int) extends Employee
case class Manager(name: String, salary: Int, title: String) extends Employee

object Employee {
	// returns the salary of the employee
	def salary(employee: Employee): Int = ???

	// returns the sum of all salaries
	// HINT: use the previous function
	def totalSalary(employees: List[Employee]): Int = ???

	// returns the name of the employee
	def name(employee: Employee): String = ???

	// returns the employee, but with the new salary
	def setSalary(salary: Int, employee: Employee): Employee = ???

	// returns the employee, but with the increased salary
	def raiseSalary(increase: Int, employee: Employee): Employee = ???

	// returns the list of all employees, but with a raise for the manager's salary
	def raiseManagerSalaries(increase: Int, employees: List[Employee]): List[Employee] = ???

	// returns the list of all employees, but with a raise for the developer's salary
	def raiseDeveloperSalaries(increase: Int, employees: List[Employee]): List[Employee] = ???

	// returns the list of all employees, but with a percentage raise for the manager's salary
	def raiseManagerSalariesByPercent(percent: Int, employees: List[Employee]): List[Employee] = ???

	// returns the list of all employees, but with a percentage raise for the developer's salary
	def raiseDeveloperSalariesByPercent(percent: Int, employees: List[Employee]): List[Employee] = ???
}
