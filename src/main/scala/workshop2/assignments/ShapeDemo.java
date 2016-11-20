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
package workshop2.assignments;

public class ShapeDemo {

	public static void main(String[] args) {
		Shape rectangle = new Rectangle(2, 1);
		Shape circle = new Circle(1);

		System.out.println(rectangle.circumference());
		System.out.println(rectangle.area());

		System.out.println(circle.circumference());
		System.out.println(circle.area());
	}

	// 3a. Implementation in Java with the operations as abstract methods on `Shape`
	//     Note: the static keywords are here to get it all working in a single file
	static abstract class Shape {
		abstract double circumference();
		abstract double area();
	}

	static class Rectangle extends Shape {
		double w, h;
		public Rectangle(double w, double h) {
			this.w = w;
			this.h = h;
		}

		@Override
		public double circumference() {
			return 2 * (w + h);
		}

		@Override
		public double area() {
			return w * h;
		}
	}

	static class Circle extends Shape {
		double r;
		public Circle(double r) {
			this.r = r;
		}

		@Override
		public double circumference() {
			return 2 * Math.PI * r;
		}

		@Override
		public double area() {
			return Math.PI * Math.pow(r, 2.0);
		}
	}
}
