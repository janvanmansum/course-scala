Approximating pi
================

There are many ways to [approximate pi]. In this assignment we will do so using a variant of *[summing a circle's area]*.
You can view this as randomly throwing a large number of darts on a `r x r` board with origin `(0, 0)` and measuring how
many of them have a distance smaller than or equal to `r`. The ratio between these and the total number of darts thrown
should give an approximation of pi. In our case we use a `1 x 1` board and our darts are groups of two random numbers
between 0.0 and 1.0, representing the x- and y-coordinate respectively.

[approximate pi]: https://en.wikipedia.org/wiki/Approximations_of_%CF%80
[summing a circle's area]: https://en.wikipedia.org/wiki/Approximations_of_%CF%80#Summing_a_circle.27s_area
 
It is **not** the goal of this exercise to understand the algorithm or to have fun with math, but to practice with a
number of operators in Rx. Each of the steps below represents *a single operator* to be added to a chain of subsequent
operators.

1. Given is the function `random` that generates an infinite stream of random numbers between 0.0 and 1.0. Make sure you
   understand this what this function does and why it does it. Subscribe to the resulting `Observable` to see what it does
   in practice. Also try to understand what `Observable.defer` is doing; we haven't discussed it yet, but the documentation
   should give you some pointers. Check out what happens when we peel off the `Observable.defer`. **Hint:** this construction
   is equal to the [RxScala variant] of [`Observable.fromCallable`].
   
   - *Don't forget to recompile!*. Without `defer` the one value from the `just`-observable is *just* repeated.
   
2. In the `main` method, call `random` and make groups of 2 random numbers; this should yield an `Observable[Seq[Double]]`;
   allocate this to `groupsOfTwo`.
3. Each group of 2 numbers (type `Seq[Boolean]`) represents the `x`- and `y`-coordinate of a single point respectively. Using
   the value `groupsofTwo`, transform each coordinate into a `Boolean` such that it indicates whether the distance to the
   circle's origin is smaller than or equal to `1.0`. Allocate this result to `insideCircle: Observable[Boolean]`.
   **Hint:** use `math.sqrt(x * x + y * y) <= 1.0` in this calculation.
4. Given `insideCircle`, transform each `Boolean` into an accumulated `Hits` object. If `true`, 1 is added to `successes`,
   else `successes` stays the same. `total` is incremented every time, regardless of the `Boolean`. The result should be an
   accumulated stream of `Hits` instances (so, including all intermediate results!). Allocate this to `hits: Observable[Hits]`.
   **Hint (1):** use `Hits.empty` as the seed of this operator. **Hint (2):** use `booleanToInt` to transform the `Boolean`s.
5. What should happen to the seed value? Is this part of the actual result or should this be discarded? Just append the
   action to be taken (if any) to `hits`, as this is actually still part of step 4.
6. Transform each `Hits` instance in the stream `hits` to a `Double` using the formula `hit => hit.successes * 4.0 / hit.total`.
   This calculates an intermediate approximation of pi. Allocate the result to `piApprox: Observable[Double]`.
7. Until now the stream is not bounded in size and produces an infinite number of approximations. In this step, make sure
   that the stream terminates with an `onCompleted` after 1.000.000 approximations. Allocate the result to
   `oneMillionApproximations: Observable[Double]`.
8. `oneMillionApproximations` is subscribed to by printing every approximation to the console, printing the stacktrace of
   any error and printing `done` once the stream completes (as specified in the previous step).

Make sure that by running this stream, you get an output of each intermediate approximation of pi.

Finally, chain all operators into one long query to make it more readable.

[02 Observable.md]: ../02%20Observable.md
[RxScala variant]: http://reactivex.io/rxscala/comparison.html
[`Observable.fromCallable`]: http://reactivex.io/RxJava/javadoc/rx/Observable.html#fromCallable(java.util.concurrent.Callable)
[`repeat`]: http://reactivex.io/rxscala/scaladoc/index.html#rx.lang.scala.Observable@repeat:rx.lang.scala.Observable[T]
