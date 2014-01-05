Wreath
======

The Wreath library offers a tiny EDSL for designing a wreath of lights,
along with a few programs to exercise it. The motivation was a request
from my wife to make such a thing (a real lighted wreath) combined with
my desire to practice Haskell programming.

The EDSL grew to exactly accommodate my needs and is therefore very limited. 
However, I was able to use it to draw pictures of a lighted wreath to elicit
feedback, view an array of wreaths with small changes in order to tune parameters,
render an engineering diagram for construction, and create instructions for
installing the lights.

The aforementioned programs are in the examples directory.

There is a small test suite that I'm adding after the fact for more practice.
The code was developed with interactive testing in GHCI.

Here's an example of the simulation:

![Simulation](https://github.com/bobgru/wreath/blob/master/images/simulation.png?raw=true "Simulation")

Here the same parameters are used to create the engineering drawing:

![Design](https://github.com/bobgru/wreath/blob/master/images/design.png?raw=true "Design")


Here are instructions for stringing the lights, again from the same data:

![Walk](https://github.com/bobgru/wreath/blob/master/images/walk.png?raw=true "Walk")


Here's an admittedly terrible photograph of the real artifact:

![Physical wreath](https://github.com/bobgru/wreath/blob/master/images/wreath-night.jpg?raw=true "Physical wreath")
