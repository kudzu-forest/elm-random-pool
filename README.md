#elm-random-pool

This package is for those who wants random picking up system that can

  * change weight(probability) of chosen element and reuse it.
  * give constraints like "it takes at least 3 turns for the same element to appear again."
  * add new content at the moment when all the existing contents has appered at least one time.

etc.

## Mental Model
The data structure `RandomPool` consists of two part, called *pond* and *pump*.
A *pump* is a one-way queue with arbitrary length, and a *pond* is tree structure each element of which can be randomly chosen within log scaled time.
The randomness is only included in the sucking up process from the pond into the rear edge of pump, so all the operation that takes new element into the pomp must return `Random.Generator`.
