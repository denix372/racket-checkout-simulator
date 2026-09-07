# Functional Supermarket Checkout Simulator

A purely functional simulation of a supermarket checkout system, built in Racket. 

This project models the flow of customers through supermarket checkout counters based on a stream of events (customer arrivals, counter delays, time progression, and counter management). It avoids mutable state entirely, relying on recursion, higher-order functions, and persistent data structures to transition the system from one state to the next.

## Key Features

* **Dynamic Queue Balancing:** Automatically routes customers to the most optimal counter based on wait times (Total Time) and departure times (Exit Time).
* **Express & Standard Lanes:** Supports different types of counters (e.g., fast lanes restricted to a small number of items).
* **Auto-Scaling:** Dynamically opens new counters when the average wait time across the store exceeds a configurable threshold.
* **Complex Event Handling:** Accurately processes chronologically mixed events, including time progression, unexpected counter delays, and the opening/closing of counters (including the dynamic redistribution of waiting customers).

## Technical Architecture & Concepts

This project was built to demonstrate advanced functional programming paradigms and algorithmic optimizations:

* **Purely Functional State Management:** The entire simulation runs without side effects or mutable variables. State updates are achieved through currying and higher-order functions mapping over the counter states.
* **Abstract Data Types (ADT):** Customer lines are managed using a custom Queue ADT.
* **Amortized to Worst-Case $O(1)$ Optimization:** 
  * *Iteration 1:* The queue was initially implemented using a two-stack approach (one for enqueue, one for dequeue), achieving an **amortized $O(1)$** complexity.
  * *Iteration 2:* To eliminate the $O(N)$ worst-case spike during list reversal, the internal representation was upgraded to use **Streams (Lazy Evaluation)**. By delaying the evaluation of the queue rotation, the ADT achieves strict **worst-case $O(1)$** time bounds for both enqueue and dequeue operations.

## Technologies Used
* **Language:** Racket (Scheme/Lisp dialect)
* **Paradigms:** Functional Programming, Lazy Evaluation, Event-Driven Simulation

## Example Flow
The simulator consumes a list of heterogeneous requests:
1. `(name items)` - Customer joins the best available queue.
2. `(delay index minutes)` - A specific counter is delayed.
3. `(ensure average)` - Opens new counters if the average wait time is too high.
4. `x (integer)` - Advances the simulation time by `x` minutes, triggering customer departures.
5. `(close index)` / `(open index)` - Manages counter availability, automatically redistributing customers from closed lanes to active ones.