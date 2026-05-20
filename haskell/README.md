# JiggleFab

An attempt to demonstrate the first _organic-feeling visualizable Universal Constructor_ supported by a simple artificial chemistry that doesn't cheat by baking in too much stuff. What counts as "cheating" and "too much stuff" is an aesthetic I can't yet fully specify but you'll see details and examples in the bottom-up explanation of my choices below.

Universal Constructors are devices within an environment that can build anything within that environment including themselves. "Anything" here inevitably really means "anything within a very large class". Eg: A real-life atomic printer would require input resources and instructions and wouldn't fuse new atoms but would usually still count if it could print pretty much anything made of atoms including itself. Another eg, Conway proved the Game of Life Cellular Automata can host a Universal Constructor in the sense that anything that can be made from glider collisions could be made from the constructor.

Version 0.1.6 | [ChangeLog](ChangeLog.md) | [Roadmap](Roadmap.md)

Work in progress. Run with `stack run`

# Physics

The alternative physics used here is a euclidean [2D perfectly elastic](https://www.wikiwand.com/en/Elastic_collision#Two-dimensional) physics containing only identical circles of constant mass and radius called "balls". Locally euclidean and isotropic systems feel organic in a way that cellular automata can't, and this was the simplest organic physics I could think of.

Additionally, collisions can occur normally, from the outside, but can also occur from the inside. Balls can either bounce or pass through depending on the chemical rules. This allows two balls to be inside each other in the sense that each of their center points are inside the other ball. Having every ball be the same size makes sidedness a symmetric relationship, (ie. if A is in B then B is in A). Collisions from the inside are mathematically identical to collisions from the outside.

Initial speeds are constant and initial velocity directions are random. This forces more robustness into the biochemical design because designs aren't dependent on extremely low-entropy details. Designs can't depend on collision angles. Initial positions allow for low-entropy design but are just used to determine initial sidedness and containment (whether a chain of balls surrounds another) rather than for perfect placement. I'm currently using explicit initial positions instead of sidedness and containment to allow for a better editing experience but might drop it if I can find a nice alternative.

# Chemical Laws

I'm still settling the specific ruleset for the primary alternative chemistry but I've chosen some design constraints. Reactions are instant, only occur on collision, and ball states only change when reacting. Reactions take sidedness into account so that, on collision, different reactions can occur if they collided from the inside or from the outside.

Conservation of ball number is NOT preserved. Balls can be destroyed during a reaction. One ball can be created during a reaction as well, and is positioned exactly between the two colliding balls. New balls have the initial constant speed and random velocity. This causes approximate conservation of average velocity rather than conservation of total velocity.

A possible simplifying law would be to separate reactions into pass-through vs chemical changes. This would mean that on passt-hrough (ie. sidedness changes) the chemistry does not change. Then other chemical state changes would only occur on proper side-preserving collisions. Another possibility is for chemical changes to only occur on inner collisions so that a chemical change would first require pass-through. Another constraint would be to separate reactions into deletion, creation, and state-change reactions, where the states would only change in some minimal way when also performing deletion or creation reactions. So far these constraints or laws are mostly followed but are NOT fully followed in all designs.

# Chemistry

The primary design used across chemistries is the concept of a Wire. A Wire ball has a dormant state and a signal state. When a Wire collides with a Wire from the inside, then their states are swapped. By positioning Wire balls inside each other intransitively we can build chains of Wire that signal can travel down without letting externally colliding Wires transfer signal. Setting Wire to swap signal only to empty Wire ensures that signal order is preserved on a wire. Without further rules the signal travels randomly but if order is preserved and there is enough signal on the wire, the signals act like pressure pushing signals in the preferred direction.

It's useful to have balls in another chemical state that treat "inputs" differently from "outputs". Most of these chemistries use Port balls which get signals from Wire OR drop signals onto Wires, but NOT vice versa. So an input Port takes signal from Wire without swapping its held signal back to the Wire and an output Port rejects signal from Wire but will drop its held signal onto a Wire if the Wire is empty. Then other non-Wire non-Port balls can react differently with Ports holding signal than empty Ports, allowing directionality and real signalling to occur.

Throughout, every ball is implementing identical reactions and only acts differently due to two ball states and the sidedness of the reaction. Specifically, a Wire could become a Port or vice versa depending on the chemistry. This project contains many chemistries so that I can experiment but only one chemistry is actually run in any given simulation.

# Biochemistry

Biochemical functionality is limited at this point and most components are built with partially ad hoc minimal chemistries suitable for that specific component. Ultimately I would like "complete" biochemical functionality for a primary simple chemistry that can be used to build full functional systems.

Current Prototypes:
- **Striped-Wire** sends signal down a wire one-way rather than relying on random motion of signal back and forth on a wire
- **Encoder** converts one input signal into a pre-set stream of signals
- **Logic Gates** that wait for both signals to arrive from each wire before emitting the output
- **Turnbuckle** takes left or right signals and moves a "head" back and forth on a wire

Further functionality including things like a Chain Grower, a Chain Transporter, a Signal Loader, a Decoder, and many other components should allow for the building of biochemical objects based on signals. A system with the ability to construct other (dormant) biochemical objects, fasten them together, and activate them, should be enough to build a universal constructor capable of replication.
