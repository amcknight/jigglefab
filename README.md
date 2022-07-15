# chains

Version 0.1.1

[ChangeLog](ChangeLog.md) | [Roadmap](Roadmap.md)

Work in progress. Run with `stack run`

# Physics

The alternative physics used here is a specialized classical physics where there are two dimensions, all masses are constant, all shapes are circles of the same radius, and the only force is an instant elastic collision force. I chose this physics so that reactions would be isotropic and would feel more organic compared to cellular automata but still quite simple.

Additionally, collisions can occur normally, from the outside, but can also occur from the inside. This is because balls can either bounce or pass through depending on the chemical rules. This allows two balls to be inside each other in the sense that each of their center points are inside the other ball. Having every ball be the same size makes sidedness a symmetric relationship, (ie. if A is in B then B is in A). Collisions from the inside are mathematically identical to collisions from the outside.

Initial speeds are constant and initial direction of velocity is random. This forces more robustness into the biochemical design because it must be part of the initial positions and chemistries rather than the precise velocties. Designs can't depend on collision angles.

# Chemical Laws

The primary alternative chemistry used here has not yet settled to a specific ruleset but some design constraints have been decided. Reactions are instant, only occur on collision, and the state of each ball only changes when they react. Reactions take sidedness into account so that, on collision, different reactions can occur if they collided from the inside or from the outside.

Conservation of ball # is NOT preserved. Balls can be destroyed during a reaction. One ball can be created during a reaction as well, and is positioned exactly between the two colliding balls. New balls have the initial constant speed and random velocity. This causes approximate conservation of average velocity rather than conservation of total velocity.

A possible simplifying law would be to separate reactions into pass-through vs chemical changes. This would mean that, on passthru (ie. sidedness was going to change), then the chemistry does not change. Then other chemical state changes would only occur on a proper collision. Another possibility is for chemical changes to only occur on inner collisions so that a chanemical change would first require pass-through. Another constraint would be to separate reactions into deletion, creation, and state-change reactions, where the states would only change in some minimal way when also performing deletion or creation reactions. So far these constraints or laws are mostly followed but are NOT fully followed in all designs.

# Chemistry
The current primary chemistry is incomplete but there are a few building blocks.

The most basic design used across chemistries is the concept of a Wire. A Wire ball has a dormant state and a signal state. When a Wire collides with a Wire from the inside, then their states are swapped. This allows the signal to travel down Wire chains.

When dealing with signals from wires, it's useful to have special balls with different chemistry, but then those special balls should treat inputs differently from outputs. So something is required to distinguish these cases. Ports generally get signals from Wire OR drop signals onto Wires, but NOT vice versa. So an input Port would take signals from Wires and then a special ball would react differently with Ports with signals than Ports without signals. There are also designs such as the sync Port which requires two Ports with signal to collide before starting dowstream reactions. This allows for building logic gates that don't require synchronized signal timing.

# Biochemistry
Biochemical functionality is very limited at this point and has so far been done with different chemistries suitable to creating the functionality. Ultimately I would like "complete" biochemical functionality for a primary simple chemistry that can be used to build full functional systems. Currently I have prototype Encoders (which convert an input signal into a pre-set stream of signals), a Turnbuckle (which takes left or right signals and moves a "head" back and forth on a wire), Striped-Wire (which sends signal down a wire one-way rather than relying on random motion of signal back and forth on a wire), and a few Logic Gates.

Further functionality including things like a Chain Grower, a Chain Transporter, a Signal Loader, a Decoder, and many other components  should allow for the building of biochemical objects based on signals. A system with the ability to construct other (dormant) biochemical objects, fasten them together, and activate them, should be enough to build a universal constructor capable of replication.
