# utilon

> [!WARNING]  
> This library is an experiment and I make no guarantees about anything whatsoever. If it turns out to be a good idea I might write docs and publish a crate.

Utilon is a utility AI library for Bevy. It takes inspiration from [big_brain](https://github.com/zkat/big-brain) and [bevy_observed_utility](https://github.com/ItsDoot/bevy_observed_utility), but tries to be simpler, more performant, and/or require less boilerplate. 

## Design goals

- Scoring should happen in ordinary user-defined Bevy systems to maximize performance and keep things idiomatic.
- Behaviors live directly on actors, without any hierarchy.
- Scores are calculated lazily and in sequence if a threshold score is specified, or eagerly and in parallel if asking for the maximum score.
- Scores can be cached automatically.
- Scorers depend only on their component inputs and never on each other, avoiding all issues relating to hierarchy and order of operations.
- State transitions are implemented automatically, and can optionally be hooked into if needed.
- Behaviors support both static and dynamic configuration. Response curves can be tweaked at runtime.
