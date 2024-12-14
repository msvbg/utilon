# utilon

> [!WARNING]  
> This library is an experiment and I make no guarantees about anything whatsoever. If it turns out to be a good idea I might write docs and publish a crate.

Utilon is a utility AI library for Bevy. It takes inspiration from [big_brain](https://github.com/zkat/big-brain) and [bevy_observed_utility](https://github.com/ItsDoot/bevy_observed_utility), but tries to be simpler, more performant, and/or require less boilerplate. 

## Design goals

- Actions are components on entities, and there is no hierarchy or order of operations.
- Scores can be cached automatically.
- State transitions can be hooked into via observers, and are otherwise handled automatically.
- Behaviors support both static and dynamic configuration. Response curves can be tweaked at runtime.
