# Von Noman Standing

## Summary

## Components

Your base is made up of the buildings you control. It is organized as:

```
RAM           (Cache Type)  (Cache Size)
(XCores)      CPU*          Spawns
Clock         (Pipeline)    (Branch Predictor)
```

Spawns is not destroyable.

## Upgrades

### Branch predictor

You start with no branch prediction whatsoever - every if statement will trash
the pipeline.

Building it gives you a 2 state saturation counter.

Second upgrade gives you a 4 state saturation counter.

Third upgrade gives you an oracle predictor - no if statement will trash the
pipeline.

### Cache Type

You start with a simple direct-mapped cache.

Building it gives you a two way set associative cache.

Second upgrade gives you a four way set associative cache.

Third upgrade gives you a fully associative cache.

### Cache Size

You start with ? bytes of cache.

Building it gives you ? bytes of cache.

Second upgrade gives you ? bytes of cache.

Third upgrade gives you ? bytes of cache.

### Cores

You start with 1 CPU core.

Building it gives 2 CPU cores.

Second upgrade gives 4 CPU cores.

Third upgrade gives 8 CPU cores.

### Pipeline

The pipeline is divided into 4 sections: Fetch, Decode, Execute, Write

You start with NO pipelining.

Building it gives 2 stage pipelines: 2 instructions can exist in the pipeline
at once.

Second upgrade gives 3 stage pipelines: 3 instructions can exist in the pipeline
at once.

Third upgrade gives 4 stage pipelines: 4 instructions can exist in the pipeline
at once.


---
NOTES:

Write your own AI.

Can upgrade your system (EG: cache, branch prediction). Can target other
player's system.

Looks like an RTS/Dota-style when watched. "Looks good" is important - must be
fun to watch!

Requires at least a basic compiler.

1 AI to control all your units!


