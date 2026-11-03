# Baleia

Have you ever wanted to code DSP in Haskell straight in your DAW?? No? Well whatever, now you can.

Baleia is a small arrow-based DSP language embedded in Haskell, compiled into a native shared library (`Baleia.dll`) that an audio host calls block by block through a C interface.

## Writing a patch

A patch is a Haskell module with a `Control` record (its parameters) and a `process` signal function written in `proc` notation. It takes the controls and a stereo `Sample` and returns them, so patches can also write output parameters.

```haskell
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
module MyModule where
import Baleia
import Baleia.Note as N

data Control = Control
    { _Gain :: Float
    } deriving Data

initControl = Control 0.1

process = proc (c, _) -> do
    l <- oscSqr -< N.a3
    r <- oscSaw -< N.a2
    returnA -< (c, mono $ (l + r) * 0.5 * _Gain c)
```

Field names become host parameter names via Template Haskell introspection, and `initControl` provides their defaults. The library builds the patch in `src/MyModule.hs`. See [`demo/`](demo) for more examples.

### Building blocks

| Module | Contents |
|---|---|
| `Baleia.Internal` | `SF` signal-function arrow; oscillators (`oscSin`, `oscSqr`, `oscSaw`, `oscTri`, `oscRamp`, `oscWNoise`); filters (`lpf`, `bpf`, `hpf`); `fdbkDelay`, `fdbkDelayDyn`; `mono`, `pan`, `clip`, `dB` |
| `Baleia.Note` | Note frequencies (`a3`, `cs'1`, ...) |
| `Baleia.Time` | Musical durations in samples: `ms`, `seconds`, `beats`, `bars` (e.g. `3 beats`) |
| `Baleia.Sample` | `Sample = (Float, Float)` with numeric instances |

## Building

Requires GHC and Cabal ≥ 3.12, on **Windows** (the linker flags are MinGW-specific).

```sh
cabal build
```

This produces a standalone `Baleia.dll` with the exports listed in `Baleia.def`.

## C interface

| Function | Purpose |
|---|---|
| `HsStart()` / `HsEnd()` | Start and stop the Haskell runtime |
| `startCtx()` / `freeCtx(ctx)` | Create and free a patch instance |
| `main2(ctx, nSamples, nParams, ctrlIn, ctrlOut, inL, inR, outL, outR)` | Process one block of stereo audio |
| `controlInMeta()` | Parameter names and default values as a string |

The host-side plugin that loads the DLL is not part of this repository. Sample rate and tempo are currently hard-coded in `Baleia.Time` (44.1 kHz, 150 BPM).
