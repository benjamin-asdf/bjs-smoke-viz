# bjs-smoke-viz

Real-time generative smoke in Clojure: a **spectral (FFT) Stable-Fluids** solver,
driven by a **Physarum** (slime-mould) agent layer, with additive colour mixing,
a procedural noise-field wind, twinkling "stars", and switchable scene **themes**.
Rendered with [Quil](http://quil.info/).

![slime](assets/slime.gif)

## Run

```bash
clj -M:run        # opens the window
```

Drag the mouse to push/paint smoke. `space` pauses, `r` resets.

## Themes (modes)

A theme bundles a *mode* with its config — switch theme, switch the whole look
(call `start!` after switching for a clean reset):

| theme | what |
|-------|------|
| `:slime`   | **default** — Physarum agents emit the smoke: they sense the smoke *density* as their chemoattractant (density == trail) and deposit their colour; the fluid advects it into flowing coloured networks. |
| `:jets`    | colored moving **sources** emit the smoke (Brownian drift + boids flocking), no Physarum. |
| `:network` | the classic **white** Physarum network on its own trail map. |

On top of any theme, **stars** spawn as persistent bright particles at
high-density peaks — they drift with a small acceleration and twinkle white.

## Live coding

```bash
clj -M:nrepl      # connect your editor, then:
```

```clojure
(require '[smoke.core :as sc])
(sc/start!)                                  ; (re)launch  ·  (sc/start! :fullscreen true)
(swap! sc/params assoc :theme :network)      ; :slime :jets :network  -> then (sc/start!)
(swap! sc/params assoc :star-thresh 3.5)     ; rarer stars   ·  :stars false to disable
(swap! sc/params assoc :wind 6 :blur-passes 1)
(sc/save-frame! "/tmp/f.png")                ; dump the current frame
```

Knobs live in `smoke.scene/default-params`; handlers are `#'`vars so functions
can be redefined live too.

## How it works

- **Fluid** (`smoke.fluid`) — Jos Stam's spectral *Stable Fluids*: `add force →
  self-advect → FFT → diffuse+project → IFFT`. The projection onto
  divergence-free flow is exact in Fourier space (remove the component of each
  wavevector parallel to **k**), so there's no iterative solver and no
  collocated-grid checkerboard. The domain is periodic; an absorbing sponge
  border fades flow at the edges. Density is carried in **R/G/B channels** so
  colours mix additively.
- **Physarum** (`smoke.physarum`) — agents sense a field at 3 forward sensors,
  steer toward the strongest, move (dragged by the fluid wind), and deposit. In
  `:slime` they sense the smoke density and deposit colour; in `:network` they
  use a separate white trail that diffuses + decays (Jones 2010).
- **Boids** (`smoke.boids`) — in `:jets`, the handful of sources flock
  (separation / alignment / cohesion) so the emitters drift in loose coordination.
- **Wind** — a procedural noise flow-field force, so the smoke gusts and swirls.
- **Stars** — persistent particles spawned at density peaks, drifting and
  flashing white via a time lerp (rendered as bright colour discs).
- **Headless** (`smoke.headless`) — render frames to PNG with no window
  (`snap`, `film`); used for tuning, stills, and the GIFs here.

## References

- Jos Stam, *Stable Fluids*, SIGGRAPH 1999 — the spectral/FFT method used here.
- R. Fedkiw, J. Stam, H. Jensen, *Visual Simulation of Smoke*, SIGGRAPH 2001.
- Jeff Jones, *Characteristics of Pattern Formation and Evolution in Approximations
  of Physarum Transport Networks*, Artificial Life 2010 — the slime-mould model.
- Craig Reynolds, *Flocks, Herds, and Schools: A Distributed Behavioral Model*,
  SIGGRAPH 1987 — boids.
- Reference FFT Stable-Fluids ports: [daichi-ishida](https://github.com/daichi-ishida/Stable-Fluids),
  [richardbenstead](https://github.com/richardbenstead/Stable-Fluids).

## Libraries

[Quil](http://quil.info/) (rendering) · [JTransforms](https://github.com/wendykierp/JTransforms)
(FFT) · [dtype-next](https://github.com/cnuernber/dtype-next).
