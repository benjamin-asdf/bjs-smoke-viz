# bjs-smoke-viz

2D real-time smoke in Clojure. Jos Stam's **Stable Fluids** solver (semi-Lagrangian
advection + Gauss-Seidel pressure projection) with **buoyancy** and **vorticity
confinement** for the smoke look, rendered with **Quil**. Sim fields are plain
`float-array`s (dtype-next-compatible buffers) so the hot loops stay primitive.

## Run

```bash
clj -M:run          # opens the window
```

Drag the mouse to inject smoke; a steady plume rises from the bottom.
`space` pauses, `r` resets.

## Live coding

```bash
clj -M:nrepl        # then connect your editor; tweak smoke.core tunables and re-eval
```

## Layout

| file | what |
|------|------|
| `src/smoke/fluid.clj` | the solver — `make-fluid`, `vel-step`, `dens-step`, plus the operators (advect/diffuse/project) and the smoke forces (buoyancy/vorticity). All on `(n+2)²` float grids. |
| `src/smoke/core.clj`  | Quil fun-mode sketch — input, the update loop, and the PImage pixel blit. Tunables (`N`, `DT`, `BUOY`, `VORT`, …) at the top. |

## Next: fungus

The solver exposes velocity (`:u` `:v`) and a scalar density (`:dens`) on a shared
grid, which is exactly what a Physarum / slime-mold layer needs:

- **slime agents** sense + deposit into a new trail-map `float-array` of the same
  size, steering by 3 forward sensors (Jones 2010);
- couple by sampling `:u`/`:v` as wind on each agent, and/or injecting agent
  density back into `:dens0` so the colony puffs smoke as it grows.

Add it as `src/smoke/physarum.clj` reusing `f/idx` for indexing.

## References

- Stam, *Stable Fluids* (SIGGRAPH 1999); *Real-Time Fluid Dynamics for Games* (GDC 2003)
- Fedkiw, Stam & Jensen, *Visual Simulation of Smoke* (SIGGRAPH 2001)
- Jones, *Pattern Formation in Physarum Transport Networks* (Artificial Life, 2010)
