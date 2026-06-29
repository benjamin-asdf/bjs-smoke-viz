# Publishing strategy — smoke-viz as a tool

Goal: get this in front of people (esp. German music artists) and, eventually, on
really large screens. Two product modes were proposed: (1) live real-time picture,
(2) "render like this" → output video.

## The fact that decides the architecture

The two modes have *opposite* hosting economics:

- **Live / real-time** — cannot run on a server for free users. Real-time rendering
  needs one machine (or GPU) dedicated per active viewer; you can't time-share it.
  ~10 concurrent users ≈ 10 always-on GPUs ≈ €3–5k/month. A donate button will never
  cover that. **Real-time must run on the user's own machine** (desktop app, or a
  client-side browser WebGL rewrite).

- **"Render like this" → video file** — ideal for a server. It's a batch job: one
  machine drains a queue, one render at a time. A 4-min track is cents of compute.
  And `smoke.video` already does exactly this, on CPU — **no server GPU needed.**

So the two modes are not equal-effort siblings: the video service reuses code that
already exists; the live mode is a rewrite or a desktop-packaging job.

## Phased plan

### Phase 0 — validate before building (now)
Use the showcase videos that already exist (`aldebara.mp4`, `alicante-preview.mp4`, …).
Pick 2–3 German artists, render their track *for* them as a free gift, send the
finished video. Zero infrastructure. If a few say "can I use this for my release /
live show," there's a product. If nobody bites, months are saved.

### Phase 1 — render-to-video web service (first product)
Nearly free to build because it wraps existing code:
- **Frontend**: upload audio + expose the controls as render settings + "render"
  button → email / download link.
- **Backend**: a job queue running the *current* `smoke.video` renderer unchanged.
  CPU batch, no GPU.
- **Donate**: Ko-fi / Stripe / PayPal button — trivial.
- **Artist outreach**: just a URL. "Upload your track, get a video."

### Phase 2 — live VJ tool (the "huge screen" vision)
Real-time → ships as a **desktop app**, not browser, not server. Steam has this exact
category (Synesthesia, Magic Music Visuals, plane9, MilkDrop-likes) selling to the
VJ / live-visuals crowd. Package the Clojure app (jpackage / native bundle) to keep
existing code. A client-side WebGL browser port reaches more people but is a full
shader rewrite of the solver — defer until Phase 1 proves demand.

## Answers to the open questions

- **GPU on server?** Only for the offline video path, and even there the renderer is
  CPU, so no. Never for live.
- **Complete browser version?** Only feasible for live mode as a client-side WebGL
  rewrite (big effort, defer). The video service needs only a thin browser frontend
  over the existing backend.
- **Steam vs browser?** Both, different modes: Steam = real-time VJ app (Phase 2),
  web = render service (Phase 1). Not either/or.
- **Reaching artists?** The free-finished-video gift (Phase 0) converts far better
  than handing over a tool to learn. Lead with a result, not a UI.

## Next step
Phase 0 now with existing videos. In parallel, scaffold Phase 1: wrap `smoke.video`
in a job queue + a small upload/settings frontend. Most of the work is plumbing, not
new rendering code.
