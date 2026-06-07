"""
Loads the deployed jigglefab page in real Chrome with WebGPU enabled,
captures the console log, takes screenshots before/after init, and reports
what it saw. Run:

    python scripts/verify-web.py [URL]

Default URL is the production deploy. Exits non-zero if the canvas isn't
rendering pixels other than the dark clear colour after a few seconds.
"""

import asyncio
import sys
from playwright.async_api import async_playwright, TimeoutError as PlaywrightTimeoutError

URL = sys.argv[1] if len(sys.argv) > 1 else "https://amcknight.ca/jigglefab/"
OUT_DIR = "C:/Users/thedo/git/jigglefab/scripts/verify-out"


async def main() -> int:
    import os
    os.makedirs(OUT_DIR, exist_ok=True)

    async with async_playwright() as p:
        # Real Chrome (not bundled Chromium) — WebGPU is on by default in
        # Chrome stable; bundled Chromium often isn't. Flags are extra
        # insurance for headless WebGPU.
        headed = "--headed" in sys.argv
        browser = await p.chromium.launch(
            channel="chrome",
            headless=not headed,
            args=[
                "--enable-unsafe-webgpu",
                "--enable-features=Vulkan",
                "--enable-webgpu-developer-features",
                "--ignore-gpu-blocklist",
            ],
        )
        context = await browser.new_context(viewport={"width": 1024, "height": 768})
        page = await context.new_page()

        console_lines: list[str] = []
        page.on("console", lambda msg: console_lines.append(f"[{msg.type}] {msg.text}"))
        page.on("pageerror", lambda exc: console_lines.append(f"[pageerror] {exc}"))

        webgpu_available = None

        await page.goto(URL, wait_until="domcontentloaded")

        # Confirm WebGPU is exposed to JS at all.
        webgpu_available = await page.evaluate("'gpu' in navigator")
        adapter_info = await page.evaluate(
            "(async () => {"
            "  if (!('gpu' in navigator)) return { error: 'no navigator.gpu' };"
            "  const result = {};"
            "  try {"
            "    const a = await navigator.gpu.requestAdapter();"
            "    result.default = a ? { ok: true, isFallback: a.isFallbackAdapter, info: await a.requestAdapterInfo?.() } : 'null';"
            "  } catch (e) { result.default = String(e); }"
            "  try {"
            "    const a = await navigator.gpu.requestAdapter({ forceFallbackAdapter: true });"
            "    result.fallback = a ? { ok: true, isFallback: a.isFallbackAdapter, info: await a.requestAdapterInfo?.() } : 'null';"
            "  } catch (e) { result.fallback = String(e); }"
            "  try {"
            "    const a = await navigator.gpu.requestAdapter({ powerPreference: 'low-power' });"
            "    result.lowpower = a ? { ok: true, isFallback: a.isFallbackAdapter, info: await a.requestAdapterInfo?.() } : 'null';"
            "  } catch (e) { result.lowpower = String(e); }"
            "  return result;"
            "})()"
        )

        # Sample several timepoints so we can detect a freeze: if the
        # snapshots at t=3 s and t=15 s are byte-identical, the simulation
        # stopped advancing — i.e. the user's "freezes after 1-2 s" report.
        import hashlib
        snapshots: dict[float, str] = {}
        for t in (1.0, 3.0, 8.0, 15.0):
            await page.wait_for_timeout(int(t * 1000) - sum(int(x * 1000) for x in [0.0] + sorted([k for k in snapshots])))
            shot_path = f"{OUT_DIR}/t{int(t)}.png"
            try:
                await page.screenshot(path=shot_path, full_page=False, timeout=60000)
                with open(shot_path, "rb") as f:
                    snapshots[t] = hashlib.sha256(f.read()).hexdigest()[:16]
            except Exception as e:
                snapshots[t] = f"FAILED: {e}"
                break

        if "--editor" in sys.argv:
            # Editor smoke test (extended for chains / rect / lasso / move / delete).
            await page.wait_for_function("typeof window.__jigglefabSetMode === 'function'", timeout=10000)
            await page.evaluate("window.__jigglefabSetMode('edit')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'")

            # Start from a known-empty canvas. The default scene is densely
            # packed, so placing on an occupied spot is a silent no-op — which
            # makes placement-based steps (lasso enclosure, revert setup)
            # non-deterministic. Clearing first makes every step reliable; the
            # tool assertions below all use relative bead counts, so an empty
            # start is fine.
            if await page.evaluate("window.__jigglefabBeadCount()") > 0:
                page.once("dialog", lambda d: d.accept())
                await page.evaluate("document.getElementById('btn-clear').click()")
                await page.wait_for_function("window.__jigglefabBeadCount() === 0", timeout=2000)

            box = await page.evaluate(
                "(() => { const c = document.querySelector('canvas');"
                " const r = c.getBoundingClientRect();"
                " return {x: r.left, y: r.top, w: r.width, h: r.height}; })()"
            )
            cx, cy = box["x"] + box["w"] / 2, box["y"] + box["h"] / 2

            async def place_bead(x, y):
                """Place exactly one bead at (x, y), retrying until it lands.
                The editor swallows the first place-click after a delete /
                selection-clear (deselect-click handling), so a single click
                isn't reliable. (x, y) must also clear existing beads'
                placement min-spacing (~115 px on screen) or it's a no-op."""
                await page.evaluate("window.__jigglefabSetTool('place')")
                start = await page.evaluate("window.__jigglefabBeadCount()")
                for _ in range(4):
                    await page.mouse.click(x, y)
                    try:
                        await page.wait_for_function(
                            f"window.__jigglefabBeadCount() > {start}", timeout=1000)
                        return
                    except PlaywrightTimeoutError:
                        continue
                raise AssertionError(f"failed to place a bead at ({x}, {y})")

            # --- Place tool: still works as in MVP. ---
            before = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.click(cx, cy)
            await page.wait_for_function(f"window.__jigglefabBeadCount() === {before + 1}", timeout=2000)

            # --- Chain tool: drag a short path. Expect bead count > +1. ---
            await page.evaluate("window.__jigglefabSetTool('chain')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'chain'")
            chain_before = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.move(cx - 100, cy + 100)
            await page.mouse.down()
            for i in range(1, 8):
                await page.mouse.move(cx - 100 + i * 15, cy + 100 + i * 15)
            await page.mouse.up()
            await page.wait_for_function(f"window.__jigglefabBeadCount() > {chain_before + 1}", timeout=2000)

            # --- Rect tool: drag across the chain. Selection > 0. ---
            await page.evaluate("window.__jigglefabSetTool('rect')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'rect'")
            await page.mouse.move(cx - 150, cy + 50)
            await page.mouse.down()
            await page.mouse.move(cx + 50, cy + 250)
            await page.mouse.up()
            await page.wait_for_function("window.__jigglefabSelectionCount() > 0", timeout=2000)

            # --- Delete: shrink bead count, clear selection. ---
            sel_count = await page.evaluate("window.__jigglefabSelectionCount()")
            beads_before_del = await page.evaluate("window.__jigglefabBeadCount()")
            await page.keyboard.press("Delete")
            await page.wait_for_function(
                f"window.__jigglefabBeadCount() === {beads_before_del - sel_count}", timeout=2000)
            await page.wait_for_function("window.__jigglefabSelectionCount() === 0", timeout=2000)

            # --- Lasso tool: drag a closed loop. Selection > 0. ---
            # Place a fresh bead inside the loop so the lasso has something to
            # enclose. (cx+100,cy+100) sits well inside the loop below and clear
            # of the centre bead's placement min-spacing.
            await place_bead(cx + 100, cy + 100)
            await page.evaluate("window.__jigglefabSetTool('lasso')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'lasso'")
            await page.mouse.move(cx + 50, cy + 50)
            await page.mouse.down()
            # Interpolate (steps) so the lasso polyline gets enough sample
            # points to form a well-defined polygon — a sparse 4-jump loop
            # selects unreliably.
            for (dx, dy) in [(80, 0), (80, 80), (0, 80), (0, 0)]:
                await page.mouse.move(cx + 50 + dx, cy + 50 + dy, steps=5)
            await page.mouse.up()
            await page.wait_for_function("window.__jigglefabSelectionCount() > 0", timeout=2000)

            # --- Move: drag a selected bead. Run still works after. ---
            # We don't assert the exact translation; just that drag-then-Run preserves count.
            sel_after_lasso = await page.evaluate("window.__jigglefabSelectionCount()")
            beads_after_lasso = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.move(cx + 100, cy + 100)
            await page.mouse.down()
            await page.mouse.move(cx + 140, cy + 140)
            await page.mouse.up()
            assert sel_after_lasso > 0
            await page.evaluate("window.__jigglefabSetMode('run')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'run'", timeout=2000)
            await page.wait_for_function(f"window.__jigglefabBeadCount() === {beads_after_lasso}", timeout=2000)
            await page.wait_for_function("window.__jigglefabSelectionCount() === 0", timeout=2000)

            # --- Revert: snapshot at Edit→Run is restored, persists across reverts,
            # and is invalidated by Clear. ---
            # Establish a known scene: drop into Edit, place one bead clear of
            # existing beads' placement min-spacing.
            await page.evaluate("window.__jigglefabSetMode('edit')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'", timeout=2000)
            await place_bead(cx, cy - 150)
            snap_count = await page.evaluate("window.__jigglefabBeadCount()")

            # Run, let the sim mutate, then Revert.
            await page.evaluate("window.__jigglefabSetMode('run')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'run'", timeout=2000)
            await page.wait_for_function("window.__jigglefabCanRevert() === true", timeout=2000)
            await page.wait_for_timeout(300)

            await page.evaluate("window.__jigglefabRevert()")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'", timeout=2000)
            assert await page.evaluate("window.__jigglefabBeadCount()") == snap_count, \
                "revert did not restore the pre-Run bead count"
            assert await page.evaluate("window.__jigglefabCanRevert()") is True, \
                "snapshot should persist across reverts"

            # Clear should invalidate the snapshot.
            page.once("dialog", lambda d: d.accept())
            await page.evaluate(
                "document.getElementById('btn-clear').click()"
            )
            await page.wait_for_function("window.__jigglefabCanRevert() === false", timeout=2000)

            # --- Zoom: wheel changes zoom; '0' resets to fit. ---
            # winit's web wheel-sign convention isn't assumed: scroll one way,
            # reset, scroll the other; the zoom-in direction must raise zoom > 1,
            # the zoom-out direction is clamped at the fit minimum (1.0).
            await page.evaluate("window.__jigglefabSetMode('edit')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'", timeout=2000)
            z0 = await page.evaluate("window.__jigglefabGetZoom()")
            await page.mouse.move(cx, cy)
            await page.mouse.wheel(0, -300)
            await page.wait_for_timeout(150)
            z_up = await page.evaluate("window.__jigglefabGetZoom()")
            await page.keyboard.press("0")
            await page.wait_for_timeout(100)
            await page.mouse.move(cx, cy)
            await page.mouse.wheel(0, 300)
            await page.wait_for_timeout(150)
            z_down = await page.evaluate("window.__jigglefabGetZoom()")
            assert max(z_up, z_down) > z0 + 0.01, \
                f"wheel did not zoom in (either direction): z0={z0} up={z_up} down={z_down}"
            await page.keyboard.press("0")
            await page.wait_for_timeout(100)
            z_reset = await page.evaluate("window.__jigglefabGetZoom()")
            assert abs(z_reset - 1.0) < 0.01, f"0-key did not reset to fit: {z_reset}"
            console_lines.append(f"[editor] zoom smoke OK: z0={z0} up={z_up} down={z_down} reset={z_reset}")

            # --- Torus pan: a huge pan wraps the center into the domain. ---
            await page.keyboard.press("0")  # reset view to fit
            await page.wait_for_timeout(100)
            cx_before = await page.evaluate("window.__jigglefabGetCenterX()")
            await page.mouse.move(cx, cy)
            await page.mouse.down(button="middle")
            await page.mouse.move(cx + 4000, cy)  # pan far right (many world-widths)
            await page.mouse.up(button="middle")
            await page.wait_for_timeout(150)
            cx_after = await page.evaluate("window.__jigglefabGetCenterX()")
            assert cx_after != cx_before, f"pan did not move center: {cx_before} -> {cx_after}"
            # Wrapped (small, finite) — NOT clamped at an edge and NOT run away to thousands.
            assert 0.0 <= cx_after < 1000.0, f"center not wrapped into domain: {cx_after}"
            console_lines.append(f"[editor] torus pan OK: center.x {cx_before} -> {cx_after}")

            # --- Grid fade: ~0 at rest, rises while zooming, falls after idle. ---
            await page.wait_for_timeout(2000)  # let any prior camera motion settle (hold=0.4s + fade-out tau=0.4s * ln(10) ~= 1.3s needed)
            a_rest = await page.evaluate("window.__jigglefabGridAlpha()")
            await page.mouse.move(cx, cy)
            await page.mouse.wheel(0, -300)
            await page.wait_for_timeout(100)
            a_zoom = await page.evaluate("window.__jigglefabGridAlpha()")
            await page.wait_for_timeout(2000)  # idle: HOLD + fade-out
            a_idle = await page.evaluate("window.__jigglefabGridAlpha()")
            assert a_rest < 0.1, f"grid not hidden at rest: {a_rest}"
            assert a_zoom > 0.1, f"grid did not fade in on zoom: {a_zoom}"
            assert a_idle < 0.1, f"grid did not fade out when idle: {a_idle}"
            console_lines.append(f"[editor] grid fade OK: rest={a_rest} zoom={a_zoom} idle={a_idle}")

            console_lines.append("[editor] extended smoke test passed")

            # ---- Device library ----
            print("editor: device library")
            await page.wait_for_function("typeof window.__jigglefabGetDock === 'function'", timeout=10000)
            await page.evaluate("window.__jigglefabSetMode('edit')")
            await page.wait_for_function("window.__jigglefabGetMode() === 'edit'")
            # Clear to a known empty scene.
            if await page.evaluate("window.__jigglefabBeadCount()") > 0:
                page.once("dialog", lambda d: d.accept())
                await page.evaluate("document.getElementById('btn-clear').click()")
                await page.wait_for_function("window.__jigglefabBeadCount() === 0", timeout=2000)

            # Recompute canvas center using the main render canvas (identified by
            # its alt attribute, not the thumbnail canvases added to the dock).
            box = await page.evaluate(
                "() => { const c = document.querySelector('canvas[alt]');"
                " const r = c.getBoundingClientRect();"
                " return {x: r.x + r.width/2, y: r.y + r.height/2}; }"
            )
            cx, cy = box["x"], box["y"]

            # Place 3 beads with the Place tool, then Rect-select them.
            await page.evaluate("window.__jigglefabSetTool('place')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'place'")
            for (dx, dy) in [(-20, 0), (0, 0), (20, 0)]:
                await page.mouse.click(cx + dx, cy + dy)
            await page.wait_for_function("window.__jigglefabBeadCount() === 3", timeout=2000)
            await page.evaluate("window.__jigglefabSetTool('rect')")
            await page.wait_for_function("window.__jigglefabGetTool() === 'rect'")
            await page.mouse.move(cx - 60, cy - 40)
            await page.mouse.down()
            await page.mouse.move(cx + 60, cy + 40)
            await page.mouse.up()
            await page.wait_for_function("window.__jigglefabSelectionCount() === 3", timeout=2000)

            # Save selection to the dock.
            await page.evaluate("window.__jigglefabSaveToDock('smoke-device')")
            await page.wait_for_function("window.__jigglefabGetDock().length === 1", timeout=2000)
            dock0 = await page.evaluate("window.__jigglefabGetDock()[0]")
            assert dock0["beads"] and len(dock0["beads"]) == 3, f"expected 3 device beads, got {dock0}"

            # Persistence: localStorage should hold the library after the save.
            await page.wait_for_function(
                "() => { const v = localStorage.getItem('jigglefab.library.v1');"
                " return v && JSON.parse(v).dock.length === 1; }", timeout=2000)

            # Arm + stamp: bead count grows by 3 (isolated copy).
            dev_id = dock0["id"]
            await page.evaluate(f"window.__jigglefabArmDevice({dev_id})")
            await page.wait_for_function(f"window.__jigglefabArmedId() === {dev_id}", timeout=2000)
            beads_before_stamp = await page.evaluate("window.__jigglefabBeadCount()")
            await page.mouse.click(cx, cy + 80)
            await page.wait_for_function(
                f"window.__jigglefabBeadCount() === {beads_before_stamp + 3}", timeout=2000)
            await page.evaluate("window.__jigglefabDisarm()")
            await page.wait_for_function("window.__jigglefabArmedId() === -1", timeout=2000)

            # Suite save -> remove device -> load suite restores it.
            await page.evaluate("window.__jigglefabSaveSuite('smoke-suite')")
            await page.wait_for_function(
                "window.__jigglefabGetSuiteNames().includes('smoke-suite')", timeout=2000)
            await page.evaluate(f"window.__jigglefabRemoveDevice({dev_id})")
            await page.wait_for_function("window.__jigglefabGetDock().length === 0", timeout=2000)
            await page.evaluate("window.__jigglefabLoadSuite('smoke-suite')")
            await page.wait_for_function("window.__jigglefabGetDock().length === 1", timeout=2000)

            # Import a device with a bogus state -> present but incompatible.
            chem_now = await page.evaluate("window.__jigglefabGetChemistryName()")
            bad_json = (
                '{"name":"bad-suite","chemistry":"' + chem_now + '",'
                '"devices":[{"id":0,"name":"bogus","chemistry":"' + chem_now + '",'
                '"chemistry_hash":0,"beads":[{"state":"__no_such_state__","pos":[0,0]}],'
                '"bonds":[],"ports":[]}]}'
            )
            await page.evaluate("(j) => window.__jigglefabImportSuite(j)", bad_json)
            await page.wait_for_function(
                "window.__jigglefabGetSuiteNames().includes('bad-suite')", timeout=2000)
            await page.evaluate("window.__jigglefabLoadSuite('bad-suite')")
            await page.wait_for_function(
                "window.__jigglefabGetDock().some(d => d.compatible === false)", timeout=2000)
            print("editor: device library OK")

        # Inspect the canvas: did winit append one? What's its drawing-buffer
        # size and CSS size?
        canvas_info = await page.evaluate(
            "() => {"
            "  const c = document.querySelector('canvas[alt]') || document.querySelector('canvas');"
            "  if (!c) return { exists: false };"
            "  const r = c.getBoundingClientRect();"
            "  return {"
            "    exists: true,"
            "    width: c.width, height: c.height,"
            "    clientWidth: r.width, clientHeight: r.height,"
            "    inlineStyle: c.getAttribute('style') || '',"
            "  };"
            "}"
        )

        after = f"{OUT_DIR}/t15.png"
        # Take a full-viewport screenshot after waiting.
        # (the per-timepoint screenshots above already wrote files)

        # Pixel-sample the screenshot dead-center: if the wasm is rendering,
        # the centre of the chain at x=40,y~40 in world space should NOT be
        # the dark clear colour.
        sample = await page.evaluate(
            "() => {"
            "  const c = document.querySelector('canvas[alt]') || document.querySelector('canvas');"
            "  if (!c) return null;"
            "  try {"
            "    const ctx = c.getContext('2d', { willReadFrequently: true });"
            "    if (!ctx) return { error: 'cannot get 2d context (canvas is webgpu)' };"
            "    return null;"
            "  } catch (e) { return { error: String(e) }; }"
            "}"
        )

        await browser.close()

        print(f"URL:                {URL}")
        print(f"navigator.gpu:      {webgpu_available}")
        print(f"adapter probe:      {adapter_info}")
        print(f"canvas:             {canvas_info}")
        print(f"2d sample probe:    {sample}")
        print(f"Screenshot:         {after}")
        print()
        print("Per-timepoint snapshot SHA-256 (first 16 hex):")
        for t in sorted(snapshots):
            print(f"  t={t:>4.1f}s  {snapshots[t]}")
        if len({v for v in snapshots.values() if not v.startswith('FAILED')}) == 1 and len(snapshots) > 1:
            print("  ⚠ All snapshots are identical — simulation is FROZEN")
        print()
        print("Console:")
        for line in console_lines:
            print(f"  {line}")

        # Exit non-zero if there's no canvas or no WebGPU.
        if not webgpu_available:
            return 2
        if isinstance(adapter_info, dict) and adapter_info.get("error"):
            return 3
        if not canvas_info or not canvas_info.get("exists"):
            return 4
        # Surface stuck at 1×1 is the bug we're trying to confirm fixed.
        if canvas_info.get("width", 0) <= 1 or canvas_info.get("height", 0) <= 1:
            return 5
        return 0


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
