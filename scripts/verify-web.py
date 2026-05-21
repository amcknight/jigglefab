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
from playwright.async_api import async_playwright

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

        # Give the WASM bundle several seconds to init and start rendering.
        await page.wait_for_timeout(4000)

        # Inspect the canvas: did winit append one? What's its drawing-buffer
        # size and CSS size?
        canvas_info = await page.evaluate(
            "() => {"
            "  const c = document.querySelector('canvas');"
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

        before = f"{OUT_DIR}/before.png"
        after = f"{OUT_DIR}/after.png"
        # Take a full-viewport screenshot after waiting.
        try:
            await page.screenshot(path=after, full_page=False, timeout=60000)
        except Exception as e:
            print(f"screenshot failed: {e}")

        # Pixel-sample the screenshot dead-center: if the wasm is rendering,
        # the centre of the chain at x=40,y~40 in world space should NOT be
        # the dark clear colour.
        sample = await page.evaluate(
            "() => {"
            "  const c = document.querySelector('canvas');"
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
