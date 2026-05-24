"""
Boots each demo size via the URL hash chooser, measures sustained FPS over
3 seconds with requestAnimationFrame timing, reports a row per size. Use
against trunk serve or the live deploy.

    python scripts/measure-fps.py [BASE_URL]
"""

import asyncio
import sys
from playwright.async_api import async_playwright

BASE = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8765/"
SIZES = [
    "wire-20x30", "wire-40x30", "wire-60x30", "wire-80x30", "wire-100x30",
    "wire-100x30x10",
]


async def measure_one(p, size: str) -> dict:
    browser = await p.chromium.launch(
        channel="chrome",
        headless=True,
        args=[
            "--enable-unsafe-webgpu",
            "--enable-features=Vulkan",
            "--enable-webgpu-developer-features",
            "--ignore-gpu-blocklist",
        ],
    )
    context = await browser.new_context(viewport={"width": 800, "height": 800})
    page = await context.new_page()
    await page.goto(f"{BASE}#{size}", wait_until="domcontentloaded")
    # Warm up — wait for renderer to be ready and a few frames to draw.
    await page.wait_for_timeout(2000)
    result = await page.evaluate(
        """
        () => new Promise(resolve => {
            const frames = [];
            let start = performance.now();
            function tick(t) {
                frames.push(t);
                if (t - start < 3000) {
                    requestAnimationFrame(tick);
                } else {
                    const dts = [];
                    for (let i = 1; i < frames.length; i++) dts.push(frames[i] - frames[i-1]);
                    dts.sort((a, b) => a - b);
                    const mean = dts.reduce((s, x) => s + x, 0) / dts.length;
                    const p50 = dts[Math.floor(dts.length * 0.5)];
                    const p95 = dts[Math.floor(dts.length * 0.95)];
                    const p99 = dts[Math.floor(dts.length * 0.99)];
                    resolve({ frames: frames.length, mean_ms: mean, p50_ms: p50, p95_ms: p95, p99_ms: p99 });
                }
            }
            requestAnimationFrame(tick);
        })
        """
    )
    await browser.close()
    return result


async def main() -> int:
    async with async_playwright() as p:
        print(f"BASE URL: {BASE}")
        print(f"{'size':<14}{'frames':>8}{'mean ms':>10}{'p50 ms':>10}{'p95 ms':>10}{'p99 ms':>10}{'fps (mean)':>14}")
        for size in SIZES:
            r = await measure_one(p, size)
            fps = 1000.0 / r['mean_ms'] if r['mean_ms'] > 0 else 0
            print(
                f"{size:<14}{r['frames']:>8}{r['mean_ms']:>10.2f}{r['p50_ms']:>10.2f}"
                f"{r['p95_ms']:>10.2f}{r['p99_ms']:>10.2f}{fps:>14.1f}"
            )
    return 0


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
