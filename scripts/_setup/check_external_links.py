#!/usr/bin/env python3
"""Auditoria não destrutiva de URLs externos nos documentos Markdown.

Somente HTTP 404/410 confirmado também por GET é considerado quebrado.
HTTP 401/403/429, timeout, TLS e falhas temporárias são inconclusivos.
"""
from __future__ import annotations

import concurrent.futures
import os
from pathlib import Path
import re
import sys
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

ROOT = Path(__file__).resolve().parents[2]
URL = re.compile(r"https?://[^\s<>()\"'`]+", re.I)
HEADERS = {
    "User-Agent": "bayes-stan-r-link-audit/1.0 (+https://github.com/riscotta/bayes-stan-r)",
    "Accept": "text/html,application/pdf,*/*;q=0.5",
}


def discover() -> dict[str, list[str]]:
    found: dict[str, list[str]] = {}
    for path in sorted(ROOT.rglob("*")):
        if path.suffix.lower() not in {".md", ".qmd"}:
            continue
        if any(part in {".git", "renv"} for part in path.relative_to(ROOT).parts):
            continue
        for number, line in enumerate(path.read_text(encoding="utf-8-sig").splitlines(), 1):
            for item in URL.findall(line):
                url = item.rstrip(".,;:!?]}\\")
                found.setdefault(url, []).append(f"{path.relative_to(ROOT)}:{number}")
    return found


def request(url: str, method: str) -> int | str:
    try:
        with urlopen(Request(url, headers=HEADERS, method=method), timeout=14) as response:
            return response.status
    except HTTPError as e:
        return e.code
    except (URLError, TimeoutError, OSError, ValueError) as e:
        return type(e).__name__


def probe(url: str) -> tuple[str, str, str]:
    status = request(url, "HEAD")
    # Verificar novamente com GET antes de declarar um 404 real.
    if status in (404, 410, 405, 501):
        status = request(url, "GET")
    if isinstance(status, int) and 200 <= status < 400:
        return url, "OK", str(status)
    if status in (404, 410):
        return url, "BROKEN", str(status)
    return url, "INCONCLUSIVE", str(status)


def main() -> int:
    links = discover()
    print(f"URLs externos únicos encontrados: {len(links)}")
    with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
        results = sorted(pool.map(probe, links), key=lambda x: (x[1], x[0]))

    broken = [result for result in results if result[1] == "BROKEN"]
    uncertain = [result for result in results if result[1] == "INCONCLUSIVE"]

    rows = ["# Verificação semanal de URLs externos", "",
            f"Total: {len(results)} · 404/410 confirmados: {len(broken)} · "
            f"Inconclusivos: {len(uncertain)}", ""]
    for url, status, detail in results:
        if status == "OK":
            continue
        source = ", ".join(links[url][:3])
        rows.append(f"- **{status} ({detail})** · `{url}` · {source}")
    if not broken and not uncertain:
        rows.append("Nenhuma falha detectada.")
    report = "\n".join(rows) + "\n"
    print(report)
    if os.getenv("GITHUB_STEP_SUMMARY"):
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a", encoding="utf-8") as out:
            out.write(report)
    # Não contar bloqueio de bot, timeout ou rate limiting como link quebrado.
    return 1 if broken else 0


if __name__ == "__main__":
    sys.exit(main())
