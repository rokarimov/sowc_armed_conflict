# CLAUDE.md

## Project Overview

This is an R-based research project for UNICEF's **State of the World's Children (SOWC) 2026** report, analyzing the relationship between armed conflict and child/maternal health outcomes.

## Running Scripts

This is an RStudio project — scripts are run interactively or sourced from the project root.

All file paths use `here::here()`, so scripts must be run from the project root (where `2026 SOWC Conflict.Rproj` lives).

## Style

Function names are in CamelCase.

Do not use `map_dfr()` / `map_dfc()` (or their `map2_`, `imap_`, `pmap_` variants) — these were superseded in purrr 1.0.0. Use `map(...) |> list_rbind()` or `map(...) |> list_cbind()` instead.

Do not us `transmute` - it was superseded. Instead, use `mutate(..., .keep = "none")`.

Do not factor code into helpers unless the code will reoccur 4 or more times. Avoid using nested helpers where possible. It is best to keep code inline for ease of human verification.

## Terminology

Do not use "dose-response" (or "dose-response relationship"). Our exposures are ordered
bands — conflict deaths, years of consecutive conflict — not titrated doses, and the
pharmacological metaphor does not suit a policy audience. Do not use "intensity gradient"
either.

Use **"effect by conflict intensity"**, in prose as well as in table and figure headings.
"Graded by conflict severity" works where a verb form reads better; "exposure-response" is
acceptable where epidemiological convention is expected.

This applies to variable names, comments, table headers, figure captions and write-ups alike.
Note that Wagner et al. 2018 use "dose response" in their own appendix (§2.6) — paraphrase
rather than quote that sentence.
