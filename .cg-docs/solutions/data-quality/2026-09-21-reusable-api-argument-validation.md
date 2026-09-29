---
date: 2026-09-21
title: "Reusable API argument validation with per-client composers"
category: "data-quality"
language: "R"
tags: [validation, api-client, cli-abort, vector-coercion, testthat, dry, refactoring]
root-cause: "Monolithic per-function validation duplicated logic across API clients and let malformed arguments reach the API as HTTP 404/500 errors"
severity: "P2"
---

# Reusable API argument validation with per-client composers

## Problem
The `pipr` package's API client functions (`get_stats()`, `get_cp()`, `get_cp_ki()`, `get_aux()`) validated arguments inconsistently. Only `get_stats()` had a dedicated validator; the others used scattered inline checks or none at all. Malformed arguments (e.g. `release_version = "2024-06-27"` instead of `"20240627"`) were forwarded to the PIP API, which returned a generic HTTP 404 "Invalid query arguments" — giving the user no clue which argument was wrong or what format was expected.

## Root Cause
Validation logic was duplicated or missing across five client functions, and there was no local boundary between argument checking and HTTP request construction. Each function rolled its own error messages (`match.arg()` defaults, raw `cli_abort()`, or nothing), so the same class of error produced different messages in different functions.

## Solution
Split the monolithic `validate_get_stats_args()` into focused, single-concern validators in `R/utils.R`, each returning its validated value and aborting via a shared `abort_invalid_argument()` helper:

```r
abort_invalid_argument <- function(argument, expected) {
  cli::cli_abort(c(
    "Invalid `{argument}`.",
    "i" = "`{argument}` must be {expected}."
  ))
}

validate_release_version <- function(release_version) {
  is_valid <- is.character(release_version) &&
    length(release_version) == 1 &&
    !is.na(release_version) &&
    grepl("^[0-9]{8}$", release_version) &&
    !is.na(as.Date(release_version, format = "%Y%m%d"))
  if (!is.null(release_version) && !is_valid) {
    abort_invalid_argument("release_version", "a date in YYYYMMDD format")
  }
  release_version
}
```

Then compose only the relevant validators per client via thin composer functions:

```r
validate_get_cp_args <- function(country, povline, version, ppp_version,
                                 release_version, api_version, format,
                                 simplify, server) {
  validate_country(country)
  validate_povline(povline)
  validate_shared_versions(version, ppp_version, release_version, simplify, server)
  list(
    api_version = match_choice(api_version, "api_version", PIP_API_VERSIONS),
    format = match_choice(format, "format", PIP_CP_FORMATS)
  )
}
```

Each `get_*()` calls its composer before any request construction, so malformed input aborts locally with a message naming the argument and its expected format.

### Key gotcha: vector coercion in validators

When tightening `validate_ppp_version()` to reject non-integer/`Inf` numerics, the first attempt introduced a new bug:

```r
# BROKEN: is.finite(c(2017, 2011)) returns length-2; && requires length 1
is_valid_ppp_version <- is.numeric(ppp_version) &&
  is.finite(ppp_version) &&
  ppp_version == trunc(ppp_version)
```

`is.finite()` is vectorized, so for a length-2 input `&&` errored with
`'length = 2' in coercion to 'logical(1)'` instead of the intended clean
`abort_invalid_argument()`. The fix is to guard length **before** the
element-wise checks:

```r
# FIXED: length guard first, so && always sees scalars
is_valid_ppp_version <- length(ppp_version) == 1 &&
  ((is.numeric(ppp_version) &&
    is.finite(ppp_version) &&
    ppp_version == trunc(ppp_version)) ||
    (is.character(ppp_version) && grepl("^[0-9]{4}$", ppp_version)))
```

The new test `expect_error(validate_ppp_version(c(2017, 2011)), "ppp_version")` caught this immediately — a direct payoff of writing rejection tests for every validator.

### Verification pattern for pre-existing test failures
When a full-suite run shows failures in tests you didn't write, verify they are pre-existing before treating them as regressions: `git stash` your changes, re-run the failing test file, and compare. In this work, two `ppp_version = 2011` tests failed identically on unmodified code (PIP API returns HTTP 500 / non-JSON for those requests), and a direct API probe showed even `ppp_version = 2017` returned HTTP 504 — confirming transient API unavailability rather than a code regression.

## Prevention
- **Validate before HTTP**: every API client should reject malformed arguments locally with a message naming the argument and expected format — never let the API be the first validator.
- **One validator per contract**: small `@noRd` validators composed per client beat monolithic all-args functions; composers apply only the checks each function actually accepts.
- **Guard length before element-wise checks**: in R validators, `length(x) == 1 &&` must precede any `is.finite()`/comparison chain, or multi-value inputs crash with a coercion error instead of a clean abort.
- **Write rejection tests for every validator branch** — including multi-value vectors, `Inf`, and `NA` — not just happy paths; they catch coercion bugs the source review misses.
- **Don't network-gate pure-validator tests**: assertions that run before HTTP need no `skip_if_offline()`/`skip_on_cran()`; gating them silently removes offline/CI coverage.
- **Centralize API contract values** (`PIP_API_VERSIONS`, format lists) as package constants to avoid magic-string drift between signatures and validators.

## Related
- Plan: `.cg-docs/plans/2026-09-18-reusable-api-argument-validation.md`
- Review: `.cg-docs/reviews/2026-09-18-reusable-api-argument-validation-review.md`
- Execution report: `.cg-docs/work-reports/2026-09-18-reusable-api-argument-validation.md`
- No prior solutions existed; this is the first entry in the knowledge base.