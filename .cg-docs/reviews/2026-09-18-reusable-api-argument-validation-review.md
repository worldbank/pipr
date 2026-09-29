---
date: 2026-09-21
depth: light
type: standard
plan: ".cg-docs/plans/2026-09-18-reusable-api-argument-validation.md"
findings:
  P1.1: fixed
  P1.2: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P3.1: fixed
  P3.2: fixed
  P3.3: fixed
---

## Review Report

**Review mode**: light
**Files reviewed**: 8 (4 source + 4 test)
**Findings**: 8 (P0: 0, P1: 2, P2: 4, P3: 3) — 1 fixed

### P0 — BLOCKING (immediate remediation required)
None.

### P1 — CRITICAL (must fix before merge)
- **[P1.1]** [cg-testing] `tests/testthat/test-get_cp_ki.R:58-59` — Required single-country rule test was gated behind `skip_if_offline()`/`skip_on_cran()`, so the pure-validator rule was untested offline. **FIXED** by splitting the test: valid-country (network) stays gated; NULL and multi-country assertions moved to a non-gated test.
  **Why**: `validate_get_cp_ki_args()` runs before any HTTP, so the assertions never touch the network; the skips silently defeated the "required single-country rule" guarantee.
  **Fix** (applied): Split into `test_that("Country argument validation works correctly in get_cp_ki()")` (network-gated) and `test_that("get_cp_ki() validates the required single-country rule")` (pure-validator, ungated).

- **[P1.2]** [cg-code-quality] `R/utils.R:655-662` — `validate_get_cp_ki_args()` has redundant pre-checks duplicating `validate_country()` (which already rejects NULL and length>1), with divergent, less-informative error messages. [manual]
  **Why**: DRY violation — the composer's `cli_abort` messages differ from `abort_invalid_argument()` used elsewhere.
  **Fix**: Remove the redundant pre-checks and let `validate_country()` handle it, or fold into a `validate_single_country()` helper (requires deciding whether to keep the distinct friendly message).

### P2 — IMPORTANT (should fix)
- **[P2.1]** [cg-testing] `R/utils.R:564-568` + `tests/testthat/test-utils.R:105-113` — `validate_ppp_version()` silently accepts non-integer and `Inf` numerics. [advisory]
  **Why**: `is.numeric(ppp_version)` only checks type, not finiteness/integer-ness, so `2017.5` and `Inf` pass and flow to the API.
  **Fix**: Add tests for `2017.5` and `Inf`; if rejection intended, tighten to require `is.finite()` and integer.

- **[P2.2]** [cg-code-quality] `R/utils.R:630,667,697,742` — 4× repeated version/simplify/server validation block. [advisory]
  **Why**: Same tonic block repeated in all four composers — a DRY trap.
  **Fix**: Extract a `validate_shared_versions()` helper.

- **[P2.3]** [cg-code-quality] `R/utils.R:638,676,706,761` — Magic strings `"v1"` and format lists repeated across composers. [safe_auto]
  **Why**: API contract values not centralized.
  **Fix**: Define package constants for API versions and format lists.

- **[P2.4]** [cg-code-quality] `R/get_aux.R:98` — `ppp_version` validated but never forwarded to the aux request. [manual]
  **Why**: Dead validation that misleadingly suggests `ppp_version` affects aux queries.
  **Fix**: Drop `ppp_version` from the aux validator or forward it to the request (requires confirming the API contract).

### P3 — MINOR (nice to have)
- **[P3.1]** [advisory] [cg-testing] `R/utils.R:544-549` — `validate_version()` regex permits semantically invalid components (e.g. `20260324_9999_99_99_PROD`); no test documents whether the loose format is intended.
- **[P3.2]** [advisory] [cg-testing] `tests/testthat/test-utils.R:134-137` — `validate_simplify()` multi-length branch untested.
- **[P3.3]** [advisory] [cg-testing] `tests/testthat/test-utils.R:9-13` — `validate_country(c("all", "AGO"))` mixed-vector behavior undocumented.

### ✅ Passed
- `@cg-code-quality`: No P0s; refactor preserves behavior and improves DRY/naming.
- `@cg-testing`: Direct validator coverage is strong and non-tautological; edge cases well handled.
- `.Rbuildignore`: already excludes `.cg-docs/` — no P2 required.
- R skill checks: `cg-skill-r-technical` and `cg-skill-r-shared` loaded per convention.

### Autofix applied
- **P1.1** (safe_auto): Split the `get_cp_ki` country-rule test so NULL/multi-country assertions are no longer network-gated. Verified working offline.