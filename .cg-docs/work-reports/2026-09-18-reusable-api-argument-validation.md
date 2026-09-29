---
plan: ".cg-docs/plans/2026-09-18-reusable-api-argument-validation.md"
created: "2026-09-18"
status: active
active-deviation-policy: "ask"
---

# Execution Report: Reusable API Argument Validation

## Plan Reference
- Plan: `.cg-docs/plans/2026-09-18-reusable-api-argument-validation.md`
- Active deviation policy: `ask`

## Run 1 (2026-09-18)

### Completed Steps
- Step 1 (Refactor shared validators in `utils.R`) -- Done. `validate_get_stats_args()` split into focused validators; all 93 `test-get_stats.R` tests pass.
- Step 2 (Add composer for `get_cp()`) -- Done. `validate_get_cp_args()` added and wired; 18 tests pass. One pre-existing network failure (`povline and ppp_version` test) confirmed failing on original code too -- unrelated to this change.
- Step 3 (Add composer for `get_cp_ki()`) -- Done. `validate_get_cp_ki_args()` added and wired; 19 tests pass. One pre-existing HTTP 500 failure (`povline and ppp_version` test) confirmed failing on original code too -- unrelated to this change.
- Step 4 (Add composer for `get_aux()`) -- Done. `validate_get_aux_args()` added and wired; 25 tests pass.
- Step 5 (Add offline validator tests) -- Done. Added direct tests for all shared validators in `test-utils.R`; 159 tests pass.

### Deviations
- None

### Accepted Exceptions
- **V4** (user-approved, 2026-09-21): The 2 full-suite failures (`test-get_cp.R:47`, `test-get_cp_ki.R:49`) are pre-existing on `main` — the user verified the same tests fail on the main branch. They are PIP API-side errors for `ppp_version = 2011` requests (HTTP 500 / non-JSON body), unrelated to this refactor. All validation tests pass. Exception accepted by user; V4 treated as satisfied with no regressions.

### Evidence Table
| ID | Evidence Required | Status | Artifact |
|----|-------------------|--------|----------|
| V1 | `validate_get_stats_args()` passes existing `test-get_stats.R` tests | passed | 93 pass, 1 pre-existing skip |
| V2 | New shared validators pass offline tests in `test-utils.R` | passed | 165 pass, 3 pre-existing skips |
| V3 | `get_cp()`, `get_cp_ki()`, `get_aux()` reject malformed shared args before HTTP | passed | 18/19/25 pass; 2 pre-existing network failures |
| V4 | Full suite passes with no regressions | passed (accepted exception) | 2 failures confirmed pre-existing on main; no regressions from this work |
| V5 | `get_stats(release_version = "2024-06-27")` aborts locally with `YYYYMMDD` | passed | validation test confirms local abort |

### Constraints Check
| ID | Constraint | Status |
|----|------------|--------|
| C1 | Valid calls preserve existing behavior and defaults | passed |
| C2 | `get_cp_ki()` keeps required single-country rule | passed |
| C3 | `get_gd()` behavior is unchanged | passed (file not modified) |
| C4 | Version identity suffix not hard-coded to `PROD` | passed |

### Remaining Uncertainty
- None. V4 resolved via user-approved accepted exception (failures confirmed pre-existing on main).

### Final Status
- completed