---
date: 2026-09-18
title: "Reusable API argument validation"
status: active
scope: "Standard"
brainstorm: null
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
execution-report: ".cg-docs/work-reports/2026-09-18-reusable-api-argument-validation.md"
tags: [validation, refactor, api-client, utils]
---

# Plan: Reusable API Argument Validation

## Objective
Refactor the current `validate_get_stats_args()` implementation into focused, reusable validators in `R/utils.R`, then compose the applicable shared checks in `get_stats()`, `get_cp()`, `get_cp_ki()`, and `get_aux()`. Malformed arguments abort locally with a clear `cli` message instead of producing an HTTP 404.

## Context
The `pipr` package exposes five API client functions. Only `get_stats()` has a dedicated validator (`validate_get_stats_args()`); the others use scattered inline checks. Shared arguments (`version`, `ppp_version`, `release_version`, `api_version`, `format`, `simplify`, `server`, `country`, `povline`) are validated inconsistently or not at all. This refactor centralizes shared validation while preserving each function's specific behavior.

## Requirements
| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Split `validate_get_stats_args()` into focused, reusable validators | User request |
| R2 | `get_stats()` behavior and error messages remain unchanged | Backward-compatibility |
| R3 | `get_cp()`, `get_cp_ki()`, `get_aux()` validate shared args before HTTP | User request |
| R4 | `get_cp_ki()` keeps required single-country rule | Existing behavior |
| R5 | `get_gd()` behavior is unchanged | Out of scope |
| R6 | Version identity suffix not hard-coded to `PROD` | `get_versions()` authority |
| R7 | Add offline tests for new validators | Testing convention |

## Implementation Steps

### 1. Refactor shared validators in `utils.R`
- **Requirements**: R1, R2, R5, R6
- **Files**: `R/utils.R`
- **Details**: Split `validate_get_stats_args()` into top-level `@noRd` validators for shared contracts: `validate_country()`, `validate_year()`, `validate_povline()`, `validate_popshare()`, `validate_fill_gaps()`, `validate_nowcast()`, `validate_subgroup()`, `validate_welfare_type()`, `validate_reporting_level()`, `validate_version()`, `validate_ppp_version()`, `validate_release_version()`, `validate_api_version()`, `validate_format()`, `validate_simplify()`, `validate_server()`. Each returns its validated/matched value and uses `abort_invalid_argument()`. Keep `validate_get_stats_args()` as a thin composer calling these, preserving its exact return shape.
- **Test Scenarios**: happy path (valid values pass), edge case (NULL optional args), error path (each invalid value aborts)
- **Tests**: `tests/testthat/test-utils.R`
- **Acceptance criteria**: `validate_get_stats_args()` returns identical results; all existing `test-get_stats.R` tests pass

### 2. Add composer for `get_cp()`
- **Requirements**: R3
- **Files**: `R/utils.R`, `R/get_cp.R`
- **Details**: Add `validate_get_cp_args()` composing `validate_country()`, `validate_povline()`, `validate_version()`, `validate_ppp_version()`, `validate_release_version()`, `validate_api_version()`, `validate_format()`, `validate_simplify()`, `validate_server()`. Wire into `get_cp()` before the povline conditional and request construction.
- **Test Scenarios**: happy path, malformed shared args abort before HTTP
- **Tests**: `tests/testthat/test-get_cp.R`
- **Acceptance criteria**: `get_cp()` rejects malformed shared args locally

### 3. Add composer for `get_cp_ki()`
- **Requirements**: R3, R4
- **Files**: `R/utils.R`, `R/get_cp_ki.R`
- **Details**: Add `validate_get_cp_ki_args()` composing the shared validators (no `format` — not a parameter). Preserve the required single-country rule as a function-specific check.
- **Test Scenarios**: happy path, NULL country, multi-country, malformed shared args
- **Tests**: `tests/testthat/test-get_cp_ki.R`
- **Acceptance criteria**: `get_cp_ki()` rejects NULL/multi-country and malformed shared args

### 4. Add composer for `get_aux()`
- **Requirements**: R3
- **Files**: `R/utils.R`, `R/get_aux.R`
- **Details**: Add `validate_get_aux_args()` composing `validate_version()`, `validate_ppp_version()`, `validate_release_version()`, `validate_api_version()`, `validate_format()`, `validate_simplify()`, `validate_server()`. Preserve `table`, `assign_tb`, `replace` semantics.
- **Test Scenarios**: happy path, malformed shared args abort before HTTP
- **Tests**: `tests/testthat/test-get_aux.R`
- **Acceptance criteria**: `get_aux()` rejects malformed shared args locally

### 5. Add offline validator tests
- **Requirements**: R7
- **Files**: `tests/testthat/test-utils.R`
- **Details**: Add direct tests for each shared validator covering valid and invalid values, including version/date format edge cases.
- **Test Scenarios**: valid values pass, invalid values abort with correct message
- **Tests**: `tests/testthat/test-utils.R`
- **Acceptance criteria**: All new validator tests pass offline

## Testing Strategy
- Offline unit tests for each validator in `test-utils.R`
- Client-level rejection tests in `test-get_cp.R`, `test-get_cp_ki.R`, `test-get_aux.R`
- Existing `test-get_stats.R` tests must pass unchanged
- Full suite via `devtools::test()`

## Documentation Checklist
- No roxygen changes needed (validators are `@noRd` internal helpers)
- No user-facing documentation changes

## Risks & Mitigations
| Risk | Mitigation |
|------|------------|
| Behavior change in `get_stats()` | Keep `validate_get_stats_args()` as thin composer with identical return shape |
| `get_cp_ki()` country rule regression | Preserve required single-country check explicitly |
| Version format over-restriction | Validate structural format only; don't hard-code `PROD` |
| Test suite regression | Run each client's tests after each wiring step |

## Out of Scope
- `get_gd()` validation refactor (R5)
- New validation rules beyond current contracts
- Behavior changes to valid requests
- Documentation updates

## Completion Contract

### Outcome
The shared API argument validation in `R/utils.R` is refactored into focused, reusable validators. `get_stats()`, `get_cp()`, `get_cp_ki()`, and `get_aux()` validate their arguments locally before any HTTP request, with consistent `cli` error messages. Malformed inputs abort with a clear message instead of producing an HTTP 404.

### Verification Surface
| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | `validate_get_stats_args()` still passes all existing `test-get_stats.R` tests | `testthat::test_file("tests/testthat/test-get_stats.R")` | yes |
| V2 | New shared validators pass direct offline tests in `test-utils.R` | `testthat::test_file("tests/testthat/test-utils.R")` | yes |
| V3 | `get_cp()`, `get_cp_ki()`, `get_aux()` reject malformed shared args before HTTP | New tests in `test-get_cp.R`, `test-get_cp_ki.R`, `test-get_aux.R` | yes |
| V4 | Full suite passes with no regressions | `devtools::test()` | yes |
| V5 | `get_stats(release_version = "2024-06-27")` aborts locally with `YYYYMMDD` message | Manual R console check | yes |

### Constraints
| ID | Constraint | Check |
|----|------------|-------|
| C1 | Valid calls preserve existing behavior and defaults | Existing tests pass unchanged |
| C2 | `get_cp_ki()` keeps required single-country rule | `get_cp_ki()` rejects NULL and multi-country |
| C3 | `get_gd()` behavior is unchanged | No changes to `get_gd.R` |
| C4 | Version identity suffix not hard-coded to `PROD` | Validator accepts structural format only |

### Boundaries
- Allowed: Refactor `validate_get_stats_args()` into reusable validators; add composers for `get_cp()`, `get_cp_ki()`, `get_aux()`; add offline tests.
- Out of scope: `get_gd()` validation refactor; new validation rules beyond current contracts; behavior changes to valid requests; documentation updates.

### Iteration Policy
1. Refactor `utils.R` validators first, keeping `validate_get_stats_args()` behavior identical.
2. Wire composers into clients one at a time, running each client's tests after each.
3. Add tests incrementally alongside each validator.
4. Under `ask` policy, pause before any deviation from this plan.

### Blocked-Stop Conditions
- Any required verification cannot be run through the safe runner.
- A required evidence item fails after allowed recovery attempts.
- A required deviation is discovered under policy `ask` and user approval is unavailable.
- A protected boundary must be crossed to continue.
- `get_gd()` behavior would need to change to proceed.