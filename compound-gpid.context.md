# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely —
this file is committed to git and shared with the team.

## Data Sources
<!-- Where does data come from? File paths, databases, APIs, vintage conventions -->

## Domain Rules
<!-- Project-specific rules that Copilot should always follow -->

### API argument validation
- All API client functions (`get_stats()`, `get_cp()`, `get_cp_ki()`, `get_aux()`) must validate arguments locally via the shared validators in `R/utils.R` before building any HTTP request. Malformed input aborts with `abort_invalid_argument()` — never let the PIP API be the first validator.
- New client functions should compose existing validators (see `validate_get_cp_args()` as the template) rather than writing inline checks.
- In R validators, guard `length(x) == 1 &&` before any `is.finite()`/comparison chain — `is.finite()` is vectorized and `&&` errors on length > 1.
- API contract values live in package constants (`PIP_API_VERSIONS`, `PIP_CP_FORMATS`, `PIP_AUX_FORMATS`) — do not hard-code `"v1"` or format lists in new code.
- The `version` identifier is validated structurally (`YYYYMMDD_PPP_XX_YY_IDENTITY`); the identity suffix is not limited to `PROD` — `get_versions()` is the authority for valid values.
- Tests for pure-validator behavior must not be gated by `skip_if_offline()`/`skip_on_cran()` — only network-calling assertions need those skips.

## Work in Progress
<!-- Modules, features, or migrations currently underway -->

## Workspace Notes
<!-- Related folders, dependencies on other projects in the VS Code workspace -->

## Wiki Configuration
<!-- folder: wiki -->
<!-- audience: developers | researchers | end-users -->
<!-- tone: technical | conversational | formal -->