# API Reference

<!-- cg:auto:functions -->
The package exposes the following API client functions:

- `get_stats()` — compute poverty and inequality statistics
- `get_cp()` — get country profiles
- `get_cp_ki()` — get country profile key indicators
- `get_gd()` — get grouped data
- `get_aux()` — get auxiliary tables
- `display_aux()` — list clickable auxiliary tables
- `delete_cache()` / `get_cache_info()` — manage the local API response cache

All API client functions validate their arguments **locally, before any HTTP
request is made**. Malformed arguments abort immediately with a consistent
`cli` error message naming the argument and its expected format
(`Invalid `<argument>`. `<argument>` must be <expected>.`), instead of
producing generic HTTP 404/500 errors from the PIP API.
<!-- cg:auto:end -->

<!-- cg:auto:parameters -->
Each function accepts parameters for country/region selection, poverty lines,
years, and indicator choices. See the package documentation (`?get_stats`) for
full parameter details.

Key validation contracts enforced locally:

- `release_version` — a date in `YYYYMMDD` format (e.g. `"20240627"`, not
  `"2024-06-27"`)
- `ppp_version` — a single 4-digit year, either numeric (integer, non-`Inf`)
  or a `"YYYY"` character string
- `version`, `format`, `api_version` — must match the allowed choices
  (`PIP_API_VERSIONS`, `PIP_CP_FORMATS`, etc.); invalid choices abort with the
  expected value listed
- `country`, `povline` — validated per client before request construction
<!-- cg:auto:end -->

<!-- cg:auto:return-values -->
Functions return tidy data frames of computed indicators. API responses are
cached locally according to the PIP API cache policy, and retries are handled
automatically when hitting rate limits. Invalid arguments never reach the API:
they abort locally with a `cli` error before any request is sent.
<!-- cg:auto:end -->

← [Home](README.md)