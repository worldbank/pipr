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
<!-- cg:auto:end -->

<!-- cg:auto:parameters -->
Each function accepts parameters for country/region selection, poverty lines, years, and indicator choices. See the package documentation (`?get_stats`) for full parameter details.
<!-- cg:auto:end -->

<!-- cg:auto:return-values -->
Functions return tidy data frames of computed indicators. API responses are cached locally according to the PIP API cache policy, and retries are handled automatically when hitting rate limits.
<!-- cg:auto:end -->

← [Home](README.md)