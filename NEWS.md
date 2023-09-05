# pipr 1.1.0

* [Use httr2](https://github.com/worldbank/pipr/pull/70)
  * API responses are now cached locally according to the PIP API cache policy
  from the PIP API responses headers
  * `pipr` automatically handles retries when hitting the PIP API rate limiting
  threshold
  * Improved translation of HTTP errors into R error messages
* New helper functions `delete_cache()` and `get_cache_info()`

# pipr 1.0.0

* [Mock live API calls or skip them on CRAN](https://github.com/worldbank/pipr/pull/45)
* [fix for dictionary table](https://github.com/worldbank/pipr/pull/43)
* Add new `display_aux` function to get a list of clickable auxiliary tables
* Improved documentation

# pipr 0.0.4

* [Hot fix](https://github.com/worldbank/pipr/pull/40)

# pipr 0.0.3

* [Add new ppp_version and release_version parameters](https://github.com/worldbank/pipr/pull/38)
* [Remove systematic check_api() call in get_stats()](https://github.com/worldbank/pipr/pull/38)

# pipr 0.0.2

## Bug fixes
* Fix `popshare` argument not being passed to `get_stats()`

# pipr 0.0.1

* [CRAN release](https://github.com/worldbank/pipr/issues/18)
* Add helper functions to facilitate retrieval of static tables [Link to PR](https://github.com/worldbank/pipr/pull/27)
* [Include citation in README file](https://github.com/worldbank/pipr/issues/31)

# pipr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
