---
date: 2026-09-25
title: "Mock ppp_version = 2011 tests that fail on live API errors"
category: "bugs"
language: "R"
tags: [testthat, mocking, httr2, local-mocked-bindings, fixture-gap, api-availability, r-cmd-check]
test-gap: "fixture-gap"
expected-behavior-source: "user-requirement"
severity: "P2"
red-phase-confirmed: "yes"
---

# Mock ppp_version = 2011 tests that fail on live API errors

## Problem
R CMD check failed on the PR branch because two live-API tests failed:

- `tests/testthat/test-get_cp.R:47` — `get_cp(country = "AGO", ppp_version = 2011, povline = NULL)` → "Failed to parse error body… Unexpected content type text/plain"
- `tests/testthat/test-get_cp_ki.R:49` — `get_cp_ki(country = "IDN", ppp_version = 2011, povline = NULL)` → HTTP 500 on `/api/v1/cp-key-indicators`

Both failures were confirmed pre-existing on `main` (verified via `git stash` re-run), so they were not regressions from the validation refactor.

## Root Cause
The PIP API errors for `ppp_version = 2011` requests (HTTP 500 / non-JSON body). The tests were correct in intent — they covered the `povline <- 1.9` conditional branch in `get_cp()`/`get_cp_ki()` — but depended on live API availability for a specific PPP version. This is a **fixture-gap**: the test environment (live API) cannot supply the fixture the test needs.

## Solution
Keep the branch coverage but make it deterministic and offline: mock `httr2::req_perform` with `local_mocked_bindings()` and assert on the **captured request URL** instead of the response.

```r
test_that("get_cp() sends povline = 1.9 when ppp_version = 2011 and povline is NULL", {
  captured_url <- NULL
  mock_res <- structure(
    list(
      url = "http://mock-api/cp-download",
      status_code = 200,
      body = charToRaw("{}"),
      headers = list("content-type" = "application/json")
    ),
    class = "httr2_response"
  )

  local_mocked_bindings(
    req_perform = function(req) {
      captured_url <<- req$url
      mock_res
    },
    .package = "httr2"
  )

  suppressWarnings(get_cp(country = "AGO", ppp_version = 2011, povline = NULL))

  expect_true(grepl("povline=1.9", captured_url))
})
```

The failing live assertions were removed from both files and replaced by a comment pointing to the mocked tests. Companion tests assert that for non-2011 versions **no** `povline` parameter is sent (see gotcha below).

### Key gotchas

**1. The 2.15 default is applied server-side, not client-side.** The first
version of the non-2011 test asserted `grepl("povline=2.15", captured_url)`
and failed: the captured URL was
`...cp-download?country=AGO&ppp_version=2017&format=arrow` — no `povline`
parameter at all. The client only injects `povline = 1.9` for
`ppp_version = 2011`; for other versions the API applies its own default.
The correct assertion is the *absence* of the parameter:

```r
expect_false(grepl("povline=1.9", captured_url))
expect_false(grepl("povline=", captured_url))
```

**2. `get_cp_ki()` post-processes the response, so the mock body must not
break `unnest_ki()`.** A `{}` JSON body caused
`Error in fix.by(by.x, x): 'by' must specify uniquely valid columns` in the
merge chain inside `unnest_ki()`. Since the tests only care about the
request URL, pass `simplify = FALSE` to skip the unnest step entirely.

**3. Why URL assertions instead of changing tests to 2017.** Switching the
tests to `ppp_version = 2017` would lose the only coverage of the
`povline <- 1.9` conditional branch (the 2017 default 2.15 is applied
server-side and already covered by other assertions). `get_versions()`
still lists 2011 versions, so the branch is legitimate code that must stay
covered.

## Prevention
- **Never let R CMD check depend on live API availability for a specific data version** — mock the HTTP boundary when the assertion is about what the client *sends*, not what the server returns.
- **Assert on the request, not the response, when testing client-side conditional logic** — capturing `req$url` in a mocked `req_perform` is deterministic and offline.
- **Check where defaults are applied** (client vs server) before asserting a default value appears in a request URL.
- **When mocking responses for functions with post-processing**, either supply a realistic body fixture or bypass the post-processing path (`simplify = FALSE`) if it is not under test.
- **Verify pre-existing failures before fixing** — `git stash` + re-run confirmed these failures existed on `main`, avoiding a misdiagnosis as a refactor regression.

## Follow-up
Roadmap item added: restore the live `ppp_version = 2011` API tests when the PIP API supports those requests.

## Related
- Prior solution: `.cg-docs/solutions/data-quality/2026-09-21-reusable-api-argument-validation.md` (verification pattern for pre-existing test failures)
- Mocking pattern source: `tests/testthat/test-other.R`
- Roadmap: `roadmap.json` (feature `restore-ppp-2011-live-tests`)
