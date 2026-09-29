## Resubmission

The package was archived on 2026-08-24 after a CRAN example made a live PIP
API request and received HTTP 504 on r-devel-linux-x86_64-fedora-gcc. The
`call_aux()` example no longer makes any network requests during checks.
Examples that require the live service now run only in interactive sessions.

Version 1.5.0 also documents the stricter argument validation introduced
since 1.4.0.

## R CMD check results

0 errors | 0 warnings | 1 note

* CRAN incoming feasibility reports "New submission", "Package was archived
  on CRAN", and the archive comment. This is expected for a resubmission of
  an archived package. The check also reports intermittent Internet access.

Checked locally on macOS arm64 with R 4.6.1 using
`devtools::check(remote = TRUE, manual = TRUE, args = "--as-cran")`.
Both the PDF and HTML manuals passed their checks.

## Reverse dependencies

No CRAN packages currently list `pipr` as a dependency.
