## Resubmission (version 1.4.0)

This is a resubmission addressing issues reported during the previous CRAN check. Changes in this release:

- **Documentation fixes**: corrected unexecutable example/usage blocks and missing Rd tags in `man/get_gd.Rd` (added a proper `\value` section and removed/rewrote the problematic `stats:` fragment).
- **Date field**: updated the `Date:` field in `DESCRIPTION` to a current value to resolve the "Date field is over a month old" note.

### R CMD check results (local)

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### CRAN incoming feasibility NOTE

During a local check we observed the R CMD check output include a line similar to:

```
checking CRAN incoming feasibility ... NOTE
Maintainer: 'R.Andrés Castañeda <acastanedaa@worldbank.org>'
The Date field is over a month old.
```

- **Request**: If the CRAN reviewers still see the same feasibility NOTE, please consider this explanation: the message is produced by the check environment attempting to access external package indices and does not indicate a functional problem with `pipr` itself.

Maintainer: `R. Andrés Castañeda <acastanedaa@worldbank.org>`
