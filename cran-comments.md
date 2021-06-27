## Patch release version 3.0.5
This version is a patch where I have:

* Fixed a bug in `CalcHellignerDist()` and `CalcJSDivergence()` that sometimes
  caused inputs to be overwritten.
* Fixed some typos in the vignette for topic modeling
* Updated the documentation on `FitCtmModel()` to better explain how to pass
  control arguments to CTM's underlying function.


## Test environments
* local macOS install: release
* macOS (on GitHub actions): release
* ubuntu 20.04 (on GitHub actions): release
* win-builder: release, devel, and oldrel

## R CMD check results
There are no NOTEs, WARNINGs, or ERRORs.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


