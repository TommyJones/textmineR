## Patch release version 3.0.6
This version is a patch where:

* The C++11 requirement has been removed.
* Package dependencies in vignettes have been handled more gracefully, to guard
  against failure to build vignettes during CMD check.
* Handled class checking with inherits() not if(class(object) == "class").
* Fixed broken URLs in documentation.

## Test environments
* local macOS install: release
* macOS (on GitHub actions): release
* ubuntu 24.04.3 (on GitHub actions): release
* win-builder: release, devel, and oldrel

## R CMD check results
There are no NOTEs, WARNINGs, or ERRORs.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


