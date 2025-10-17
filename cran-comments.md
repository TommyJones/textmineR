## Patch release version 3.0.6
This is a re-submission correcting WARNINGs found after submission to CRAN.

"Warning: textmineR.Rd:3: docType '_PACKAGE' is unrecognized"

* Documentation now follows proper syntax.
* Vignettes no longer rely on parallel processing.

Note also, textmineR is in maintenance-only mode: it remains functional and useful for
existing users, but is no longer under active feature development. Where possible
I have made minimal adjustments (e.g., vignette gating, DESCRIPTION metadata 
updates) to keep the package compliant.

For new projects, I recommend users adopt `tidylda`, which is the actively 
maintained successor to `textmineR` and implements more modern topic modeling workflows.

If the maintenance burden grows significantly, I am prepared to allow the package
to be archived and direct users to `tidylda`.

## Test environments
* local macOS install: release
* macOS (on GitHub actions): release
* ubuntu 24.04.3 (on GitHub actions): release
* win-builder: release, devel, and oldrel

## R CMD check results
There are no NOTEs, WARNINGs, or ERRORs.

## revdepcheck results

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


