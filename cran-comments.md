### Patch version 2.0.2
This is a patch, I have
* Corrected functions using the `...` argument so that conflicts in the 
  underlying functions do not arise.
* Swapped a dependency of the irlba package for a dependency of the RSpectra
  package
* Corrected TmParallelApply so it no longer throws an error when called from
  R running on Windows. 

### Test environments
* local OS X install, R 3.3
* Ubuntu 14.04 LTS (local install), R 3.2.5
* win-builder (devel and release)

### R CMD check results
There are no NOTEs, WARNINGs, or ERRORs.

### Downstream dependencies
There are no downstream dependencies. 

