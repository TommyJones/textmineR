### Patch release version 3.0.4
This version is a patch where I have:

* Removed unconditional stripping in MAKEVARs as specified by CRAN
* Improved outputs of `FitLdaModel`


### Test environments
* local MacOS install, R 3.5.3
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel, release, and oldrel)

### R CMD check results
There are no WARNINGs or ERRORs.

There is one NOTE indicating that I changed "Thomas Jones" to "Tommy Jones" in
the maintainer field.

### Downstream dependencies
There are no downstream dependencies. 

