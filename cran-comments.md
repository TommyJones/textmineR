### Patch release version 3.0.4
This version is a patch where I have:

* Removed unconditional stripping in MAKEVARs as specified by CRAN
* Improved outputs of `FitLdaModel`


### Test environments
* local MacOS install, R 3.5.3
* Ubuntu 14.04 LTS (on travis ci), R 3.5.3
* Ubuntu 16.04 LTS (on R Hub), R-Release
* win-builder (devel, release, and oldrel)

### R CMD check results
There are no WARNINGs or ERRORs.

There is one NOTE on Ubuntu 16.04:

checking installed package size ... NOTE
  installed size is  5.8Mb
  sub-directories of 1Mb or more:
    libs   4.0Mb

This is due to unstripped libraries. I commented out the command to 
unconditionally strip libraries during compilation as instructed by CRAN in
an email sent 10 April to me and several other package developers. 

### Downstream dependencies
There are no downstream dependencies. 

