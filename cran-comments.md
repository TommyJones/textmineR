### Patch
This is a patch, I have
* Corrected a significant error in two functions. 
  - Users would expect stop words to be removed, but an error in the function meant
    that they would not be removed regardless of users passing an argument
  - This is now fixed

### Note on CRAN check of version 2.0
* r-oldrel-windows-ix86+x86_64 is showing an ERROR
* `text2vec` is included as an import in the DESCRIPTION file
* I cannot reproduce this ERROR on any of my machines

### Test environments
* local OS X install, R 3.2.4
* Ubuntu 14.04 LTS (local install), R 3.2.5
* win-builder (devel and release)

### R CMD check results
There are no NOTEs, WARNINGs, or ERRORs.

### Downstream dependencies
There are no downstream dependencies. 

