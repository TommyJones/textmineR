### Patch version 2.1.
In this version, as directed by CRAN, I have deprecated RecursiveRbind - it depended on a deprecated function from the Matrix package. And the replacement offered by Matrix operates recursively, making this function truly superfluous.

### Test environments
* local OS X install, R 3.4.2
* Ubuntu 16.04 LTS (local install), R 3.4.3
* win-builder (devel and release)

### R CMD check results
There are no WARNINGs, NOTES, or ERRORs.

### Downstream dependencies
There are no downstream dependencies. 

