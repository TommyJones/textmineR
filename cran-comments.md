
### Resubmission
This is a resubmission. In this version I have

* Replaced the C header "math.h" with the preferred "cmath"
* Explicitly cast integers as double before performing sqrt
* Explicitly declared log and sqrt as coming from the std library

In the previous resubmission(s) I have

* Restructured most example code so that it can be checked explicitly
* Removed RcppArmadillo from imports


### Test environments
* local OS X install, R 3.2.3
* Ubuntu 14.04 LTS (local install), R 3.2.2
* win-builder (devel and release)

### R CMD check results
There were no ERRORs or WARNINGs. 
There is one NOTE.
  - This is a first-time submission

### Downstream dependencies
There are no downstream dependencies. This is a first-time submission of textmineR.

