
Note that this is a second submission of textmineR. I've addressed the comments
I received in the following ways:

I have restructured most example code so it can be checked as part of the
submission process. I did this by creating some data that the examples can run
off of, without throwing the check error about parallel processes.

I have removed RcppArmadillo from the imports. (I was 
incorrect that this removal caused an error. The error I was thinking of was 
from another source.)

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

