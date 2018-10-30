### Major release version 3.0.0
This version significantly changes textmineR. Changes are listed below.

Since vingette build time has been an issue in the past, my build times for
Mac and Windows testing environements are: 
  - MacOS 4m 56.6s
  - win-builder 4m 30s

A summary of changes:
* Several functions that were slated for deletion in version 2.1.3 are now gone
* The function FitLdaModel has changed significantly in its behavior. 
* I have added many unit tests using the testthat package.
* I have added a new function SummarizeTopics
* I have changed package dependencies, adding some, removing others, and shifting
  some from Imports to Suggests.
* I have changed the license from GPL to MIT and added a LICENSE file.


### Test environments
* local MacOS install, R 3.5.1
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel and release)

### R CMD check results
There are no WARNINGs, NOTES, or ERRORs.

### Downstream dependencies
There are no downstream dependencies. 

