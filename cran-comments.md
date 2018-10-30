### Patch release version 3.0.1
This version is in response to issues revealed by automatic checks on package submission.

I have
* Used the CRAN template for my MIT LICENSE file
* Modified the example of the LabelTopics function to speed up run time for that example

Since build and check time has been an issue in the past, my build times for
Mac and Windows testing environements are: 
  - MacOS 4m 56.6s for vignettes and 10m 4.9s total
  - win-builder (devel) 5m 53s for vignettes and 12m 16s total
  - win-builder (release) 5m 41s for vignettes and 11m 18s total

### Test environments
* local MacOS install, R 3.5.1
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel and release)
* 

### R CMD check results
There are no WARNINGs, ERRORs.

On Ubuntu, I am receiving one NOTE.

checking installed package size ... NOTE
  installed size is  5.0Mb
  sub-directories of 1Mb or more:
    libs   4.0Mb    

This just started. Not sure what there is to do here. The compiled size of the 
libs directory on my computer is 365 Kb and I'm not getting that NOTE from the 
win-builder. Possibly this is a result of my poor configuration of the travis ci 
instance, but I'm not able to reproduce it on other systems.

### Downstream dependencies
There are no downstream dependencies. 



Notes from the immediately prior (submitted and failed) release are below.

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



