### Patch release version 3.0.1
This version is in response to issues revealed by automatic checks on package 
submission plus an additional issue I encountered along the way.

I have
* Used the CRAN template for my MIT LICENSE file
* Modified the example of the LabelTopics function to speed up run time for that example
* Modified vignettes to run in less time 
* Added a Makevars file to keep compiled code small on Ubuntu.

Since build and check time has been an issue in the past, my build times for
my testing environements are: 
  - MacOS 3m 23s for vignettes and 8m 34s total
  - Ubuntu unknown for vignettes and 9m 58s total
  - win-builder (devel) 4m 20s for vignettes and 11m 11s total
  - win-builder (release) 3m 53ss for vignettes and 9m 45s total
  
In a future version, I plan to migrate the bulk of the vignette code to a stand-
alone website, which will save the CRAN check farm CPU time. For now, I have 
greatly restricted the scope of vignette examples to keep rebuilding time lower.

### Test environments
* local MacOS install, R 3.5.1
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel and release)

### R CMD check results
There are no WARNINGs, ERRORs, or NOTEs.

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



