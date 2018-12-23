### Patch release version 3.0.2
This version is in response to a NOTE that appeared during checks after v3.0.1
appeared on CRAN. I have modified the NAMESPACE to explicitly say 
"importFrom(stopwords,stopwords)" to address the issue.

### Test environments
* local MacOS install, R 3.5.2
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel, release, and oldrel)

### R CMD check results
There are no WARNINGs, ERRORs, or NOTEs.

### Downstream dependencies
There are no downstream dependencies. 

