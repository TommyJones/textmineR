### Patch release version 3.0.2
This version is a patch that addresses two issues:

* I have modified the NAMESPACE to explicitly say 
"importFrom(stopwords,stopwords)" to address the issue.
* I have added an update method for the lda_topic_model class.

### Test environments
* local MacOS install, R 3.5.2
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel, release, and oldrel)

### R CMD check results
There are no WARNINGs, ERRORs, or NOTEs.

### Downstream dependencies
There are no downstream dependencies. 

