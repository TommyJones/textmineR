### Patch release version 3.0.2
This version is a patch where I have:

* addressed some issues from CRAN's automatic checks. I added a new vignette and
  (a) spawned too many processes during execution and (b) copied another vignette's
  title. Both issues have been fixed.
* modified the NAMESPACE to explicitly say "importFrom(stopwords,stopwords)" to 
  address an issue that came up during checks for some systems after going to CRAN.
* added an update method for the lda_topic_model class.
* added a vignette for using `tidytext` alongside `textmineR`

### Test environments
* local MacOS install, R 3.5.2
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel, release, and oldrel)

### R CMD check results
There are no WARNINGs, ERRORs, or NOTEs.

### Downstream dependencies
There are no downstream dependencies. 

