### Patch release version 3.0.3
This version is a patch where I have:

* changed my name in the maintainer field from "Thomas Jones" to "Tommy Jones".
* fixed an error related to the `update.lda_topic_model` method.
* added a method `posterior.lda_topic_model` to sample from the posterior of an
  LDA topic model. 


### Test environments
* local MacOS install, R 3.5.2
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel, release, and oldrel)

### R CMD check results
There are no WARNINGs or ERRORs.

There is one NOTE indicating that I changed "Thomas Jones" to "Tommy Jones" in
the maintainer field.

### Downstream dependencies
There are no downstream dependencies. 

