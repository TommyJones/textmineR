
### Update
This is an update, moving textmineR from version 1.5.1 to 1.6.0. 
In this version I have 

* Added deprecation warning to ProbCoherence 
* Allowed for arguments of number of cores to be passed to every function that 
  uses implicit parallelziation 
* Allowed for passing of libraries to TmParallelApply (makes this function truely
  independent of textmineR) 
* For Vec2Dtm ensured that stopwords and custom stopwords are lowercased 
  when lower = TRUE 
* Updated README example to use model caches 


### Test environments
* local OS X install, R 3.2.3
* Ubuntu 14.04 LTS (local install), R 3.2.2
* win-builder (devel and release)

### R CMD check results
There is one WARNING.

* Warning: package ‘Matrix’ was built under R version 3.2.4

R version 3.2.4 is not scheduled for official release until 3/10/2016. 
Perhaps this build of 'Matrix' is premature? If this is a killer, I'd just as
soon wait until the 10th for 3.2.4 to be officially released.


### Downstream dependencies
There are no downstream dependencies. 

