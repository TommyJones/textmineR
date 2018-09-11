### Patch version 2.1.3
I have addressed CRAN comments on vignette length. Additional updates are described below.

In this version, I have deprecated many functions. I am preparing a release of version 3.0 which drops or significantly modifies the behaviour of several functions. I am planning the major update for late October.

* Functions slated for deletion:
  - RecursiveRbind
  - Vec2Dtm
  - JSD
  - HellDist
  - GetPhiPrime
  - FormatRawLdaOutput
  - Files2Vec
  - DepluralizeDtm
  - CorrectS
  - CalcPhiPrime
  
* In addition: FitLdaModel is going to change significantly in its functionality and argument calls. When run, it issues a warning.

### Test environments
* local OS X install, R 3.5.1
* Ubuntu 14.04 LTS (on travis ci), R 3.5.1
* win-builder (devel and release)

### R CMD check results
There are no WARNINGs, NOTES, or ERRORs.

### Downstream dependencies
There are no downstream dependencies. 

