### Patch version 2.0.5
This is a patch, I have
* Add `verbose` option to `CreateDtm` and `CreateTcm` to surpress status messages.
* Add function `GetVocabFromDtm` to get `text2vec` vocabulary object from a `dgCMatrix` 
  document term matrix.
* Updated some initializations to conform to R devel relating to registration of 
  dynamic symbols.

### Test environments
* local OS X install, R 3.3.3
* Ubuntu 14.04 LTS (local install), R 3.3.1
* win-builder (devel and release)

### R CMD check results
There are no WARNINGs, or ERRORs.

There is one NOTE: 
Maintainer: 'Thomas Jones <jones.thos.w@gmail.com>'

New maintainer:
  Thomas Jones <jones.thos.w@gmail.com>
Old maintainer(s):
  Thomas W. Jones <jones.thos.w@gmail.com>
  
This is due to a change in format for the maintainer in the DESCRIPTION file.
The maintainer has not actually changed.

### Downstream dependencies
There are no downstream dependencies. 

