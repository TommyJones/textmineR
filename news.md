
### Updates for version 2.1.1
* Corrected some code in the vignettes that caused errors on Linux machines.

### Updates for version 2.1.0
* Added vignettes for common use cases of textmineR
* Modified averaging for `CalcProbCoherence`
* Updated documentation to `CreateTcm`

### Updates for version 2.0.6
* Back-end changes to CreateTcm in response to new `text2vec` API. Functionality is unchanged.
* Changes to how the package interfaces with Rcpp

### Updates for version 2.0.5
* Add `verbose` option to `CreateDtm` and `CreateTcm` to surpress status messages.
* Add function `GetVocabFromDtm` to get `text2vec` vocabulary object from a `dgCMatrix` 
  document term matrix.
  
### Updates for version 2.0.4
* Patching errors introduced in version 2.0.3

### Updates for version 2.0.3

* Patches to `CreateDtm` and `CreateTcm` in response to updates to `text2vec`.
* More formal update to take advantage of `text2vec`'s latest optimizations to follow.

### Updates for version 2.0.2

* Patched `CreateDtm` and `CreateTcm`. remove_punctuation now supports non-English 
  characters.
* Patched `TmParallelApply`. Added an option to declare the environment to search 
  for your export list. Default to that argument just searches the local 
  environment. The default should cover ~95% of use cases. (And avoids crash on
  Windows OS)
* Patched `FitLdaModel`. Use of the `...` argument now allows you to control 
  `TmParallelApply`, `lda::lda.collapsed.gibbs.sampler`, and `topicmodels::LDA`
  without error.
* Patched `FitCtmModel` where the `...` argument now goes to `topicmodels::CTM`'s
  `control` argument.
* Patched `CreateTcm` to return objects of class `dgCMatrix`. This allows you to
  run functions like `FitLdaModel` on a TCM.
* Switched from irlba to RSpectra for LSA models because RSpectra's 
  implementation is much faster.


### Updates for version 2.0.1

* Patched CreateDtm and CreateTcm. An error caused stopwords to not be removed

### Updates for version 2.0.0

* Vec2Dtm is now deprecated in favor of CreateDtm
* A function, CreateTcm, now exists to create term co-occurence matrices
* CreateDtm and CreateTcm are implemented with a parallel C++ back end through the text2vec library
  - the implementation is _much_ faster! I've clocked 2X - 10X speedups, depending on options
  - adds external dependencies - C++ compiler and GNU make - and takes away an external
    dependency - Java.
  - now _all_ tokens will be included, regardless of length. (tm's framework silently
    dropped all tokens of fewer than 3 characters.)
* Allow generic stemming and stopwords in CreateDtm & CreateTcm
  - Now there is only one argument for stopwords, making it clearer how to use 
    custom or non-English stopwords
  - Now the stemming argument allows for passing of stem/lemmatization functions.
* Function for fitting correlated topic models
* Function to turn a document term matrix to term co-occurence matrix
* Allowed LabelTopics to use unigrams, if you want. (n-grams are still better.)
* More robust error checking for CalcTopicModelR2 and CalcLikelihood
* All function arguments use "_", not ".".
* CalcPhiPrime replaces (the now deprecated) GetPhiPrime
  - Allows you to pass an argument to specify non-uniform probabilities of each 
    document
* Similarly, CalcHellingerDist and CalcJSDivergence replace HellDist and JSD.
  This is to conform to a naming convention where functions are "verbs".


### Updates for version 1.7.0

* Added modeling capability for latent semantic analysis in FitLsaModel()
* Added CalcProbCoherence() function which replaces ProbCoherence() and can calculate
  probabilistic coherence for the whole phi matrix.
* Added data from NIH research grants instead of borrowd data from tm
* Removed qcq data 
* Added variational em method for FitLdaModel()
* Added function to represent document clustering as a topic model Cluster2TopicModel()

### Updates for version 1.6.0

* Add deprecation warning to ProbCoherence 
* Allow for arguments of number of cores to be passed to every function that 
  uses implicit parallelziation 
* Allow for passing of libraries to TmParallelApply (makes this function truely
  independent of textmineR) 
* For Vec2Dtm ensure that stopwords and custom stopwords are lowercased 
  when lower = TRUE 
* Update README example to use model caches 
  
