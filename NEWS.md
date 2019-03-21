# textmineR 3.0.3
This version is a patch. In this version I have

* fixed an error related to the `update.lda_topic_model` method.
* added a method `posterior.lda_topic_model` to sample from the posterior of an
  LDA topic model. 

# textmineR 3.0.2
This version is a patch. In this version I have

* changed some elements of NAMESPACE to pass additional CRAN checks.
* added an update method for the lda_topic_model class. This allows users to add
  documents to an existing model (and even add new topics) without chaning the 
  indices of previously-trained topics. e.g. topic 5 is still topic 5.
* added a vignette for using `tidytext` alongside `textmineR`

# textmineR 3.0.1
This version is a patch in response to issues revealed by automatic checks upon 
submission to CRAN plus an additional issue I encountered along the way.

I have
* Used the CRAN template for my MIT LICENSE file
* Modified the example of the LabelTopics function to speed up run time for that example
* Modified vignettes to run in less time 
* Added a Makevars file to keep compiled code small on Ubuntu.

Please read below for major updates between v2.x.x and v3.x.x

# textmineR 3.0.0
This version significantly changes textmineR.

* Several functions that were slated for deletion in version 2.1.3 are now gone.
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
  
* FitLdaModel has changed significantly. 
  - Now only Gibbs sampling is a supported training method. The Gibbs sampler is
    no longer wrapping lda::lda_collapsed_gibbs_sampler. It is now native to 
    textmineR. It's a little slower, but has additional features.
  - Asymmetric priors are supported for both alpha and beta.
  - There is an option, optimize_alpha, which updates alpha every 10 iterations
    based on the value of theta at the current iteration. 
  - The log likelihood of the data given estimates of phi and theta is optionally 
    calculated every 10 iterations. 
  - Probabilistic coherence is optionally calculated at the time of model fit.
  - R-squared is optionally calculated at the time of model fit.
  
* Supported topic models (LDA, LSA, CTM) are now object-oriented, creating their
  own S3 classes. These classes have their own predict methods, meaning you do
  not have to do your own math to make predictions for new documents.
  
* A new function SummarizeTopics has been added.

* tm is no longer a dependency for stopwords. We now use the stopwords package. 
  The extended result of this is that there is no longer *any* Java dependency.

* Several packages have been moved from "Imports" to "Suggests". The result is 
  a faster install and lower likelihood of install failure based on packages with
  system dependencies. (Looking at you, topicmodels!)
  
* Finally, I have changed the textmineR license to the MIT license. Note, however,
  that some dependencies may have more restrictive licenses. So if you're looking
  to use textmineR in a commercial project, you may want to dig deeper into 
  what is/isn't permissable.


# textmineR 2.1.3
* Deprecating functions that will be removed, renamed, or have significant changes to syntax or functionality in the forthcoming textmineR v3.0. 
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
* In addition: FitLdaModel is going to change significantly in its functionality and argument calls.

# textmineR 2.1.2
* Deprecated RecursiveRbind - it depended on a deprecated function from the Matrix package. And the replacement offered by Matrix operates recursively, making this function truly superfluous.

# textmineR 2.1.1
* Corrected some code in the vignettes that caused errors on Linux machines.

# textmineR 2.1.0
* Added vignettes for common use cases of textmineR
* Modified averaging for `CalcProbCoherence`
* Updated documentation to `CreateTcm`

# textmineR 2.0.6
* Back-end changes to CreateTcm in response to new `text2vec` API. Functionality is unchanged.
* Changes to how the package interfaces with Rcpp

# textmineR 2.0.5
* Add `verbose` option to `CreateDtm` and `CreateTcm` to surpress status messages.
* Add function `GetVocabFromDtm` to get `text2vec` vocabulary object from a `dgCMatrix` 
  document term matrix.
  
# textmineR 2.0.4
* Patching errors introduced in version 2.0.3

# textmineR 2.0.3

* Patches to `CreateDtm` and `CreateTcm` in response to updates to `text2vec`.
* More formal update to take advantage of `text2vec`'s latest optimizations to follow.

# textmineR 2.0.2

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


# textmineR 2.0.1

* Patched CreateDtm and CreateTcm. An error caused stopwords to not be removed

# textmineR 2.0.0

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


# textmineR 1.7.0

* Added modeling capability for latent semantic analysis in FitLsaModel()
* Added CalcProbCoherence() function which replaces ProbCoherence() and can calculate
  probabilistic coherence for the whole phi matrix.
* Added data from NIH research grants instead of borrowd data from tm
* Removed qcq data 
* Added variational em method for FitLdaModel()
* Added function to represent document clustering as a topic model Cluster2TopicModel()

# textmineR 1.6.0

* Add deprecation warning to ProbCoherence 
* Allow for arguments of number of cores to be passed to every function that 
  uses implicit parallelziation 
* Allow for passing of libraries to TmParallelApply (makes this function truely
  independent of textmineR) 
* For Vec2Dtm ensure that stopwords and custom stopwords are lowercased 
  when lower = TRUE 
* Update README example to use model caches 
  
