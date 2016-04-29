### Updates for version 2.0.2 (ongoing)

* Patched CreateDtm and CreateTcm. remove_punctuation now supports non-English characters.
* Patched TmParallelApply. Added an option to declare the environment to search for your export list.
  Default to that argument just searches the local environment. The default should cover ~95% of 
  use cases.


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
  
