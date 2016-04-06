### Updates for version 2.0.0

#### Implemented
* Vec2Dtm is now implemented with a parallel C++ back end through the text2vec library
  - the implementation is _much_ faster! About a 10x speedup for smaller corpora.
    I expect there to be an even greater speedup with larger corpora.
  - adds external dependencies - C++ compiler and GNU make - and takes away an external
    dependency - Java.
  - stop words are the same and come from the tm library
  - stemming should be the same, using Porter's stemmer
  - now _all_ tokens will be included, regardless of length. (tm's framework silently
    dropped all tokens of fewer than 3 characters.)
* Function for fitting correlated topic models
* Function to turn a document term matrix to term co-occurence matrix
* Allowed LabelTopics to use unigrams, if you want. (n-grams are still better.)
* More robust error checking for CalcTopicModelR2 and CalcLikelihood
* All function arguments use "_", not "." to separate words.
* CalcPhiPrime replaces (the now deprecated) GetPhiPrime
  - Allows you to pass an argument to specify non-uniform probabilities of each 
    document
* Similarly, CalcHellingerDist and CalcJSDivergence replace HellDist and JSD.
  This is to conform to a naming convention where functions are "verbs".

#### Possible
* Allow generic stemming and stopwords in Vec2Dtm
  - At a minimum, communicate better how to use non-English stopwords and make
    the remove_stopwords argument less English-centric.


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
  
