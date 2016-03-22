### Updates for version 1.6.1
#### Implemented
* Add modeling capability for latent semantic analysis
* Add CalcProbCoherence function which replaces ProbCoherence and can calculate
  probabilistic coherence for the whole phi matrix.

#### Planned 
* Deprecating arguments that use periods instead of underscores
* Update add arguments using underscores where necessary


### Updates for version 1.6.0

* Add deprecation warning to ProbCoherence 
* Allow for arguments of number of cores to be passed to every function that 
  uses implicit parallelziation 
* Allow for passing of libraries to TmParallelApply (makes this function truely
  independent of textmineR) 
* For Vec2Dtm ensure that stopwords and custom stopwords are lowercased 
  when lower = TRUE 
* Update README example to use model caches 
  
### Planned for version 2.0.0

* Parallelization at C++ level with RcppParallel where possible
* Core DTM creating functions moved from tm framework to being performed in 
  C++, parallelized where possible
  - regexp for stopword removal/punctuation removal/number removal
  - tokenization on spaces
  - n-gram tokenization
  - stemming/lemmatization