
### Planned for version 1.6.0

* Add deprecation warning to ProbCoherence - DONE
* Allow for arguments of number of cores to be passed to every function that 
  uses implicit parallelziation
* Allow for passing of libraries to TmParallelApply (make this function truely
  independent of textmineR) - TESTING
* For Vec2Dtm ensure that stopwords and custom stopwords are lowercased 
  when lower= TRUE - DONE
* Update README example to use model caches - DONE
  
### Planned for version 2.0.0

* Parallelization at C++ level with RcppParallel where possible
* Core DTM creating functions moved from tm framework to being performed in 
  C++, parallelized where possible
  - regexp for stopword removal/punctuation removal/number removal
  - tokenization on spaces
  - n-gram tokenization
  - stemming/lemmatization