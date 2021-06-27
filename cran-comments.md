## Resubmition
I have addressed CRAN issues relating to two URLs.

*  In man/nih.Rd: 
    http://exporter.nih.gov/ExPORTER_Catalog.aspx --> https://exporter.nih.gov/ExPORTER_Catalog.aspx
* In inst/doc/b_document_clustering.html
    http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determining-the-optimal-number-of-clusters-3-must-know-methods/ --> https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
    
I've updated both. 

In addition, I've merged a PR that came between submission and re-submission
that gives the option for a tibble or data.frame output from some functions (
described in the last bullet point, below.)

## Patch release version 3.0.5
This version is a patch where I have:

* Fixed a bug in `CalcHellignerDist()` and `CalcJSDivergence()` that sometimes
  caused inputs to be overwritten.
* Fixed some typos in the vignette for topic modeling
* Updated the documentation on `FitCtmModel()` to better explain how to pass
  control arguments to CTM's underlying function.
* Enabled return of a `tibble` or `data.frame` (instead of only `data.frame`) in
  the following functions: `SummarizeTopics`, `GetTopTerms`, `TermDocFreq`


## Test environments
* local macOS install: release
* macOS (on GitHub actions): release
* ubuntu 20.04 (on GitHub actions): release
* win-builder: release, devel, and oldrel

## R CMD check results
There are no NOTEs, WARNINGs, or ERRORs.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


