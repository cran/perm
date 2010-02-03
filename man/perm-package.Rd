\name{perm-package}
\alias{perm}
\docType{package}
\title{Exact or Asymptotic linear permutation tests}
\description{
This package gives several methods for performing permutation tests.}
\details{
\tabular{ll}{
Package: \tab perm\cr
Type: \tab Package\cr
Version: \tab 0.9-1.3\cr
Date: \tab 2009-04-01\cr
License: \tab GPL \cr
}
The package has three main functions, to perform linear permutation tests. 
These tests are tests where the test statistic is the sum of the product of a covariate (usually group indicator) and the 
scores. The three tests are: \code{\link{permTS}} to perform two sample permutation tests,
\code{\link{permKS}} to perform K-sample permutation tests,
\code{\link{permTREND}} to perform trend permutation tests on numeric values.
By using suitable scores one can create
for example, the permutation t-test (general scores), the Wilcoxon rank sum test (rank scores),
the logrank test (need to use other functions to create these scores). The two sample test uses either 
exact (network algorithm, complete enumeration, or Monte Carlo) or asymptotic calculations (using permutational 
central limit theorem [pclt]), while the other tests use only the exact Monte Carlo or the pclt.  
Most (if not all) of the tests here are also implemented in the \code{coin} package. 
This package provides an independent validation of that package. 
}
\author{
Michael Fay

Maintainer: Michael Fay <mfay@niaid.nih.gov>
}
%\references{}
%~~ Optionally other standard keywords, one per line, from file ~~
%~~ KEYWORDS in the R documentation directory ~~
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}
