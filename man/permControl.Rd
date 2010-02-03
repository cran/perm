\name{permControl}
\alias{permControl}
\title{Auxiliary for controling permutation tests}
\description{
A function to create a list of arguments for \code{\link{permTS}}, \code{\link{permKS}} or 
\code{\link{permTREND}}.
}
\usage{
permControl(cm=NULL,nmc=10^3-1,seed=1234321,digits=12,p.conf.level=.99,setSEED=TRUE)
}
\arguments{
\item{cm}{a choose(n,m) by n matrix, used if method='exact.ce', ignored otherwise}
\item{nmc}{number of Monte Carlo replications, used if method='exact.mc', ignored otherwise}
\item{seed}{value used in \code{set.seed} if method='exact.mc', ignored otherwise} 
\item{setSEED}{logical, set to FALSE when performing simulations that use method='exact.mc'}
\item{p.conf.level}{confidence level for p value estimate, used if method='exact.mc', ignored otherwise }
\item{digits}{number of digits to use in \code{\link{signif}} for precision of test statistics}
}
\details{

When \code{cm}=NULL the resulting matrix is created by \code{\link{chooseMatrix}}, it may be optionally provided here 
only so that \code{chooseMatrix} does not need to be repeatedly called in simulations.  Also when doing simulations with
method='exact.mc', use setSEED=FALSE so that the seed is not reset to the same value each time you call the permutation test
function.

See \code{\link{calcPvalsMC}} for description of how p.conf.level is used. 
}
\value{
An list with the arguments as components.
}
%\author{Michael Fay}
%\note{ ~~further notes~~  ~Make other sections like Warning with \section{Warning }{....} ~}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
