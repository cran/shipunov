\name{MrBayes}
\alias{MrBayes}
\title{Calls MrBayes}
\description{A slight improvement of 'ips::mrbayes()'}

\usage{MrBayes(x, file="", nst=6, rates="invgamma", ngammacat=4, nruns=2, ngen=1e+06,
 printfreq=100, samplefreq=10, nchains=4, savebrlens="yes", temp=0.2, burnin=10,
 contype="allcompat", run=FALSE, simple=TRUE, exec="mb-mpi")}

\arguments{
\item{x}{The object to process (must be 'DNAbin' class)}
\item{file}{A character string, giving the name of the MrBayes input file.}
\item{nst}{An integer giving the number of rates in the model of sequence evolution.}
\item{rates}{A character string; allowed are "equal", "gamma", "propinv", "invgamma", and "adgamma"; the default is "equal".}
\item{ngammacat}{An integer; the number rate categories for the discretized Gamma distribution; the default is '4'.}
\item{nruns}{An integer; the number of runs.}
\item{ngen}{An integer; the number of states of the MCMC.}
\item{printfreq}{An integer; the interval between states of the MCMC to be printed on the screen}
\item{samplefreq}{An integer; the interval between states of the MCMC to be sampled.}
\item{nchains}{An integer; number of Metropolis coupled MCMCs in each run.}
\item{savebrlens}{Logical; shall branch lengths be saved.}
\item{temp}{0.2}
\item{burnin}{An integer; the number of samples from the MCMC to be discarded prior to further analysis.}
\item{contype}{A character string; the type of consensus tree calculated from the posterior distribution of trees}{either "halfcompat" (majority-rule consensus tree) or "allcombat" (strict consensus tree).}
\item{run}{Logical; 'run = FALSE' will only print the NEXUS file, 'run = TRUE' will also start the MCMC runs, if the 'path' argument is correctly specified.}
\item{simple}{New option: if TRUE (default), then outputs tree in the format readable by functions from 'ape' package}
\item{exec}{New option: name of UNIX executable (to appow multithreaded version)}
}

\details{
'MrBayes()' is a slight improvement of 'ips::mrbayes()'. Please see its documentation for clarity and other options.

'MrBayes()' has two more options, it also both views and saves output (this works only on UNIX).
}

% \value{}

% references{}

\author{Alexey Shipunov}

\seealso{ips::\code{\link[ips]{mrbayes}}}

\examples{
require(ips)
data(ips.cox1)
x <- ips.cox1[, 100:140]
\dontrun{
## requires MrBayes installation
MrBayes(x, file="cox1", ngen=100, run=TRUE)
}
}

\keyword{Strictly biological}

