\name{Class.sample}
\alias{Class.sample}
\title{Samples along the class labels}
\description{Samples within each class separately}

\usage{Class.sample(labels, nsam)}

\arguments{
\item{labels}{Vector of labels to convert into factor}
\item{nsam}{Number of samples to take from each class}
}

\details{'Class.sample()' splits labels into groups in accordance with classes, and samples each of them separately.

If 'nsam' is bigger then class size, the whole class will be sampled.
}

\value{Logical vector of length equal to 'vector'}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
(sam <- Class.sample(iris$Species, 5))
iris.trn <- iris[sam, ]
iris.tst <- iris[-sam, ]
}

\keyword{System}
