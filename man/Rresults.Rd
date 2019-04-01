\name{Rresults}
\alias{Rresults}
\title{Rresults shell script}
\description{Rresults shell script}

% \usage{}

% \arguments{}

\details{'Rresults' is a bash shell script which allows to gather all R input and R textual output in one text file,
and (unnamed) R graphical output in another (PDF) file. If graphical output is names, it will be saved in its own file(s).

Very good for debugging and other non-interactive working with R scripts as everything is one place.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
cat("\"Hello, world!\"", file="hello.r")
\donttest{
## works only if the script is properly installed
system("Rresults hello.r")
## interactive command
file.show("hello_rresults.txt")
}
}

\keyword{Utilities}

