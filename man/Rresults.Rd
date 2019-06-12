\name{Rresults}
\alias{Rresults}
\title{Rresults shell script}
\description{Rresults shell script}

% \usage{}

% \arguments{}

\details{'Rresults' is a bash shell script which allows to gather all R input and R textual output in one text file,
and (unnamed) R graphical output in another (PDF) file (only if the 'pdftk' utility is installed).
If graphical output has name(s), it will be saved in its own file(s).

Very good for the debugging and other non-interactive activities with R scripts as everything is one place.

The script has one option "-d" which adds the timestamp to the file name of text results.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
\donttest{
## works only if the script is properly installed
cat("\"Hello, world!\"\n", "plot(1:20)\n", file="hello.r")
system("Rresults hello.r")
system("Rresults -d hello.r")
## interactive command
file.show("hello_rresults.txt")
}
}

\keyword{Utilities}

