\name{Files}
\alias{Files}
\title{Textual filesystem browser}

\description{Textual filesystem browser}

\usage{Files(root=getwd(), multiple=FALSE, hidden=FALSE)}

\arguments{
\item{root}{Root directory}
\item{multiple}{Allows multiple files to be selected}
\item{hidden}{Show hidden files?}
}

\details{
Interactive text-based file chooser dialog.
Alternatives for Linux: tcltk::tk_choose.files() and tcltk::tk_choose.dir()
}

\value{Returns character vector of selected files, or dirname (useful for 'setwd()'), or new user-defined file name with full path.}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{setwd}}, \code{\link{getwd}}, \code{\link{dir}}}

\examples{
\donttest{
## interactive commands
setwd <- Files() # then select directory to work in
Files("~", hidden=TRUE) # explore home directory with hidden files (Linux, macOS)
}
}

\keyword{System}
