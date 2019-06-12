MrBayes <- function(x, file="", nst=6, rates="invgamma", ngammacat=4,
 nruns=2, ngen=1e+06, printfreq=100, samplefreq=10,
 nchains=4, savebrlens="yes", temp=0.2, burnin=10,
 contype="allcompat", run=FALSE,
 simple=TRUE, exec="mb-mpi", method="dna")                            # three new options
{
 requireNamespace("ips")
 method <- match.arg(method, choices=c("dna", "mixed"))
 if (method == "dna") {
 if (!inherits(x, "DNAbin"))
  stop("object 'x' is not of class 'DNAbin'")
 bayes <- c("\nbegin mrbayes;", paste("\tlset nst=", nst,
  " rates=", rates, " ngammacat=", ngammacat, ";", sep=""),
  paste("\tmcmc nruns=", nruns, " ngen=", as.integer(ngen),
   " printfreq=", printfreq, " samplefreq=", samplefreq,
   " nchains=", nchains, " savebrlens=", savebrlens,
   " temp=", temp, ";", sep=""), paste("\tsumt filename=",
   file, " burnin=", burnin, " contype=", contype,
   if(simple) { " conformat=simple;" },                                # 'simple format' allows to import node labels
   sep=""), "end;")
 if (file == "") {
  nexus <- ips::write.nex(x, interleave=FALSE)
  nexus <- c(nexus, bayes)
  cat(bayes, sep="\n")
 }
 else {
  nexus <- ips::write.nex(x, file="", interleave=FALSE)
  nexus <- c(nexus, bayes)
  write(nexus, file=file)
 }
 if (run) {
  if (.Platform$OS.type == "unix") {
   system(paste(exec, file, "| tee -a", paste0(file, ".out")))         # use 'exec' and view _and_ save output (into specific files)
  }
  else {
   system(paste("mrbayes ", file, ".bayes", sep=""))
  }
  tr <- ape::read.nexus(paste(file, ".con.tre", sep=""))
 }
 }
 if (method == "mixed") {
 ntax <- dim(x)[1]
 ncha <- dim(x)[2] # stantard data should be after DNA
 p2 <- min(which(x[1, ] %in% c("0", "1")))
 datatype <- paste("datatype=mixed(DNA:1-", p2-1, ",Standard:", p2, "-", ncha, ")", sep="")
 if ((p2-1) == 0) datatype <- paste("datatype=mixed(Standard:", p2, "-", ncha, ")", sep="")
 nexus <- vector(length=ntax + 11)
 nexus[1] <- "#NEXUS"
 nexus[c(2, 6, 8, ntax + 11)] <- ""
 nexus[3] <- "begin data;"
 nexus[4] <- paste("\tdimensions ntax=", ntax, " nchar=",
  ncha, ";", sep="")
 nexus[5] <- paste("\tformat", datatype, "missing=N gap=-;")
 nexus[7] <- "matrix"
 for (i in 1:ntax) {
  s <- paste(x[i, ], collapse="")
  s <- paste(rownames(x)[i], toupper(s))
  nexus[i + 8] <- s
 }
 nexus[ntax + 9] <- ";"
 nexus[ntax + 10] <- "end;"
 bayes <- vector(length=4)
 bayes[1] <- "begin mrbayes;"
 bayes[2] <- paste("\tlset nst=", nst, " rates=", rates, " ngammacat=",
  ngammacat, ";", sep="")
 bayes[3] <- paste("\tmcmc nruns=", nruns, " ngen=", as.integer(ngen),
  " printfreq=", printfreq, " samplefreq=", samplefreq,
  " nchains=", nchains, " savebrlens=", savebrlens, " temp=",
  temp, ";", sep="")
 bayes[4] <- paste("\tsumt filename=",
   file, " burnin=", burnin, " contype=", contype,
   if(simple) { " conformat=simple;" },                                # 'simple format' allows to import node labels
   "end;", sep="")
 nexus <- c(nexus, bayes)
 write(nexus, file)
 if (run) {
  if (.Platform$OS.type == "unix") {
   system(paste(exec, file, "| tee -a", paste0(file, ".out")))         # use 'exec' and view _and_ save output (into specific files)
  }
  else {
   system(paste("mrbayes ", file, ".bayes", sep=""))
  }
  tr <- ape::read.nexus(paste(file, ".con.tre", sep=""))
 }
 }
 return(tr)
}
