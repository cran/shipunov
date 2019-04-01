## R functions for multiple recoding

Recode <- function(var, from, to, char=TRUE)
{
 if (char)  if(is.factor(to)) to <- as.character(to)
 x.tmp <- x <- as.vector(var)
 for (i in 1:length(from)){x <- replace(x, x.tmp == from[i], to[i])}
 if(is.factor(var)) factor(x) else x
}

## ===

Recode4 <- function(var, from, to, missed="")
{
 ifelse(Recode(var, from, to) == var, missed, Recode(var, from, to))
}

## ===

RecodeR <- function(var, from, to, char=TRUE)
{
 if (char)  if(is.factor(to)) to <- as.character(to)
 x <- as.vector(var)
 for (i in 1:length(from)){x <- replace(x, x == from[i], to[i])}
 if(is.factor(var)) factor(x) else x
}

## ===

Recode4R <- function(var, from, to, missed="")
{
 ifelse(RecodeR(var, from, to) == var, missed, RecodeR(var, from, to))
}
