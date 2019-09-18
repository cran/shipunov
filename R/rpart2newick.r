Rpart2newick <- function(rpart.object) {
if (is.null(attr(rpart.object, "ylevels"))) stop("I need 'rpart' response as a factor")
fr <- rpart.object$frame
fr$var <- paste(fr$var)
if (nrow(fr) <= 1L) stop("Invalid tree, root only")
tree <- vector("list", 2)
.index <- function(node) {
## node is an integer in rpart's binary ordering system
## outputs a vector of ones and twos for dendrogram indexing
 column <- 1
 multiplier <- 1
 tmp <- node
 while (multiplier * 2 <= node) {
  tmp <- tmp - multiplier
  column <- column + 1
  multiplier <- multiplier * 2
 }
 out <- integer(column - 1)
 for(i in seq_along(out)) {
  multiplier <- multiplier / 2
  if (tmp <= multiplier) {
    out[i] <- 1
  } else {
    out[i] <- 2
    tmp <- tmp - multiplier
  }
 }
out
}
for (i in 2:nrow(fr)) {
 ind <- .index(as.numeric(rownames(fr)[i]))
 ind <- paste0("[[", paste0(ind, collapse = "]][["), "]]")
 if(fr$var[i] == "<leaf>") {
  subtree <- attr(rpart.object, "ylevels")[fr$yval[i]]
  } else {
  subtree <- vector("list", 2)
  }
 eval(parse(text = paste0("tree", ind, " <- subtree")))
}
## this is "list to Newick", based on how as.character() works with lists
return(gsub('\"| ', '', gsub('c\\(|list\\(', '(', paste0('(', paste(tree, collapse=','), ');'))))
}
