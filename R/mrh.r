MRH <- function(hcl, dim=NULL, method="groups") {
method <- match.arg(method, choices=c("groups", "height", "cophenetic"))
h <- hcl$height
num <- length(h) + 1
if(method=="groups"){
if(is.null(dim)) dim <- (num-1):2 # margin k give no information
res <- cutree(hcl, k=dim)
}
if(method=="height"){
if(is.null(dim)) dim <- num-1 # this makes it similar to cmdscale, but better is to set large number manually
heights <- seq(max(h), min(h), length.out=(dim+2))[2:(dim+1)] # take out marginal heights
res <- cutree(hcl, h=heights)
}
if(method=="cophenetic"){
if(is.null(dim)) dim <- num-1 # must be 'n-1' for cmdscale()
res <- cmdscale(cophenetic(hcl), k=dim)
}
return(res)
}
