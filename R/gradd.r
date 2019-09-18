Gradd <- function(model2var, data2var, spacing=75, trnsp=0.3, pch=20, cex=0.2,
 palette=NULL, type="ids", User.Predict=function(model2var, X) {}, ...)
{
X <- expand.grid(data.frame(apply(data2var, 2,
 function(.x) seq(range(.x)[1], range(.x)[2], length.out=spacing))))
if (type == "ids") newids <- predict(model2var, X) else
if (type == "lda") newids <- predict(model2var, X)$class else
if (type == "neuralnet") newids <- apply(neuralnet::compute(model2var, X)$net.result, 1,
 function(.x) model2var[["model.list"]][["response"]][which.max(.x)]) else
if (type == "tree") newids <- predict(model2var, X, type="class") else
if (type == "user") newids <- User.Predict(model2var, X) else stop("Unknown model type")
if (!is.factor(newids)) newids <- as.factor(newids)
if (is.null(palette))
 {
 cols <- as.numeric(newids)
 }
else
 {
 cols <- palette[as.numeric(newids)]
 }
points(X, col=adjustcolor(cols, alpha.f=trnsp), pch=pch, cex=cex, ...)
}
