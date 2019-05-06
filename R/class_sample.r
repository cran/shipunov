Class.sample <- function(labels, nsam) {
 ave(1:length(labels), labels, FUN=function(.x) sample.int(length(.x))) <= nsam
}
