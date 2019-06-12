Biokey <- function(data, from=NULL, to=NULL, recalculate=TRUE, internal=FALSE) {
available.from <- c("bracket", "branched", "classif", "indented", "newick", "serial")
available.to <- c("backreferenced", "bracket", "classif","indented", "newick", "serial", "table")
if (!from %in% available.from | !to %in% available.to) 
 stop(paste("'from' must be exactly one of", available.from, "and 'to' exactly one of", available.to))
##
.depth <- function(vec) { # calculate depths
 depths <- numeric(length(vec))
 for(n in 2:length(depths)) {
  if(vec[n] != vec[n-1]) {
   m <- match(vec[n], vec[1:(n-1)])
   if(!is.na(m)) {
    depths[n] <- depths[m]
    } else {
    depths[n] <- depths[n-1] + 1
    }
   } else {
   depths[n] <- depths[n-1]
   }
  }
 depths
}
##
data <- as.data.frame(data, stringsAsFactors=FALSE) # in case 'key' is a matrix
##
if (from == "newick") {
 data <- data[1, 1] #  "newick" string is in cell[1, 1]
 tmp1 <- strsplit(data, "")[[1]]
 tmp2 <- tmp1[!tmp1 %in% c(",", ";")]
 maxrank <- max(table(tmp2)) + 1
 tmp6 <- tmp3 <- Recode(tmp2, c("(", ")"), c(-1, 1))
 tmp4 <- suppressWarnings(as.numeric(tmp3))
 tmp4[is.na(tmp4)] <- 0
 tmp5 <- cumsum(tmp4) + maxrank
 tmp6[tmp3 == "-1"] <- tmp5[tmp3 == "-1"]
 tmp6 <- tmp6[tmp3 != "1"]
 tmp8 <- tmp7 <- tmp6
 tmp7[!grepl("[0-9]+", tmp6)] <- 1 # rank of terminals
 tmp7 <- as.numeric(tmp7)
 nonames <- grep("[0-9]+", tmp6)
 tmp8[nonames] <- as.character(as.roman(nonames)) # fake names of intermediate ranks
 key <- cbind(tmp7, NA, tmp8, NA) # (1) ids are _relative_ ranks
}
if (from == "bracket") {
 key <- cbind(data, suppressWarnings(as.numeric(data[, 3]))) # goto's into 4th column
 key[, 3] <- ifelse(is.na(key[, 4]), key[, 3], "") # 3rd column with terminals only
 for(n in 2:nrow(key)) {
  jump <- key[n-1, 4] # 4th column must contain goto's
  if(!is.na(jump)) {
   first <- key[1:(n-1), ]
   move2 <- (1:nrow(key))[key[, 1] == jump]
   second <- key[move2, ]
   third <- key[-c(1:(n-1), move2), ]
   key <- rbind(first, second, third)
   row.names(key) <- NULL
   }
  }
}
if (from == "branched") { # the most simple format, similar to internal 'key'
 key <- cbind(data, NA) # add fake 4th column
}
if (from == "indented") { # similar to 'branched' but have idents as first column
 key <- cbind(data[, 2:3], NA) # skip indents (they will be recalculated if needed)
}
if (from == "serial") { # similar to branched but ids are two ref colums (id + pair)
 idsf <- paste(data[, 1], data[, 2])
 idsr <- paste(data[, 2], data[, 1])
 newid <- idsf
 for (n in 1:length(idsf)) newid[n] <- newid[which(idsr %in% idsf[n])]
 newid <- as.numeric(factor(newid, levels=unique(newid)))
 key <- cbind(newid, data[, 3:4], NA) # discard old ref columns, add fake 4th column
}
if (from == "classif") {
 if (to != "table") { # we need _relative_ ranks and higher groups separate from terminals
 data[, 1] <- as.numeric(as.factor(data[, 1])) # convert absolute numeric ranks to relative
 data[, 3] <- character(nrow(data))
 higher <- data[, 1] > min(data[, 1])
 data[higher, 3] <- data[higher, 2] # keep higher names in 3rd column (temporary)
 data[higher, 2] <- "" # remove names of all higher groups
 for(n in 1:(nrow(data)-1)) { # insert fake rows to propagate all ranks
  dfs <- data[n, 1] - data[n+1, 1]
  if(dfs > 1) {
   before <- data[1:n, ]
   after <- data[-(1:n), ]
   insert <- data[n, ]
   insert[, 1] <- data[n, 1] - 1 # intermediate rank here
   insert[, 2] <- "" # no terminal here
   insert[, 3] <- as.character(as.roman(n)) # fake higher group description
   data <- rbind(before, insert, after)
   row.names(data) <- NULL
   }
  }
 key <- cbind(data[, 1], data[, 3], data[, 2], NA) # add fake 4th column
} else { # for "table" (maybe also fo keys?), we need to keep original ranks and names
 key <- cbind(data[, 1], NA, data[, 2], NA) # add fake 2nd (classifs do not have descriptions) and 4th columns
}
}
## 'key' is the universal internal type: linear, branched, with four columns: (1) ids, (2) descriptions, (3) terminals (or empty strings), (4) goto's (if any, otherwise NAs)
## column #4 needed to _output_ bracket (e.g., with recalculated numbers or with backreferences)
## at this point, all "from" conversions should finish their output and return this 4-column object
colnames(key) <- c("id", "description", "terminal", "goto")
key[is.na(key[, 3]), 3] <- "" # sometimes, terminals contain NAs instead of empty strings (depends on input format)
##
## recalculation for all key-like inputs:
if (recalculate & from != "classif" & from != "newick") { # "classif" and "newick" do not need recalculation
 newids <- as.numeric(factor(key[, 1], levels=unique(key[, 1]))) # recalculate (1) ids in order of appearance
 newgotos <- Recode(key[, 4], key[, 1], newids) # and (2) goto's
 key[, 1] <- newids
 key[, 4] <- newgotos
}
## "to" part:
##
if (to == "classif") {
 res <- key[, c(1, 3)] # classifs should have ranks a.k.a. ids and terminals
}
if (to == "indented") {
 indents <- .depth(key[, 1])
 res <- cbind(indents, key[, 1:3]) # indents as numbers; to make typographic, convert 'indents' to spaces and add "...", like:
 ## for (i in 1:length(key[, 1])) key[i, 1] <- paste(rep("_", key[i, 1]), collapse="")
 ## ifelse(!is.na(key[, 3]), "...", "")
}
if (to == "serial") {
 refs <- numeric(nrow(key))
 for(n in 1:length(refs)) {
  w <-  which(key[, 1] %in% key[n, 1])
  refs[n] <- w[w != n]
 }
 refs <- cbind(1:length(refs), refs) # refs as two columns (id and pair); to make typographic, add parentheses around 'refs'
 res <- cbind(refs, key[, 2:3]) # add two new id columns and skip 1st column with old ids
}
if (to == "bracket") {
 for (n in 1:nrow(key)) {
 if(key[n, 3] == "") key[n, 3] <- key[n+1, 1] # goto's taken from next step and placed into 3rd column
 }
 res <- key[order(key[, 1]), 1:3] # now theses and anti-theses are together
}
if (to == "backreferenced") {
 back <- numeric(nrow(key))
 for (n in 1:nrow(key)) {
  if(key[n, 3] == "") {
   key[n, 3] <- key[n+1, 1] # goto taken from next step id and placed into 4th column
   back[n+1] <- key[n, 1] # backreference for next step taken from previous step id
  }
 }
 back[back == 0] <- "" # replace skipped back's with empty strings
 key <- cbind(key[, 1], back, key[, 2:3]) # backrefrences as separate character column, to make typographic, add parentheses around 'back'
 res <- key[order(key[, 1]), ] # now theses and anti-theses are together
}
if (to == "newick") {
 indents <- .depth(key[, 1])
 dfs <- indents - c(indents[2:(length(indents))], 0)
 brt <- Recode(sign(dfs), c(-1, 0, 1), c("(", ",", ")"))
 mul <- abs(dfs) + (dfs==0)
 for (i in 1:length(brt)) brt[i] <- paste(rep(brt[i], mul[i]), collapse="")
 tmp0 <- cbind(key[, 3], brt)
 tmp1 <- paste(paste0(t(tmp0), colalpse=""), collapse="")
 tmp2 <- gsub(")(", "),(", tmp1, fixed=TRUE)
 tmp3 <- gsub("([A-z0-9])\\(", "\\1,\\(", tmp2) # 'name(' -> 'name,('
 tmp4 <- gsub("\\)([A-z0-9])", "\\),\\1", tmp3) # ')name' -> '),name'
 tmp5 <- gsub("(,", "(", tmp4, fixed=TRUE) # remove empty terminals
 tmp6 <- gsub(",)", ")", tmp5, fixed=TRUE) # remove empty terminals
 tmp7 <- gsub("()", "", tmp6, fixed=TRUE) # some unknown empties, maybe will not appear with propagated ranks
 tmp8 <- gsub(",,+", ",", tmp7) # remove empty terminals
 tmp9 <- gsub(" ", "_", tmp8, fixed=TRUE) # newick dislikes spaces
 res <- paste0("(", tmp9, ");") # make one group and add newick EOL
}
if (to == "table") {
 allranks <- sort(unique(key[, 1]))
 for (i in 1:length(allranks)) {
  tmp <- key[, 3]
  tmp[(key[, 1] != allranks[i])] <- ""
  tmp <- Fill(tmp)
  assign(allranks[i], tmp)
 }
 res <- do.call(cbind, mget(allranks))
 res <- res[key[, 1] == min(as.numeric(allranks)), ]
 have.empty <- apply(res, 2, function(.x) sum(.x == "")) > 0
 res <- res[, !have.empty] # if the rank did not propagated fully, remove column
}
if (internal) res <- key # allows to output internal 4-column object
res
}

# ===

Numranks <- function(nums=NULL, ranks=NULL, add=NULL, empty="Species") {
 if(!is.null(nums) & !is.null(ranks)) stop("either 'nums' or 'ranks' (but not both) should be specified")
 mat <- matrix(c(
  0.5, "Varietes",
  0.8, "Subspecies",
  1.0, "Species",
  1.2, "Subsectio",
  1.5, "Sectio",
  1.8, "Subgenus",
  2.0, "Genus",
  2.2, "Subtribus",
  2.5, "Tribus",
  2.8, "Subfamilia",
  3.0, "Familia",
  3.2, "Superfamilia",
  3.5, "Infraordo",
  3.8, "Subordo",
  4.0, "Ordo",
  4.2, "Superordo",
  4.5, "Infraclassis",
  4.8, "Subclassis",
  5.0, "Classis",
  5.2, "Superclassis",
  5.8, "Subphylum",
  6.0, "Phylum",
  6.2, "Superphylum",
  6.5, "Infraregnum",
  6.8, "Subregnum",
  7.0, "Regnum",
  7.2, "Superregnum"
  ), ncol=2, byrow=TRUE, dimnames=list(NULL, c("num", "rank")))
 if(!is.null(add)) {
  mat <- rbind(mat, matrix(c(add, names(add)), ncol=2))
 }
 if (is.null(nums) & is.null(ranks)) res <- data.frame(mat)
 if(!is.null(nums) & is.null(ranks)) {
  res <- Recode(nums, mat[, 1], mat[, 2])
 }
 if(is.null(nums) & !is.null(ranks)) {
  ranks <- gsub("[^a-z]", "", tolower(ranks))
  ranks <- Recode(ranks, c("family", "order", "kingdom"), c("familia", "ordo", "regnum"))
  ranks[ranks == ""] <- empty
  .cap <- function(.x) { # from help(toupper)
   sapply(strsplit(.x, split=" "), function(.xx)
    paste0(toupper(substring(.xx, 1, 1)), substring(.xx, 2), collapse=" "))
  }
  res <- as.numeric(RecodeR(.cap(ranks), mat[, 2], mat[, 1]))
 }
res
}
