Files <- function(root=getwd(), # root directory to explore (default is current working directory)
 multiple=FALSE, # allows multiple files to be selected
 hidden=FALSE) # converts into 'listfiles(all.files=...)'
{
 x <- c(dirname(normalizePath(root)), list.files(root, full.names=TRUE, all.files=hidden))
 isdir <- file.info(x)$isdir
 obj <- sort(isdir, index.return=TRUE, decreasing=TRUE)
 isdir <- obj$x
 x <- x[obj$ix]
 lbls <- sprintf('%s%s', basename(x), ifelse(isdir,'/',''))
 lbls[1] <- sprintf('../ (%s)', basename(x[1]))
 lbls <- append(lbls, 'Enter new name...')
 files <- c()
 sel <- -1
 while (TRUE) {
  sel <- menu(lbls, title=sprintf('Select file(s) (0 to quit with dirname)\n
   Current directory: %s', root))
  if (sel == 0) {
   files <- root
   break
  }
  if (sel == length(lbls)) {
   files <- paste0(root, "/", readline('File name: '))
   break
  }
  if (isdir[sel]) { # directory, browse further
   files <- c(files, Files(x[sel], multiple))
   break
  } else {
   files <- c(files, x[sel]) # file, add to list
   if (!multiple) break
   lbls <- lbls[-sel] # remove selected file from choices
   x <- x[-sel]
   isdir <- isdir[-sel]
  }
 }
 return(files)
}
