Files <- function(root=getwd(),  multiple=FALSE, hidden=FALSE) {
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
  sel <- menu(lbls, title=sprintf('Select file(s) (0 to quit with dirname or filelist)\n
   Current directory: %s', root))
  if (sel == 0) {
   if (!multiple) files <- root
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
