Readxl <- function(Head = TRUE, as.is = FALSE){
  read.table(file = 'clipboard', sep = '\t', header = Head, comment.char = '', as.is = as.is)
}

Copyxl <- function(x, col.names = TRUE, row.names = FALSE){ 
  if(row.names) x <- cbind(rownames(x), x)
  if(col.names) x <- rbind(colnames(x), x)
  write.table(x = x, file = 'clipboard', sep = '\t', row.names = FALSE, col.names = FALSE)
}



