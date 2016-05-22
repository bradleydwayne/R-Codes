RawPerms <- function(Data, Count = nrow(Data), oper = sum, reuse = FALSE, PriorChoices = NULL, sep = ","){
#  browser()
  if (is.vector(Data)) Data <- as.matrix(Data, ncol = 1)
  if (is.null(rownames(Data))) rownames(Data)  <- 1:nrow(Data)
  if (Count == 0)  cbind(paste(PriorChoices[,1], collapse = sep), paste(PriorChoices[,2], collapse = sep), do.call(oper, list(PriorChoices[,2])))
  else {
    if (reuse) lapply(1:nrow(Data), function(i) RawPerms(Data =  Data, Count = Count - 1, oper = oper, reuse = reuse, PriorChoices = rbind(PriorChoices, data.frame(rownames(Data)[i], Data[i, ]))))
    else lapply(1:nrow(Data), function(i) RawPerms(Data =  Data[-i, ], Count = Count - 1, oper = oper, reuse = reuse, PriorChoices = rbind(PriorChoices, data.frame(rownames(Data)[i], Data[i, ]))))
  }
}

OrderChoices <- function(Choices, sep = ","){
  sapply(X = strsplit(Choices, split = sep), FUN = function(x) paste(x[order(x)], collapse = sep), simplify = TRUE, USE.NAMES = FALSE)
}

CalcPerms <- function(Data, Count = nrow(Data), oper = "sum", reuse = FALSE, ordered = FALSE, sep = ","){  
#  browser()
  MatxResults <- t(matrix(unlist(RawPerms(Data, Count = Count, oper = oper, reuse = reuse)),nrow = 3))
  if(!ordered){
    MatxResults <- MatxResults[!duplicated(OrderChoices(MatxResults[, 1], sep = sep)), ]    
    MatxResults[,1] <- OrderChoices(MatxResults[,1], sep = sep)
    MatxResults[,2] <- OrderChoices(MatxResults[,2], sep = sep)
  }
  X <- data.frame(Selections = MatxResults[,1], Values = MatxResults[,2], as.numeric(MatxResults[, 3]))
  colnames(X)[3] <- oper
  X
}

PermArr <- function(Data, Count = nrow(Data), oper = '+', Prior = NULL){
  if (is.vector(Data)) Data <- as.matrix(Data, ncol = 1)
  if (Count != 1) Prior <- PermArr(Data = Data, Count = Count - 1, oper = oper, Prior = Prior)
  if (is.null(Prior)) Data
  else drop(apply(X = Prior, FUN = oper, MARGIN = 1:length(dim(Prior)), y = Data))
}


GetPerms <- function(Data, Count, Target, oper = '+', reuse){
  which(PermArr(Data, Count, oper) == Target, arr.ind = TRUE)
}

FindPerm <- function(Data, Count = nrow(Data), Target, oper = "sum", reuse = FALSE, ordered = FALSE, sep = ","){
  x <- CalcPerms(Data = Data, Count = Count, oper = oper, reuse = reuse, ordered = ordered, sep = sep)
  x[x[,3] == Target, ]
}

AllUnique <- function(x){
  length(x) == length(unique(x))
}

GetIndArr <- function(dims, index){
  sapply(X = 1:length(dims),FUN = getindlev, dims = dims, index = index )
}

getindlev <- function(i, dims, index){
  x <- index %% prod(dims[1:i])
  if (x == 0) x <- prod(dims[1:i])
  if (i !=1) x <- ceiling(x / prod(dims[1:(i-1)]))
  x
}


