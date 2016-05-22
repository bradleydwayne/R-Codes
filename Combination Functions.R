pmaxle <- function(Pool, Num, Max){
  if(Max < Num) return(0)
  else prod(Max:(Max - Num + 1)) / prod(Pool:(Pool - Num + 1))
}

pmaxeq <- function(Pool, Num, Max){
  pmaxle(Pool, Num, Max) - pmaxle(Pool, Num, Max - 1)
}

calccombs <- function(Pool, Num, ordered = FALSE, chosen = NULL){
  if (length(chosen) == Num) return(chosen)
  else if (!is.null(chosen)) {
    if (ordered) i <- (1:Pool)[-chosen]
    else i <- (chosen[length(chosen)] + 1):Pool
  } else i <- c(1:Pool)
  if (sum(is.na((1:Pool)[i])) == 0){
    return(sapply((1:Pool)[i], FUN = function(X) calccombs(Pool, Num, ordered = ordered, chosen = c(chosen, X))))
  } else return(NULL)
}

FindCombs <- function(Pool, Num, ordered = FALSE){
  t(matrix(unlist(calccombs(Pool, Num, ordered)), nrow = Num))
}

NumCombs <- function(Pool, Num, ordered = FALSE){
  n <- prod(Pool:(Pool - Num + 1))
  if (ordered) d <- 1
  else d <- prod(1:Num)
  n/d
}

PullRand <- function(Pool, Num, replace = FALSE, chosen = NULL){
  if (replace || is.null(chosen)) x <- 1:Pool
  else x <- (1:Pool)[-chosen]
  choice <- x[ceiling(runif(1, 0, length(x)))]
  if (length(chosen) + 1 == Num) return(c(chosen, choice))
  else Recall(Pool, Num, replace, c(chosen, choice))
}

ChecksByPool <- function(Hits, Pool){
  length(Hits) * (max(Hits) - length(Hits)) + sum(1:length(Hits))
}

ChecksByHits <- function(Hits){
  sum(Hits)
}

GetExpecteds <- function(Pool, Num){
  sapply(X = (1:Pool), FUN = pmaxeq, Pool = Pool, Num = Num)
}
  
DoSim <- function(Pool, Num, reps){
  sim <- replicate(reps, PullRand(Pool, Num))
  list(ByPool = mean(apply(sim, MARGIN = 2, FUN = ChecksByPool, Pool = Pool)), ByHit = mean(apply(sim, MARGIN = 2, FUN = ChecksByHits)))
}

Pool <- 12
Count <- 10
Data <- data.frame(t(replicate(100, unlist(DoSim(Pool, Count, 1000)))))
lm(formula = ByHit ~ ByPool, Data)
lapply(Data, mean)
plot(Data$ByHit, Data$ByPool)
sum(GetExpecteds(Pool , Count)[Count:Pool] * sapply(Count:Pool, FUN = function(i) ChecksByPool(Hits = i:(i - Count + 1), Pool = Pool)))



