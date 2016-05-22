IsPrime <- function(x){ 
  d <- 2:ceiling(sqrt(x))
  r <-  x %% d
  all(r != 0)
}

IsPrime(1007)
IsPrime <- function(x) all(x %% 2:ceiling(sqrt(x)) != 0)

FindDivs <- function(x) {
  if(IsPrime(x)) return 'Prime'
  else {
    d <- 2:ceiling(sqrt(x))
  }
}

