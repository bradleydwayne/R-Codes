library(magrittr)

choose <- function(choices, chosen){
  if(chosen == 0) return(1)
  else prod(choices:(choices - chosen + 1))/prod(1:chosen)
}

GetProb <- function(choices, chosen, successes){
  if(chosen == successes) failnum  <- 1
  else failnum <- (choices - chosen):(choices - 2 * chosen + successes + 1)
  if(successes == 0) succnum <- 1
  else succnum <- chosen:(chosen - successes + 1)
  prod(succnum) * prod(failnum) / prod(choices:(choices - chosen + 1)) * choose(chosen, successes)
}


Threshold = .9999999
kickers  <- c(1/26, 25/26)
Tkts <- 500000000
TaxRate <- .396
Jackpot  <- 1400000000
CostPerTicket <- 2

JProb <- GetProb(69,5,5)/26
max <- qbinom(Threshold, Tkts, JProb)
splits <- sapply(FUN = dbinom, X = 1:max, prob = JProb, size = Tkts) 
mainprobs <- lapply(FUN = GetProb, X = 5:0, choices = 69, chosen = 5) %>% unlist
probs <- sapply(FUN = `*` , X = kickers, mainprobs)

rawpayouts <- matrix(c(Jackpot, 50000, 100, 7, 4, 4, 1000000, 100, 7, 0, 0, 0),nrow = 6)
taxedpayouts <- rawpayouts * (1 - TaxRate)
payouts <- taxedpayouts
payouts[1] <- payouts[1] * sum(splits/(1:max))/sum(splits) 

(probs *  payouts) %>% sum

1 - ((1 - (probs[which(rawpayouts >= 50000)] %>% sum)) ^ (50000 / CostPerTicket))
