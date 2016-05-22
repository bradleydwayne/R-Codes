library('lubridate')

MinIter <- .01
Toler <- MinIter / 2

GetPmtDts <- function(ChgDt, NumMths, PmtDay){
  FirstPmtDt  <- mdy(paste(month(ChgDt) + 1,PmtDay,year(ChgDt), sep = '/'))
  as.Date(FirstPmtDt %m+% months(1:NumMths - 1))
}

GetDaysVect <- function(PmtDts){
  days <- vector(mode = "integer", length = length(PmtDts))
  for(j in 2:length(PmtDts)){
    days[j] <- PmtDts[j] - PmtDts[j - 1]
  }
  return(days)
}

#GetIntFacts <- function(Int, Days){
#  i <- Int/36500
#  i * Days
#}

GetIntRates <- function(IntRates, Beg, End, type = 'simple'){
  Rates <- rbind(IntRates[IntRates$Date == max(IntRates$Date[IntRates$Date <= Beg]), ], IntRates[IntRates$Date > Beg & IntRates$Date < End, ])
  if(nrow(Rates) > 1) Days <- vapply(X = 2:nrow(Rates), FUN = function(i) min(Rates$Date[i], End) - max(Rates$Date[i - 1], Beg), FUN.VALUE = as.vector(0))
  else Days <- NULL
  Days <- append(Days, End - max(Rates$Date[nrow(Rates)], Beg))
  Factor <- max(Days , 0) * Rates$Rate/36500
  return(data.frame(Days, Rate = Rates$Rate, Factor))
}

GetIntFactor <- function(IntRates, Beg, End, type = 'simple'){
  sum(GetIntRates(IntRates, Beg, End, type = type)$Factor)
}

GetIntFacts <- function(IntRates, PmtDts){
  vapply(X = 2:length(PmtDts), FUN = function(i) GetIntFactor(IntRates, Beg = PmtDts[i - 1], End = PmtDts[i]), FUN.VALUE = as.vector(0))
}

GetBals <- function(Chg, IntFacts, Pmt){
  bal <- vector(mode = 'numeric', length = length(IntFacts) + 1)
  bal[1] <- Chg
  #  sapply(X = 1:length(PmtDts) + 1, FUN = )
  for(j in 2:length(bal)){
    bal[j] <- bal[j - 1] * (1 + IntFacts[j - 1]) - Pmt
  }
  return(bal)
}

CalcPmt <- function(Chg, Int, PmtDay, ChgDt, NumMths){
  PmtDts <- GetPmtDts(ChgDt, NumMths, PmtDay)
  idays <- GetIntFacts(Int, c(ChgDt, PmtDts))
  idays[1] <- 0
  iter <- 2
  Pmts <- c(round(Chg / NumMths, 2), 0)
  FinalBal <- c(0, Chg)
  bals <- list(c(Chg, rep(0,NumMths)), rep(Chg, NumMths + 1))
  while(abs(FinalBal[iter]) >= Toler){
    iter <- iter + 1
    RawDiff <- FinalBal[iter - 1] * (Pmts[iter - 1] - Pmts[iter - 2]) / (FinalBal[iter - 2] - FinalBal[iter - 1])
    Pmts <- append(Pmts, round(Pmts[iter - 1] + max(abs(RawDiff), MinIter) * RawDiff / abs(RawDiff), 2))
    bals <- append(bals, list(GetBals(Chg, idays, Pmts[iter])))
    FinalBal <- append(FinalBal, bals[[iter]][length(idays) + 1])
    if(sum(Pmts[iter] == Pmts[-c(1:2)]) != 1) break
  }
  Summ <- data.frame(Payments = Pmts[-c(1:2)], Balances = FinalBal[-c(1:2)])
  return(Summ)
}

GetPmt <- function(Chg, Int, PmtDay, ChgDt, NumMths){
  Results <- CalcPmt(Chg, Int, PmtDay, ChgDt, NumMths)
  Results <- Results[Results$Balances <= Toler, ]
  return(unique(Results$Payments[abs(Results$Balances) == min(abs(Results$Balances))]))
}

ProjPmt <- function(Chg, Int, PmtDay, ChgDt, Pmt){
  FirstPmtDt  <- as.Date(mdy(paste(month(ChgDt) + 1,PmtDay,year(ChgDt), sep = '/')))
#  i <- Int/36500
  Proj <- data.frame(Dates = ChgDt, Chg = Chg, Interest = 0, Payment = 0, Balance = Chg)
  i <- 0
  pridate <- FirstPmtDt
  pribal <- Chg
  while(pribal > 0){
    date <- as.Date(FirstPmtDt %m+% months(i))
    days <- as.numeric(date - pridate)
    int <- round(GetIntFactor(Int, pridate, date) * Proj$Balance[nrow(Proj)], 2)
    bal <- max(pribal + int - Pmt, 0)
    Proj <- rbind(Proj, data.frame(Dates = date,  Chg = 0, Interest = int, Payment = min(Pmt, pribal + int), Balance = bal))
    pribal <- bal
    pridate <- date
    i <- i + 1
  }
  return(Proj)
}

SummProj <- function(Proj){
  Summ <- list(NumMonths = sum(Proj$Payment != 0), Payment = max(Proj$Payment), Principal = sum(Proj$Chg), TotPaid = sum(Proj$Payment), TotInt = sum(Proj$Interest), PercFin = paste(round(100 * sum(Proj$Interest) / sum(Proj$Chg), 2),'%', sep = ''))
  names(Summ) <- c('Num Months', 'Payment', 'Principal Paid', 'Total Paid', 'Total Interest Paid', '% Total Interest')
  return(Summ)
}

CalcIntRate <- function(Chg, Pmt, PmtDay, ChgDt, NumMths){
  int <- round(Chg / NumMths, 2)
  PmtDts <- GetPmtDts(ChgDt, NumMths, PmtDay)
  Pmts <- rep(Pmt, NumMths)
  int <- c(1, (((1 + Pmt / Chg)^(1/12)) - 1) * 100)
  FinalBal <- c(0, Chg)
  bals <- list(c(Chg, rep(0,NumMths)), rep(Chg, NumMths + 1))
  iter <- 2
  while(abs(FinalBal[iter]) >= Toler){
    iter <- iter + 1
    RawDiff <- FinalBal[iter - 1] * (int[iter - 1] - int[iter - 2]) / (FinalBal[iter - 2] - FinalBal[iter - 1])
    int <- append(int, round(int[iter - 1] + max(abs(RawDiff), MinIter) * RawDiff / abs(RawDiff), 2))
    Int <- list(Date = as.Date('1/1/1970', format = '%m/%d/%Y'), Rate = int[iter])
    browser()
    bals <- append(bals, list(GetBals(Chg, GetIntFacts(Int, c(ChgDt, PmtDts)), Int)))
    FinalBal <- append(FinalBal, bals[[iter]][length(idays) + 1])
    if(sum(int[iter] == int[-c(1:2)]) != 1) break
  }
  Summ <- data.frame(Payments = int[-c(1:2)], Balances = FinalBal[-c(1:2)])
  return(Summ)
}

MergeProjs <- function(){
  
}


