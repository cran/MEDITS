.q <- function(x,quantile){
  quant=quantile
  w <- aggregate(x[,2],list(G=x[,1]),FUN=sum,na.rm=TRUE)
  x <- as.numeric(as.character(w$G))
  w <- w$x
  if(quant<0.5) {
    x <- -x
    ql <- 1-quant} else {
      ql <- quant
      }
  tot <- sum(w)
  frac <- tot*(1-ql)
  w <- w[order(-x)]
  x <- x[order(-x)]
  somme <- w[1]
  i <- 2
  while(somme < frac){
    somme <- somme + w[i]
    i <- i+1
  }
  lq <- round(x[i] + (x[i-1]-x[i])/w[i-1]*(somme-frac),1)
  f1 <- w[i]/tot
  vlq <- ql*(1-ql)/(f1^2)/tot
  c(quantile=ifelse(quant<0.5,-lq,lq),vlq=vlq)
}


quant <- function(freq, quantile){

  l <- length(colnames(freq))
  years <- colnames(freq)[c(2:l)]
  ly <- length(years)
  table=data.frame(matrix(NA,nrow=2,ncol=(ly)))
i=2
  Dati_temp <- 0
  for (i in c(2:l)){
    Dati_temp <- data.frame(freq[,"Classe"],freq[,i])
    table [1,(i-1)] = round(.q(Dati_temp,quantile)[1],2)
    table [2,(i-1)] = round(.q(Dati_temp,quantile)[2],2) #round(sqrt(sum(Dati_temp[,2]*(Dati_temp[,1]-table[6,i])^2)/sum(Dati_temp[,2])),2)
    Dati_temp[,] = 0
  }
  # table[,1]=c("I quartile","min","max (99%)","LT Median","III quartile","Mean","95th percentile","s.dev")
  rownames(table)=c("percentile","Variance")
  colnames(table)=c(years)
  return(table)
}

# freq <- lfd
# q95(freq)
