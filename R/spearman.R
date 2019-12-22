# years <- c(2007,2016)
# spearman.test(timeseries, c(2007,2016))

spear<-function (x){

  N = length(x)
  S = rank(x,ties.method ="min") # crank(x)
  R = c(1:length(x))

  r = 1-6*sum((R-S)^2)/N/(N^2-1)
  t=r*sqrt((N-2)/(1-r^2))

  p=2*(1-pt(abs(t),N-2)) #
  results=data.frame(r=r,t=t,p=p)
  return(results)
}


spearman.test <- function(timeseries, years){
  yrange <- years
  yrange <- data.frame(range = as.numeric(yrange))
  rownames(yrange) <- c("start", "end")
  ###
  years_sel <- (yrange[1,1]:yrange[2,1])
  timeseries <- timeseries[timeseries$year %in% years_sel , ]
  x=log(timeseries[,2]+1)
  spearman <- spear(x)
  return(spearman)
}
