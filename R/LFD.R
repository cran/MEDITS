
LFD <- function(merge_TATC,indices,sex,LC,depth_range, type="indices"){

  if(FALSE){
    merge_TATB <-  m.TATB(TA,TB,"ARISFOL")
    merge_TATC <- m.TATC(TA,TC,"ARISFOL")
    GSA <- unique(TA$AREA)
    indices <- index.ts(merge_TATB,GSA,"ARISFOL",index = "abundance",
                        depth_range=c(200,800),sex="f", sampling = "RSS",plot=FALSE)
    sex="f"
    LC=1
    depth_range <- c(200,800)
    type="indices"
  }

  expand.lfd <- function(x, na.strings = "NA", as.is = FALSE, dec = ".") {
    # Take each row in the source data frame table and replicate it   # using the Freq value
    DF <- sapply(1:nrow(x), function(i) x[rep(i, each = x$freq[i]), ],simplify = FALSE)

    # Take the above list and rbind it to create a single DF   # Also subset the result to eliminate the Freq column
    DF <- subset(do.call("rbind", DF), select = -freq)

    # Now apply type.convert to the character coerced factor columns   # to facilitate data type selection for each column
    DF <- as.data.frame(lapply(DF,

                               function(x)
                                 type.convert(as.character(x),
                                              na.strings = na.strings,
                                              as.is = as.is,
                                              dec = dec)))

    # Return data frame
    return(DF)
  }




if (sex=="c"){
ddd <- merge_TATC[merge_TATC$LENGTH_CLASS != -1,]
ddd$n_raised <- ddd$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE * ddd$RAISING_FACTOR
lmin <- min(ddd[!is.na(ddd$LENGTH_CLASS) & ddd$LENGTH_CLASS != -1,"LENGTH_CLASS"])
lmax <- max(ddd[!is.na(ddd$LENGTH_CLASS) & ddd$LENGTH_CLASS != -1,"LENGTH_CLASS"])
years <- (unique(ddd$YEAR))

L_classes <- seq(lmin, lmax, LC)
df_n_LC <- data.frame(matrix(NA, nrow= length(L_classes), ncol=length(years)+1))
colnames(df_n_LC) <- c("Classe", years)
df_n_LC[,1] <- L_classes
L_classes <- data.frame(LC = L_classes)
i=1
for (i in 1:length(years)) {
  dd <- ddd[ddd$YEAR ==years[i] & ddd$MEAN_DEPTH >= depth_range[1] & ddd$MEAN_DEPTH < depth_range[2],]
  n_LC <- aggregate(dd$n_raised, by=list(dd$LENGTH_CLASS), sum)
  colnames(n_LC) <- c("LC", "n")
  df_temp <- merge(L_classes, n_LC,by.x ="LC",  by.y="LC", all.x =TRUE)
  ntot <- sum(df_temp[!is.na(df_temp$n), 2])
  peso_LC <- df_temp/ntot
  ind_year <- indices[indices$year == years[i], 2]
  ind_LC <- peso_LC * ind_year
  for (n in 1:length(ind_LC$LC)) {
    if (is.na(ind_LC$n[n])) {ind_LC$n[n] <- 0} else {ind_LC$n[n] <- ind_LC$n[n]}
  }
  df_n_LC[, i+1] <- ind_LC[,2]
}

i=1
min_lc <-min(merge_TATC$LENGTH_CLASS)
max_lc <-max(merge_TATC$LENGTH_CLASS)
max_freq <- max(df_n_LC[,2:length(df_n_LC)])*1.2

for (i in 1:(length(df_n_LC)-1)){
  df_n_LC_temp <- df_n_LC[,c(1,(i+1))]
  df_n_LC_temp[,3] <- as.numeric(colnames(df_n_LC)[i+1])
  colnames(df_n_LC_temp) <- c("LC", "frequency", "year")

  if (i == 1){ df_n_LC_gg <- df_n_LC_temp } else {
    df_n_LC_gg <- rbind(df_n_LC_gg, df_n_LC_temp)
  }
}
df_n_LC_gg$year <- factor(df_n_LC_gg$year)

colnames(df_n_LC_gg)[3] <- "YEAR"
DF <- df_n_LC_gg
if(type=="indices"){
  p <- ggplot(data=DF, aes(x=LC, y=frequency, group=DF$YEAR, colour=DF$YEAR)) +
  geom_line() +     # Set linetype by sex       # Use larger points, fill with white
  xlab("LC") + ylab(expression(paste("Abundance ", (n/km^2), sep=" "))) + # Set axis labels
  ggtitle("LFD (Combined)") +     # Set title
  xlim(min_lc, max_lc) +
  ylim(0,max_freq) +
  theme_bw()
print(p)
}

colnames(df_n_LC_gg) <- c("LC","freq", "YEAR")
df_n_LC_gg$GSA <- unique(merge_TATC$GSA)
na.strings = "NA"
as.is = FALSE
dec = "."
DF <- expand.lfd(df_n_LC_gg)
DF$YEAR <- as.factor(DF$YEAR)

if (type == "density"){
  p <- ggplot(data=DF, aes(x=LC,colour=DF$YEAR, group=DF$YEAR),n=50) +
    geom_density() +
    facet_wrap( ~GSA, ncol=1, scales="free_y")+
    ggtitle("LDF (Combined)")+
    theme_light()
  print(p)

} else {
  if (type == "boxplot") {
    p <- ggplot(DF, aes(x=DF$YEAR, y=LC)) +
      geom_boxplot(aes(group=DF$YEAR)) +
      facet_wrap( ~ GSA) +
      theme_light()+
      ggtitle("LDF (Combined)")
    print(p)
  }
}
return(df_n_LC)
}


#------------
# FEMALES
#------------
  if (sex=="f"){

ddd <- merge_TATC[merge_TATC$LENGTH_CLASS != -1 & merge_TATC$SEX == "F",]
ddd$n_raised <- ddd$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE * ddd$RAISING_FACTOR
lmin <- min(ddd[!is.na(ddd$LENGTH_CLASS) & ddd$LENGTH_CLASS != -1,"LENGTH_CLASS"])
lmax <- max(ddd[!is.na(ddd$LENGTH_CLASS) & ddd$LENGTH_CLASS != -1,"LENGTH_CLASS"])
years <- (unique(ddd$YEAR))

L_classes <- seq(lmin, lmax, LC)
df_n_LC_M <- data.frame(matrix(NA, nrow= length(L_classes), ncol=length(years)+1))
colnames(df_n_LC_M) <- c("Classe", years)
df_n_LC_M[,1] <- L_classes
L_classes <- data.frame(LC = L_classes)
i=1
for (i in 1:length(years)) {
  dd <- ddd[ddd$YEAR ==years[i] & ddd$MEAN_DEPTH >= depth_range[1] & ddd$MEAN_DEPTH < depth_range[2],]
  n_LC <- aggregate(dd$n_raised, by=list(dd$LENGTH_CLASS), sum)
  colnames(n_LC) <- c("LC", "n")
  df_temp <- merge(L_classes, n_LC,by.x ="LC",  by.y="LC", all.x =TRUE)
  ntot <- sum(df_temp[!is.na(df_temp$n), 2])
  peso_LC <- df_temp/ntot
  ind_year <- indices[indices$year == years[i], 2]
  ind_LC <- peso_LC * ind_year
  for (n in 1:length(ind_LC$LC)) {
    if (is.na(ind_LC$n[n])) {ind_LC$n[n] <- 0} else {ind_LC$n[n] <- ind_LC$n[n]}
  }
  df_n_LC_M[, i+1] <- ind_LC[,2]
}

min_lc <-min(merge_TATC$LENGTH_CLASS)
max_lc <-max(merge_TATC$LENGTH_CLASS)
max_freq <- max(df_n_LC_M[,2:length(df_n_LC_M)])*1.2

for (i in 1:(length(df_n_LC_M)-1)){
  df_n_LC_temp <- df_n_LC_M[,c(1,(i+1))]
  df_n_LC_temp[,3] <- as.numeric(colnames(df_n_LC_M)[i+1])
  colnames(df_n_LC_temp) <- c("LC", "frequency", "year")

  if (i == 1){ df_n_LC_gg <- df_n_LC_temp } else {
    df_n_LC_gg <- rbind(df_n_LC_gg, df_n_LC_temp)
  }
}
df_n_LC_gg$year <- factor(df_n_LC_gg$year)
colnames(df_n_LC_gg)[3] <- "YEAR"
DF <- df_n_LC_gg

if (type=="indices"){
  p <- ggplot(data=DF, aes(x=LC, y=frequency, group=DF$YEAR, colour=DF$YEAR)) +
  geom_line() +     # Set linetype by sex       # Use larger points, fill with white
  xlab("LC") + ylab(expression(paste("Abundance ", (n/km^2), sep=" "))) + # Set axis labels
  ggtitle("LFD (Females)") +     # Set title
  xlim(min_lc, max_lc) +
  ylim(0,max_freq) +
  theme_bw()
print(p)
}

colnames(df_n_LC_gg) <- c("LC","freq", "YEAR")
df_n_LC_gg$GSA <- unique(merge_TATC$GSA)
na.strings = "NA"
as.is = FALSE
dec = "."
DF <- expand.lfd(df_n_LC_gg)
DF$YEAR <- as.factor(DF$YEAR)

if (type == "density"){
  p <- ggplot(data=DF, aes(x=LC,colour=DF$YEAR, group=DF$YEAR),n=50) +
    geom_density() +
    facet_wrap( ~GSA, ncol=1, scales="free_y")+
    ggtitle("LDF (Females)")+
    theme_light()
  print(p)
} else {
  if (type == "boxplot") {
    p <- ggplot(DF, aes(x=DF$YEAR, y=LC)) +
      geom_boxplot(aes(group=DF$YEAR)) +
      facet_wrap( ~ GSA) +
      theme_light()+
      ggtitle("LDF (Females)")
    print(p)
  }
}
return(df_n_LC_M)

  }


#------------
# MALES
#------------
if (sex=="m"){

ddd <- merge_TATC[merge_TATC$LENGTH_CLASS != -1 & merge_TATC$SEX == "M",]
ddd$n_raised <- ddd$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE * ddd$RAISING_FACTOR
lmin <- min(ddd[!is.na(ddd$LENGTH_CLASS) & ddd$LENGTH_CLASS != -1,"LENGTH_CLASS"])
lmax <- max(ddd[!is.na(ddd$LENGTH_CLASS) & ddd$LENGTH_CLASS != -1,"LENGTH_CLASS"])
years <- (unique(ddd$YEAR))

L_classes <- seq(lmin, lmax, LC)
df_n_LC_M <- data.frame(matrix(NA, nrow= length(L_classes), ncol=length(years)+1))
colnames(df_n_LC_M) <- c("Classe", years)
df_n_LC_M[,1] <- L_classes
L_classes <- data.frame(LC = L_classes)
i=1
for (i in 1:length(years)) {
  dd <- ddd[ddd$YEAR ==years[i] & ddd$MEAN_DEPTH >= depth_range[1] & ddd$MEAN_DEPTH < depth_range[2],]
  n_LC <- aggregate(dd$n_raised, by=list(dd$LENGTH_CLASS), sum)
  colnames(n_LC) <- c("LC", "n")
  df_temp <- merge(L_classes, n_LC,by.x ="LC",  by.y="LC", all.x =TRUE)
  ntot <- sum(df_temp[!is.na(df_temp$n), 2])
  peso_LC <- df_temp/ntot
  ind_year <- indices[indices$year == years[i], 2]
  ind_LC <- peso_LC * ind_year
  for (n in 1:length(ind_LC$LC)) {
    if (is.na(ind_LC$n[n])) {ind_LC$n[n] <- 0} else {ind_LC$n[n] <- ind_LC$n[n]}
  }
  df_n_LC_M[, i+1] <- ind_LC[,2]
}

# write.table(df_n_LC_M,paste(wd, "/output/",sspp,"_GSA",GSA,"_LFD_(Males)_RSS.csv", sep=""),sep=";",row.names=F)
min_lc <-min(merge_TATC$LENGTH_CLASS)
max_lc <-max(merge_TATC$LENGTH_CLASS)
max_freq <- max(df_n_LC_M[,2:length(df_n_LC_M)])*1.2

for (i in 1:(length(df_n_LC_M)-1)){
  df_n_LC_temp <- df_n_LC_M[,c(1,(i+1))]
  df_n_LC_temp[,3] <- as.numeric(colnames(df_n_LC_M)[i+1])
  colnames(df_n_LC_temp) <- c("LC", "frequency", "year")

  if (i == 1){ df_n_LC_gg <- df_n_LC_temp } else {
    df_n_LC_gg <- rbind(df_n_LC_gg, df_n_LC_temp)
  }
}
df_n_LC_gg$year <- factor(df_n_LC_gg$year)
colnames(df_n_LC_gg)[3] <- "YEAR"
DF <- df_n_LC_gg

if(type=="indices"){
p <- ggplot(data=df_n_LC_gg, aes(x=LC, y=frequency, group=DF$YEAR, colour=DF$YEAR)) +
  geom_line() +     # Set linetype by sex       # Use larger points, fill with white
  xlab("LC") + ylab(expression(paste("Abundance ", (n/km^2), sep=" "))) + # Set axis labels
  ggtitle("LFD (Males)") +     # Set title
  xlim(min_lc, max_lc) +
  ylim(0,max_freq) +
  theme_bw()
print(p)
}

colnames(df_n_LC_gg) <- c("LC","freq", "YEAR")
df_n_LC_gg$GSA <- unique(merge_TATC$GSA)
na.strings = "NA"
as.is = FALSE
dec = "."
DF <- expand.lfd(df_n_LC_gg)
DF$YEAR <- as.factor(DF$YEAR)

if (type == "density"){
  p <- ggplot(data=DF, aes(x=LC,colour=DF$YEAR, group=DF$YEAR),n=50) +
    geom_density() +
    facet_wrap( ~GSA, ncol=1, scales="free_y")+
    ggtitle("LDF (Males)")+
    theme_light()
  print(p)
} else {
  if (type == "boxplot") {
    p <- ggplot(DF, aes(x=DF$YEAR, y=LC)) +
      geom_boxplot(aes(group=DF$YEAR)) +
      facet_wrap( ~ GSA) +
      theme_light()+
      ggtitle("LDF (Males)")
    print(p)
  }
}
return(df_n_LC_M)

}
}
