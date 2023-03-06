library('data.table')
library('png')
library('foreach')
library('doFuture')
library('doRNG')
registerDoFuture()
cat('\navailableCores: ')
cat(availableCores())
cat('\navailableCores-multicore: ')
cat(availableCores('multicore'))
if(Sys.info()['nodename']=='luca-HP-Z2-G9'){
    ncores <- 20}else{
    ncores <- 6}
cat(paste0('\nusing ',ncores,' cores\n'))
if(ncores>1){
    if(.Platform$OS.type=='unix'){
        plan(multicore, workers=ncores)
    }else{
        plan(multisession, workers=ncores)
    }
}else{
    plan(sequential)
}
    
buildvarinfo <- function(dt){
    gcd2 <- function(a, b){ if (b == 0) a else Recall(b, a %% b) }
    gcd <- function(...) Reduce(gcd2, c(...))
    ##
    dt <- as.data.table(dt)
    maxval <- 0
    varinfo <- data.table()
    for(x in dt){
        x <- x[!is.na(x)]
        transf <- 'identity' # temporary
        Q1 <- NA
        Q2 <- NA
        Q3 <- NA
        if(length(unique(x)) == 2){# seems binary variate
            vtype <- 'B'
            vn <- 2
            vd <- NA
            vmin <- 0
            vmax <- 1
            tmin <- NA
            tmax <- NA
            vval <- as.character(unique(x))
            names(vval) <- paste0('v',1:2)
            maxval <- max(maxval, vn)
            ## location <- 0
            ## scale <- 1
            plotmin <- vmin
            plotmax <- vmax
        }else if(!is.numeric(x)){# categorical variate
            vtype <- 'C'
            vn <- length(unique(x))
            vd <- NA
            vmin <- 1 # Nimble index categorical from 1
            vmax <- vn
            tmin <- NA
            tmax <- NA
            vval <- as.character(unique(x))
            names(vval) <- paste0('v',1:vn)
            maxval <- max(maxval, vn)
            ## location <- 0
            ## scale <- 1
            plotmin <- vmin
            plotmax <- vmax
        }else{# discrete, continuous, or boundary-singular variate
            ud <- unique(signif(diff(sort(unique(x))),6)) # differences
            multi <- 10^(-min(floor(log10(ud))))
            dd <- round(gcd(ud*multi))/multi # greatest common difference
            ##
            Q1 <- quantile(x, probs=0.25, type=6)
            Q2 <- quantile(x, probs=0.5, type=6)
            Q3 <- quantile(x, probs=0.75, type=6)
            if(dd == 0){ # consider it as continuous
                ## temporary values
                vtype <- 'R'
                vn <- Inf
                vd <- 0
                vmin <- -Inf
                vmax <- +Inf
                tmin <- -Inf
                tmax <- +Inf
                ## location <- Q2
                ## scale <- (Q3-Q1)/2
                plotmin <- min(x) - (Q3-Q1)/2
                plotmax <- max(x) + (Q3-Q1)/2
                ##
                ix <- x[!(x %in% range(x))] # exclude boundary values
                repindex <- mean(table(ix)) # average of repeated inner values
                ## contindex <- length(unique(diff(sort(unique(ix)))))/length(ix) # check for repeated values
                if(sum(x == min(x)) > repindex){ # seems to be left-singular
                    vtype <- 'D'
                    tmin <- min(x)
                    plotmin <- tmin
                }
                if(sum(x == max(x)) > repindex){ # seems to be right-singular
                    vtype <- 'D'
                    tmax <- max(x)
                    plotmax <- tmax
                }
                if(all(x > 0)){ # seems to be strictly positive
                    transf <- 'log'
                    vmin <- 0
                    ## location <- log(Q2)
                    ## scale <- (log(Q3) - log(Q1))/2
                    plotmin <- max(vmin, plotmin)
                }
            }else{# integer variate
                vtype <- 'I'
                if(dd >= 1){ # seems originally integer
                    transf <- 'Q'
                    vmin <- min(1, x)
                    vmax <- max(x)
                    vn <- vmax - vmin + 1
                    vd <- 0.5
                    tmin <- NA
                    tmax <- NA
                    ## location <- (vn*vmin-vmax)/(vn-1)
                    ## scale <- (vmax-vmin)/(vn-1)
                    plotmin <- max(vmin, min(x) - IQR(x)/2)
                    plotmax <- max(x)
                }else{ # seems a rounded continuous variate
                    vn <- Inf
                    vd <- dd/2
                    vmin <- -Inf
                    vmax <- +Inf
                    tmin <- -Inf
                    tmax <- +Inf
                    ## location <- Q2
                    ## scale <- (Q3-Q1)/2
                    plotmin <- min(x) - (Q3-Q1)/2
                    plotmax <- max(x) + (Q3-Q1)/2
                    if(all(x > 0)){ # seems to be strictly positive
                        transf <- 'log'
                        vmin <- 0
                        ## location <- log(Q2)
                        ## scale <- (log(Q3) - log(Q1))/2
                        plotmin <- max(vmin, plotmin)
                    }
                }# end rounded
            }# end integer
            vval <- NULL
        }# end numeric
        ##
        print(vval)
        varinfo <- rbind(varinfo,
                         c(list(type=vtype, transf=transf, n=vn, d=vd, min=vmin, max=vmax, tmin=tmin, tmax=tmax, plotmin=plotmin, plotmax=plotmax, Q1=Q1, Q2=Q2, Q3=Q3),
                           as.list(vval)
                         ), fill=TRUE)
    }
    varinfo <- cbind(name=names(dt), varinfo)
    varinfo
}


## gcd <- function(vect){Reduce(function(x,y) ifelse(y, Recall(y, x %% y), x), as.list(vect))}
## gcdm <- function(...){Reduce(function(x,y) ifelse(y, Recall(y, x %% y), x), list(...))}


gcd2 <- function(a, b) {
  if (b == 0) a else Recall(b, a %% b)
}
gcd <- function(...) Reduce(gcd2, c(...))

dt <- fread('ingrid_data_nogds6.csv')
data(iris)
iris <- as.data.table(iris)
iris2 <- iris
iris2$Species <- as.integer(iris2$Species)

dtx <- dt
dtx$extra <- rnorm(nrow(dtx))
dtx <- dtx[sample(1:nrow(dtx), min(10,nrow(dtx)))]
t(sapply(dtx, function(xx){
    xx <- xx[!is.na(xx)]
    testd <- unique(signif(diff(sort(unique(xx))),6))
    multi <- 10^(-min(floor(log10(testd))))
    round(gcd(testd*multi))/multi
}))





dtx <- iris
dtx$extra <- rnorm(nrow(dtx))
dtx <- dtx[sample(1:nrow(dtx), min(150,nrow(dtx)))]
t(sapply(dtx, function(xx){
    xx <- xx[!is.na(xx)]
    if(length(unique(xx)) > 2){ix <- xx[!(xx %in% range(xx))]}else{ix <- xx}
    dd0 <- diff(sort(xx))
    dd <- diff(sort(unique(xx)))
    q1 <- tquant(ix,0.25)
    q3 <- tquant(ix,0.75)
    c(
      left=sum(xx==min(xx)),
      right=sum(xx==max(xx)),
      diffratio=length(unique(dd))/length(dd),
      diffratio0=length(unique(dd0))/length(dd0),
      diffindex=length(unique(dd))/length(xx),
      diffindex0=length(unique(dd0))/length(xx),
      meanrep=mean(table(ix)),
      meanrepiqr=mean(table(ix[ix >= q1 & ix <=q3])),
      iqrrange=IQR(ix)/diff(range(xx)),
      min=min(xx),
      max=max(xx),
      int=is.integer(xx),
      unique=length(unique(xx)),
      uniqueratio=length(unique(xx))/length(xx),
      rg=diff(range(xx)),
      NULL
      )
}))
rm(dtx)

dtx <- dt
dtx$extra <- rnorm(nrow(dtx))
summary(t(sapply(1:100, function(xxx){set.seed(xxx)
    dtx <- dtx[sample(1:nrow(dtx), 10)]
    test <- t(sapply(dtx, function(xx){
        xx <- xx[!is.na(xx)]
        if(length(unique(xx)) > 2){ix <- xx[!(xx %in% range(xx))]}else{ix <- xx}
        dd <- diff(sort(unique(xx)))
        c(
            sum(xx==min(xx)),
            sum(xx==max(xx)),
            length(unique(dd))/length(dd),
            length(unique(dd))/length(xx),
            mean(table(ix))
        )
    }))
    c(sum(test['extra',5] < test[-c(13:14),5]),
      sum(test['extra',4] > test[-c(13:14),4]),
      sum(test['extra',3] > test[-c(13:14),3])
      )
})))

dtx <- dt
dtx$extra <- rnorm(nrow(dtx))
resu <- (t(sapply(1:100, function(xxx){set.seed(xxx)
    dtx <- dtx[sample(1:nrow(dtx), 150)]
    test <- t(sapply(dtx, function(xx){
        xx <- xx[!is.na(xx)]
        if(length(unique(xx)) > 2){ix <- xx[!(xx %in% range(xx))]}else{ix <- xx}
        dd <- diff(sort(unique(xx)))
        c(
            sum(xx==min(xx)),
            sum(xx==max(xx)),
            length(unique(dd))/length(dd),
            length(unique(dd))/length(xx),
            mean(table(ix))
        )
    }))
    c(
      min(test[13:14,4]),
      max(test[-c(13:14),4])
      )
})))
summary(resu)
