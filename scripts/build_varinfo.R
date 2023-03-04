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
            vmin <- 0
            vmax <- 1
            tmin <- NA
            tmax <- NA
            vval <- as.character(unique(x))
            maxval <- max(maxval, vn)
            location <- 0
            scale <- 1
            plotmin <- vmin
            plotmax <- vmax
        }else if(!is.numeric(x)){# categorical variate
            vtype <- 'C'
            vn <- length(unique(x))
            vmin <- 1 # Nimble index categorical from 1
            vmax <- vn
            tmin <- NA
            tmax <- NA
            vval <- as.character(unique(x))
            maxval <- max(maxval, vn)
            location <- 0
            scale <- 1
            plotmin <- vmin
            plotmax <- vmax
        }else{# integer, continuous, or boundary-singular variate
            ix <- x[!(x %in% range(x))] # exclude boundary values
            repindex <- mean(table(ix)) # check for repeated values
            ## contindex <- length(unique(diff(sort(unique(ix)))))/length(ix) # check for repeated values
            contflag <- (!is.integer(x) | diff(range(x)) > 64)
            Q1 <- quantile(x, probs=0.25, type=6)
            Q2 <- quantile(x, probs=0.5, type=6)
            Q3 <- quantile(x, probs=0.75, type=6)
            if(contflag){ # consider it as continuous
                ## temporary values
                vtype <- 'R'
                vn <- Inf
                vmin <- -Inf
                vmax <- +Inf
                tmin <- -Inf
                tmax <- +Inf
                location <- median(x)
                scale <- mad(x, constant=1)
                plotmin <- min(x) - IQR(x)/2
                plotmax <- max(x) + IQR(x)/2
                ##
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
                    location <- median(log(x))
                    scale <- mad(log(x), constant=1)
                    plotmin <- max(vmin, plotmin)
                }
            }else{# integer variate
                vtype <- 'I'
                vmin <- min(1, x)
                vmax <- max(x)
                vn <- vmax - vmin + 1
                tmin <- NA
                tmax <- NA
                location <- (vn*vmin-vmax)/(vn-1)
                scale <- (vmax-vmin)/(vn-1)
                plotmin <- max(vmin, min(x) - IQR(x)/2)
                plotmax <- max(x)
            }# end integer
        }# end numeric
        ##
        varinfo <- rbind(varinfo,
                         list(type=vtype, transf=transf, n=vn, min=vmin, max=vmax, tmin=tmin, tmax=tmax, location=location, scale=scale, plotmin=plotmin, plotmax=plotmax, Q1=Q1, Q2=Q2, Q3=Q3)
                         )
    }
    varinfo <- cbind(name=names(dt), varinfo)
    varinfo
}


gcd <- function(vect){Reduce(function(x,y) ifelse(y, Recall(y, x %% y), x), as.list(vect))}
gcdm <- function(...){Reduce(function(x,y) ifelse(y, Recall(y, x %% y), x), list(...))}


dt <- fread('ingrid_data_nogds6.csv')
data(iris)
iris <- as.data.table(iris)
iris2 <- iris
iris2$Species <- as.integer(iris2$Species)

dtx <- iris2
dtx$extra <- rnorm(nrow(dtx))
dtx <- dtx[sample(1:nrow(dtx), min(10,nrow(dtx)))]
t(sapply(dtx, function(xx){
    xx <- xx[!is.na(xx)]
    testd <- sort(unique(signif(diff(sort(unique(xx))),6)))
    gcd(testd*1e6)/1e6
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
