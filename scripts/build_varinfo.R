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
    maxvals <- 0
    for(x in dt){
        x <- x[!is.na(x)]
        transformation <- 'identity' # temporary
        if(length(unique(x)) == 2){# seems binary variate
            vtype <- 'B'
            vn <- 2
            vmin <- 0
            vmax <- 1
            tmin <- -Inf
            tmax <- +Inf
            vvals <- as.character(unique(x))
            maxvals <- max(maxvals, vn)
        }else if(!is.numeric(x)){# non-numeric, non-binary = categorical variate
            vtype <- 'C'
            vn <- length(unique(x))
            vmin <- 1 # Nimble index categorical from 1
            vmax <- vn
            tmin <- -Inf
            tmax <- +Inf
            vvals <- as.character(unique(x))
            maxvals <- max(maxvals, vn)
        }else{# integer, continuous, or boundary-singular variate
            ix <- x[!(x %in% range(x))] # exclude boundary values
            repindex <- mean(table(ix)) # check for repeated values
            contindex <- length(unique(diff(sort(unique(ix)))))/length(ix) # check for repeated values
            contflag <- (!is.integer(xx) | diff(range(xx)) > 64)
            if(contflag){ # consider it as continuous
                ## temporary values
                vtype <- 'R'
                vn <- Inf
                vmin <- -Inf
                vmax <- +Inf
                tmin <- -Inf
                tmax <- +Inf
                ##
                if(sum(x == min(x)) > repindex){ # seems to be left-singular
                    vtype <- 'D'
                    tmin <- min(x)
                }
                if(sum(x == max(x)) > repindex){ # seems to be right-singular
                    vtype <- 'D'
                    tmax <- max(x)
                }
                if(all(x > 0)){ # seems to be strictly positive
                    transformation <- 'log'
                    vmin <- 0
                }
            }else{# integer variate
                vtype <- 'I'
                vmin <- min(1,xx)
                vmax <- max(xx)
                vn <- vmax - vmin + 1
        }# end numeric case
            
        }
    }
}



dt <- fread('ingrid_data_nogds6.csv')


dtx <- dt
dtx$extra <- rnorm(nrow(dtx))
dtx <- dtx[sample(1:nrow(dtx), 150)]
t(sapply(dtx, function(xx){
    xx <- xx[!is.na(xx)]
    if(length(unique(xx)) > 2){ix <- xx[!(xx %in% range(xx))]}else{ix <- xx}
    dd0 <- diff(sort(xx))
    dd <- diff(sort(unique(xx)))
    q1 <- tquant(ix,0.25)
    q3 <- tquant(ix,0.75)
    c(
      minc=sum(xx==min(xx)),
      maxc=sum(xx==max(xx)),
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
