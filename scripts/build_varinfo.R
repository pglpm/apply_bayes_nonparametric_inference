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
    for(x in dt){
        x <- x[!is.na(x)]
        transformation <- 'identity' # temporary
        if(length(unique(x)) == 2){# seems binary variate
            type <- 'B'
            xv <- as.integer(as.factor(x))-1
            min <- 
        }
        if(!is.numeric(x)){# non-numeric variate

        }
        if(is.numeric(x)){# numeric variate
            ix <- setdiff(x, range(x)) # exclude boundary values
            ## contindex <- mean(table(x)) # check for repeated values
            contindex <- length(unique(diff(sort(unique(ix)))))/length(ix)  # check for repeated values
            if(contindex < 0.9){ # consider it as continuous
                type <- 'R'
                min <- -Inf # temporary
                max <- +Inf # temporary
                ##
                if(sum(x == min(x)) > contindex){ # seems to be left-singular
                    type <- 'D'
                    tmin <- min(x)
                }
                if(sum(x == max(x)) > contindex){ # seems to be right-singular
                    type <- 'D'
                    tmax <- max(x)
                }
                if(all(x > 0)){ # seems to be strictly positive
                    transformation <- 'log'
                }
            }# end continuous case
            
        }# end numeric case
            
        }
    }
}
