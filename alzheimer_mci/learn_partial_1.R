### NADPark study
library('inferno')

## Set random-generator seed to reproduce results if repeated
seed <- 16

## How many parallel CPUs to use for the computation?
parallel <- 2

## Name of directory where to save what has been "learned"
## a timestamp may be appended to this string
savedir <- '__testb'

## Call the main function for "learning"
## it will save everything in the directory outputdir
outputdir <- learn(
    data = 'partialdata_1.csv',
    ## subsampledata = 100,
    nsamples = 360,
    nchains = 2,
    maxhours = 0,
    cleanup = FALSE,
    metadata = 'meta_AD_MCI.csv',
    outputdir = savedir,
    appendtimestamp = FALSE,
    appendinfo = FALSE,
    output = 'directory',
    parallel = parallel,
    seed = seed
)
