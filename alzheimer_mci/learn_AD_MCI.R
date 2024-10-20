### NADPark study
library('inferno')

## Set random-generator seed to reproduce results if repeated
seed <- 16

## How many parallel CPUs to use for the computation?
parallel <- 12

## Name of directory where to save what has been "learned"
## a timestamp may be appended to this string
savedir <- 'output_learn_AD_MCI_1'

## Call the main function for "learning"
## it will save everything in the directory outputdir
outputdir <- learn(
    data = 'ingrid_dataupd_nogds6.csv',
    ## subsampledata = 100,
    ## nsamples = 360,
    ## nchains = 8,
    ## maxhours = 0,
    metadata = 'meta_AD_MCI.csv',
    outputdir = savedir,
    appendtimestamp = FALSE,
    appendinfo = FALSE,
    output = 'directory',
    parallel = parallel,
    seed = seed
)
