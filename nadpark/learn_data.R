### NADPark study
library('inferno')

## Set random-generator seed to reproduce results if repeated
seed <- 16

## How many parallel CPUs to use for the computation?
parallel <- 12

## Name of directory where to save what has been "learned"
## a timestamp may be appended to this string
savedir <- 'output_learn_MDS_PBMC_NAD_iqr2'

## Call the main function for "learning"
## it will save everything in the directory outputdir
outputdir <- learn(
    data = 'mmc_combine.csv',
    metadata = 'meta_mmc_combine_MDS_PBMC_NAD.csv',
    outputdir = savedir,
    appendtimestamp = FALSE,
    appendinfo = FALSE,
    output = 'directory',
    parallel = parallel,
    seed = seed
)
