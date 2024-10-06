### We now learn from the data.
###
### First we load the 'inferno' package.
### Check the README in
### https://github.com/pglpm/inferno
### for installation instructions.
library('inferno')

## Set random-generator seed to reproduce results if repeated
seed <- 16

## How many parallel CPUs to use for the computation?
parallel <- 12

## Name of directory where to save what has been "learned"
## a timestamp may be appended to this string
savedir <- 'output_learn_naddiff-1'

## Call the main function for "learning"
## it will save everything in the directory outputdir
outputdir <- learn(
    data = 'dataset_demographics_NAD2.csv',
    metadata = 'metadata_demographics_NAD_diff.csv',
    outputdir = savedir,
    appendtimestamp = FALSE,
    appendinfo = FALSE,
    output = 'directory',
    parallel = parallel,
    seed = seed
)
