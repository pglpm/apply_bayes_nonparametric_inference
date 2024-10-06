#### Prepare metadata
library('inferno')

dat <- 'mmc_combine.csv'

metadatatemplate(data = dat, file = 'meta_mmc_combine_MDS_PBMC_NAD.csv',
    excludevrt = c('SubjectId',
        'Tot.MDS.UPDRS.V2',
        'PBMCs.Me.Nam.V2',
        'NAD.ATP.V2'))
