data(SubsetExample)
MODISSubsets(LoadDat=SubsetExample, LoadMethod='object', Product='MOD13Q1',
    Bands=c('250m_16_days_EVI','250m_16_days_pixel_reliability'),
    Size=c(0,0), StartDate=TRUE, DateFormat='year')
