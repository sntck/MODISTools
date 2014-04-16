ProductSummarise <-
function(Product, Band, BandData, QualityScreen, ...)
{
    if(!(Product %in% GetProducts())) stop("Product entered cannot be found.")
    
    if(Product == "MCD12Q1" | Product == "MCD12Q2"){
      stop(cat("Land cover data (MCD12Q1) should be summarised using the LandCover function.\n
               Land cover dynamics (MCD12Q2) cannot be summarised."))
      
    } else if(Product == "MOD16A2"){
      stop("Evapotranspiration (MOD16A2) data cannot be summarised using MODISSummaries().")
      
    } else if(Product == "MCD43A1" | Product == "MCD43A2" | Product == "MCD43A4" |
                Product == "MOD09A1" | Product == "MYD09A1"){
      stop(cat("Raw reflectance data (MOD09/MYD09 or MCD43) cannot be summarised here,\n
               but MODISTimeSeries() can aid handling raw data, which can be used to derive measures 
               from scratch."))
      
    } else if(Product == "MOD11A2" | Product == "MYD11A2"){
      # Surface temperature and emissivity.
      deg.days <- rep(NA, ncol(BandData))
      period.above.threshold <- rep(NA, ncol(BandData))
      
    } else if(Product == "MOD13Q1" | Product == "MYD13Q1"){
      # Vegetation indices.
      integrated.vi <- rep(NA, ncol(BandData))
      season.shift.index <- rep(NA, ncol(BandData))
      change.vector.mag <- rep(NA, ncol(BandData))
      
    } else if(Product == "MOD15A2" | Product == "MOD15A2GFS" | Product == "MYD15A2"){
      # Leaf Area Index (LAI) and Fraction of Photosynthetically Active Radiation (FPAR).
      integrated.lai <- rep(NA, ncol(BandData))
      integrated.fpar <- rep(NA, ncol(BandData))
      
    } else if(Product == "MOD17A2_51" | Product == "MOD17A3"){
      # Gross Primary Productivity (GPP) and net photosynthesis.
      total.productivity <- rep(NA, ncol(BandData))
      peak.npp <- rep(NA, ncol(BandData))
      gpp.seasonality <- rep(NA, ncol(BandData))
      
    }
    
    # Generic summaries
    data.min <- rep(NA, ncol(BandData))
    data.max <- rep(NA, ncol(BandData))
    data.mean <- rep(NA, ncol(BandData))
    data.median <- rep(NA, ncol(BandData))
    data.sd <- rep(NA, ncol(BandData))
    poor.quality <- rep(NA, ncol(BandData))
    no.fill <- rep(NA, ncol(BandData))
    
}