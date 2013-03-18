MODISSummaries <-
  function(LoadDat, LoadMethod="object" | "ext.file", FileSep=NULL, Dir=".", Band, ValidRange, NoDataValue, ScaleFactor, QualityControl=0, Mean=TRUE, SD=TRUE, Min=TRUE, Max=TRUE, Yield=TRUE, NoFill=TRUE, PoorQuality=TRUE) 
  { 
    # Load input time-series data file; external data file, or an R object.
    if(LoadMethod == "object"){ details<- data.frame(LoadDat) }
    if(LoadMethod == "ext.file"){ details<- read.delim(LoadDat, sep=FileSep) }
    # Get a list of all downloaded subset (.asc) files in the data directory.
    filelist<- list.files(path=Dir, pattern=".asc")
    
    # Time-series analysis for each time-series (.asc file) consecutively.
    band.data.by.site<- list(NA)     # Initialise objects to store summarised data.
    band<- c()
    for(counter in 1:length(filelist)){
      # Load selected .asc file into a data frame, name columns and tell user what's being processed.
      ifelse(Dir == ".", ds<- read.csv(filelist[counter], header=FALSE,as.is=TRUE), 
            ds<- read.csv(paste(Dir,filelist[counter],sep=""), header=FALSE,as.is=TRUE))
      names(ds)<- c("row.id","land.product.code","MODIS.acq.date","where","MODIS.proc.date",1:(ncol(ds)-5))
      print(paste("Processing file ",counter," of ",length(filelist),"...",sep=""))
      
      ##### Section that uses the files metadata strings [,1:5] for each time-series to extract necessary information.
      # Find location information from metadata string attached at the beginning of each downloaded set of pixels (i.e.
      # each time-step in the time-series), using regular expression.        
      # Extract lat and long data from the metadata string.
      wherelong<- regexpr("Lon",ds$where[1])
      whereSamp<- regexpr("Samp",ds$where[1])
      lat<- as.numeric(substr(ds$where[1],4,wherelong-1))
      long<- as.numeric(substr(ds$where[1],wherelong+3,whereSamp-1))
      
      # Identify which rows in ds correspond to the product band and which are reliability data.
      which.are.band<- grep(Band,ds$row.id)
      which.are.reliability<- grep("reliability",ds$row.id)
      #####
      
      #  Organise data into matrices containing product band data and another for corresponding reliability data.
      band.time.series<- as.matrix(ds[which.are.band,6:ncol(ds)], nrow=length(which.are.band), ncol=length(6:ncol(ds)))
      rel.time.series<- as.matrix(ds[which.are.reliability,6:ncol(ds)], nrow=length(which.are.reliability), ncol=length(6:ncol(ds))) 
      
      # Screen the pixel values in band.time.series: any pixels whose value correspond to NoDataValue, or whose
      # corresponding pixel in rel.time.series is below the QualityControl threshold, will be replaced with NA so they
      # are not included in time-series analysis.
      band.time.series<- matrix(
        ifelse(band.time.series != NoDataValue & rel.time.series <= QualityControl, band.time.series, NA),
        nrow=length(which.are.band))
      # Final check, that band values all fall within the ValidRange (as defined for given MODIS product band).
      if(any((band.time.series >= ValidRange[1] && band.time.series <= ValidRange[2]) == FALSE, na.rm=TRUE)) { 
        print("Warning! Some values fall outside the valid range") 
      }
      
      # Extract year and day from the metadata and make POSIXlt dates (YYYY-MM-DD), ready for time-series analysis.
      ds$Year<- as.numeric(substr(ds$MODIS.acq.date,2,5))
      ds$Day<- as.numeric(substr(ds$MODIS.acq.date,6,8))
      ds$date<- strptime(paste(ds$Year,"-",ds$Day,sep=""),"%Y-%j")
      
      ########## Interpolation
      # Initialise objects for various summaries.
      mean.band<- rep(NA,ncol(band.time.series))
      sd.band<- rep(NA,ncol(band.time.series))
      band.yield<- rep(NA,ncol(band.time.series))
      nofill<- rep(NA,ncol(band.time.series))
      poorquality<- rep(NA,ncol(band.time.series))
      band.min<- rep(NA,ncol(band.time.series))
      band.max<- rep(NA,ncol(band.time.series))
      
      # Run time-series analysis for the ith pixel.
      for(i in 1:ncol(band.time.series)) {
        # Minimum and maximum band values observed.
        minobsband = min(as.numeric(band.time.series[,i])*ScaleFactor, na.rm = TRUE)    
        maxobsband = max(as.numeric(band.time.series[,i])*ScaleFactor, na.rm = TRUE)
        
        # Assess the quality of data at this time-step by counting the number of data left after screening, and use this
        # assessment to decide how to proceed with analysis for each time-step. 
        good<- sum(!is.na(band.time.series[,i]))
        if(good >= 2) {
          # Linearly interpolate between screened data points, for each pixel, over time.
          sout = approx(x=ds$date[which.are.band], y=as.numeric(band.time.series[,i])*ScaleFactor, method = "linear", 
                n = ((max(ds$date[!is.na(band.time.series[,i])])- min(ds$date[!is.na(band.time.series[,i])]))-1))
          
          # Carry out all the relevant summary analyses, set by options in the function call.
          if(Yield == TRUE){ band.yield[i] = (sum(sout$y) - minobsband*length(sout$x)) / length(sout$x) }    # (((365*length(years))-16)*365) = average annual yield  (i.e. work out daily yield * 365).
          if(Mean == TRUE){ mean.band[i]<- mean(sout$y) }
          if(SD == TRUE){ sd.band[i]<- sd(sout$y) }
        }
        if(good == 1){
          warning("Only single good value: cannot summarise", immediate. = TRUE)
          if(Mean == TRUE){ mean.band[i]<- mean(as.numeric(band.time.series[,i])*ScaleFactor,na.rm=TRUE) } 
        }
        if(good == 0){
          warning("No reliable data for this pixel", immediate. = TRUE)
        }
        
        # Complete final optional summaries, irrespective of data quality.
        if(Min == TRUE){ band.min[i]<- minobsband }
        if(Max == TRUE){ band.max[i]<- maxobsband }
        if(NoFill == TRUE){ 
          nofill[i]<- paste(round((sum(ds[,i+5] == NoDataValue)/length(band.time.series[,i]))*100,2),"% (",
                sum(ds[,i+5] == NoDataValue),"/",length(band.time.series[,i]),")",sep="") 
        }
        if(PoorQuality == TRUE){ 
          poorquality[i]<- paste(round((sum(rel.time.series[,i] != QualityControl)/length(rel.time.series[,i]))*100,2),
                "% (",sum(rel.time.series[,i] != QualityControl),"/",length(rel.time.series[,i]),")",sep="") 
        }
      } # End of loop for time-series summary analysis for each pixel.
      
      # Extract ID for this .asc file time-series so it can be included in final summary output.
      where.id<- regexpr("_", filelist[counter])
      id<- rep(substr(filelist[counter], 1, where.id-1), length(mean.band))
      
      # Compile time-series information and relevant summaries data into a final output listed by-sites (.asc files).
      band.data.by.site[[counter]]<- data.frame(ID=id, lat=rep(lat,length(mean.band)), long=rep(long,length(mean.band)),
              start.date=rep(min(ds$date),length(mean.band)), end.date=rep(max(ds$date),length(mean.band)),
              min.band=band.min, max.band=band.max, mean.band=mean.band, sd.band=sd.band, band.yield=band.yield, 
              no.fill.data=nofill, poor.quality.data=poorquality)
      # Extract mean band values.
      band<- rbind(band,mean.band)
    } # End of loop that reitrates time-series summary for each .asc file.
    
    # Write output summary file by appending summary data from all files, producing one file of summary stats output.
    print("Writing summaries and collecting data...")
    if(Dir != ".") {
      write.table(band.data.by.site[[1]],file=paste(Dir,"MODIS Summary ",Sys.Date(),".csv",sep=""),
            sep=",",row.names=FALSE)
      if(length(filelist) > 1){ 
        for(i in 2:length(filelist)){ 
          write.table(band.data.by.site[[i]],file=paste(Dir,"MODIS Summary.csv",sep=""),sep=",",append=TRUE,
                row.names=FALSE,col.names=FALSE) 
        } 
      }
    } else {
      write.table(band.data.by.site[[1]],file=paste("MODIS Summary ",Sys.Date(),".csv",sep=""),sep=",",row.names=FALSE)
      if(length(filelist) > 1){ 
        for(i in 2:length(filelist)){ 
          write.table(band.data.by.site[[i]],file=paste("MODIS Summary ",Sys.Date(),".csv",sep=""),sep=",",
                append=TRUE,row.names=FALSE,col.names=FALSE) 
        } 
      }
    }
    
    # Following code will append the mean (time-averaged) band values for each pixel, for each time-series, to the
    # original input dataset (details) to produce one file that contains all necessary information in a format
    # useful for statistical modelling and other work in R.
     
    # Create data frame of input data (details) with appended set of empty columns on each row, ready for band data
    # to be matched with all corresponding details rows for each unique time-series.
    res<- data.frame(details, matrix(NA, nrow=nrow(details),ncol=ncol(band)))
    # Create standardised (set digits) set of time-series information, that can be matched with standardised set in
    # the following loop.
    div.set<- matrix(c(round(res$lat,digits=7),round(res$long,digits=5),round(res$end.date),round(res$start.date)),ncol=4)
    # Take time-series information at each row and match it to all corresponding rows in the input dataset (details).
    for(i in 1:nrow(band)){
      mod.set<- matrix(c(round(details$lat[i],digits=7),round(details$long[i],digits=5),round(details$end.date[i]),round(details$start.date[i])),ncol=4)
      row.matches<- apply(div.set,1,match,mod.set)
      res[which(!is.na(apply(row.matches,2,sum))),(ncol(details)+1):ncol(res)]<- band[i,]   
    }
    
    # Write the final appended dataset to a csv file, ready for use, in Dir.
    if(Dir != "."){
      write.table(res,file=paste(Dir,"MODIS Data ",Sys.Date(),".csv",sep=""),sep=",",col.names=TRUE,row.names=FALSE)
    } else {
      write.table(res,file=paste("MODIS Data ",Sys.Date(),".csv",sep=""),sep=",",col.names=TRUE,row.names=FALSE)
    }
    
    print(paste("Done! Check files 'MODIS Summary ",Sys.Date(),".csv' and 'MODIS Data ",Sys.Date(),".csv'.",sep=""))  
  }