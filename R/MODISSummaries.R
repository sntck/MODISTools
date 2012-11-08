MODISSummaries <-
  function(LoadDat, LoadMethod='object' | 'ext.file', FileSep=NULL, Dir='.', Band, ValidRange, NoDataValue, ScaleFactor, QualityControl=0, Mean=TRUE, SD=TRUE, Min=TRUE, Max=TRUE, Yield=TRUE, NoFill=TRUE, PoorQuality=TRUE) 
  { 
    if(LoadMethod == 'object'){ details<- data.frame(LoadDat) }
    if(LoadMethod == 'ext.file'){ details<- read.delim(LoadDat, sep=FileSep) }
    filelist <-list.files(path=Dir, pattern=".asc") # Get a list of all .asc files in the data directory
    band.data.by.site<- list(NA)
    band<- c()
    # Time-series analysis
    for(counter in 1:length(filelist)){
      ############################
      # Load file into data frame, name columns and tell user what's being processed
      ############################
      ifelse(Dir == '.', ds<- read.csv(filelist[counter], header=FALSE,as.is=TRUE), ds<- read.csv(paste(Dir,filelist[counter],sep=''), header=FALSE,as.is=TRUE))
      names(ds)<- c("row.id","land.product.code","MODIS.acq.date","where","MODIS.proc.date",1:(ncol(ds)-5))
      print(paste("Processing file ",counter," of ",length(filelist),"...",sep=""))
      ########## Find location info        
      wS<- regexpr("Samp",ds$where[1])                    
      wL<- regexpr("Line",ds$where[1])
      sample.no<- as.numeric(substr(ds$where[1],wS+4,wL-1))
      line.no<- as.numeric(substr(ds$where[1],wL+4,nchar(ds$where[1])))
      which.are.band<- grep(Band,ds$row.id)
      which.are.reliability<- grep("reliability",ds$row.id)
      wlong<- regexpr("Lon",ds$where[1])
      lat<- as.numeric(substr(ds$where[1],4,wlong-1))
      long<- as.numeric(substr(ds$where[1],wlong+3,wS-1))
      ########## Set out subscripts for spatial organisation
      row.c<- ceiling(sample.no/2)
      col.c<- ceiling(line.no/2)
      mf<- matrix(NA, nrow=sample.no, ncol=line.no)
      centre<- ((row.c-1)*sample.no)+col.c
      d1.subscripts<- c(centre+1,centre-1,centre-(nrow(mf)+1),centre-(nrow(mf)),centre-(nrow(mf)-1),centre+(nrow(mf)-1),centre+(nrow(mf)),centre+(nrow(mf)+1)) 
      d2.subscripts<- c((centre-((nrow(mf)+1)*2)):(centre-((nrow(mf)+1)*2)+4), centre-((nrow(mf)+1)*2)+nrow(mf), centre-((nrow(mf)+1)*2)+(nrow(mf)*2), centre-((nrow(mf)+1)*2)+(nrow(mf)*3), 
                        centre-((nrow(mf)+1)*2)+4+nrow(mf), centre-((nrow(mf)+1)*2)+4+(nrow(mf)*2), centre-((nrow(mf)+1)*2)+4+(nrow(mf)*3), (centre+((nrow(mf)+1)*2)-4):(centre+((nrow(mf)+1)*2))) 
      d3.subscripts<- c((centre-((nrow(mf)+1)*3)):(centre-((nrow(mf)+1)*3)+6), centre-((nrow(mf)+1)*3)+nrow(mf), centre-((nrow(mf)+1)*3)+(nrow(mf)*2), centre-((nrow(mf)+1)*3)+(nrow(mf)*3),
                        centre-((nrow(mf)+1)*3)+(nrow(mf)*4), centre-((nrow(mf)+1)*3)+(nrow(mf)*5), centre-((nrow(mf)+1)*3)+6+nrow(mf), centre-((nrow(mf)+1)*3)+6+(nrow(mf)*2), centre-((nrow(mf)+1)*3)+6+(nrow(mf)*3),
                        centre-((nrow(mf)+1)*3)+6+(nrow(mf)*4), centre-((nrow(mf)+1)*3)+6+(nrow(mf)*5), (centre+((nrow(mf)+1)*3)-6):(centre+((nrow(mf)+1)*3)))
      d4.subscripts<- c((centre-((nrow(mf)+1)*4)):(centre-((nrow(mf)+1)*4)+8), centre-((nrow(mf)+1)*4)+nrow(mf), centre-((nrow(mf)+1)*4)+(nrow(mf)*2), centre-((nrow(mf)+1)*4)+(nrow(mf)*3),
                        centre-((nrow(mf)+1)*4)+(nrow(mf)*4), centre-((nrow(mf)+1)*4)+(nrow(mf)*5), centre-((nrow(mf)+1)*4)+(nrow(mf)*6), centre-((nrow(mf)+1)*4)+(nrow(mf)*7), centre-((nrow(mf)+1)*4)+8+nrow(mf), 
                        centre-((nrow(mf)+1)*4)+8+(nrow(mf)*2), centre-((nrow(mf)+1)*4)+8+(nrow(mf)*3), centre-((nrow(mf)+1)*4)+8+(nrow(mf)*4), centre-((nrow(mf)+1)*4)+8+(nrow(mf)*5), centre-((nrow(mf)+1)*4)+8+(nrow(mf)*6),
                        centre-((nrow(mf)+1)*4)+8+(nrow(mf)*7), (centre+((nrow(mf)+1)*4)-8):(centre+((nrow(mf)+1)*4)))     
      ##########  Spatial organisation
      band.time.series<- c()
      rel.time.series<- c()
      for(n in which.are.band) { 
        pixels.band<- matrix(ds[n,6:ncol(ds)], nrow=sample.no, ncol=line.no, byrow=TRUE)
        band.time.series<- rbind(band.time.series, matrix(pixels.band,nrow=1))
      } 
      for(n in which.are.reliability) { 
        pixels.reliability<- matrix(ds[n,6:ncol(ds)], nrow=sample.no, ncol=line.no, byrow=TRUE)
        rel.time.series<- rbind(rel.time.series, matrix(pixels.reliability,nrow=1))
      } 
      ########## Interpolation
      mean.band<- rep(NA,ncol(band.time.series))
      sd.band<- rep(NA,ncol(band.time.series))
      band.yield<- rep(NA,ncol(band.time.series))
      nofill<- rep(NA,ncol(band.time.series))
      poorquality<- rep(NA,ncol(band.time.series))
      band.min<- rep(NA,ncol(band.time.series))
      band.max<- rep(NA,ncol(band.time.series))
      band.time.series<- matrix(ifelse(band.time.series != NoDataValue & rel.time.series <= QualityControl, band.time.series, NA),nrow=length(which.are.band))
      if(any((band.time.series >= ValidRange[1] && band.time.series <= ValidRange[2]) == FALSE, na.rm=TRUE)) { print('Warning! Some values fall outside the valid range') }
      ds$Year<- as.numeric(substr(ds$MODIS.acq.date,2,5))
      ds$Day<- as.numeric(substr(ds$MODIS.acq.date,6,8))
      ds$date<- strptime(paste(ds$Year,"-",ds$Day,sep=""),"%Y-%j")   # Converts date to POSIXlt format
      for(i in 1:ncol(band.time.series)) {                           # Run time series for each pixel i.
        good<- sum(!is.na(band.time.series[,i]))
        if(good >= 2) {
          sout = approx(x=ds$date[which.are.band], y=as.numeric(band.time.series[,i])*ScaleFactor, method = "linear", n = ((max(ds$date[!is.na(band.time.series[,i])])- min(ds$date[!is.na(band.time.series[,i])]))-1))
          minobsband = min(as.numeric(band.time.series[,i])*ScaleFactor, na.rm = TRUE)    # Minimum value observed
          maxobsband = max(as.numeric(band.time.series[,i])*ScaleFactor, na.rm = TRUE)    # Maximum value observed
          if(Yield == TRUE){ band.yield[i] = (sum(sout$y) - minobsband*length(sout$x)) / length(sout$x) }    #(((365*length(years))-16)*365) # average annual yield  (i.e. work out daily yield * 365
          if(Mean == TRUE){ mean.band[i]<- mean(sout$y) }
          if(SD == TRUE){ sd.band[i]<- sd(sout$y) }
        }
        if(good == 1){
          warning("Only single good value: cannot estimate yield", immediate. = TRUE)
          if(Mean == TRUE){ mean.band[i]<- mean(as.numeric(band.time.series[,i])*ScaleFactor,na.rm=TRUE) } 
        }
        if(good == 0){
          warning("No reliable data for this pixel", immediate. = TRUE)
        }
        if(Min == TRUE){ band.min[i]<- minobsband }
        if(Max == TRUE){ band.max[i]<- maxobsband }
        if(NoFill == TRUE){ nofill[i]<- paste(round((sum(ds[,i+5] == NoDataValue)/length(band.time.series[,i]))*100,2),'% (',sum(ds[,i+5] == NoDataValue),'/',length(band.time.series[,i]),')',sep='') }
        if(PoorQuality == TRUE){ poorquality[i]<- paste(round((sum(rel.time.series[,i] != QualityControl)/length(rel.time.series[,i]))*100,2),'% (',sum(rel.time.series[,i] != QualityControl),'/',length(rel.time.series[,i]),')',sep='') }
      }
      distance.bands<- c()
      distance.bands[centre]<- 1
      if(length(mean.band) >= 9){ distance.bands[d1.subscripts]<- 2 }
      if(length(mean.band) >= 25){ distance.bands[d2.subscripts]<- 3 }
      if(length(mean.band) >= 49){ distance.bands[d3.subscripts]<- 4 }
      if(length(mean.band) == 81){ distance.bands[d4.subscripts]<- 5 }
      if(length(mean.band) > 81){ distance.bands<- c(distance.bands,rep(NA, length(mean.band)-length(distance.bands))) }  
      where.id<- regexpr('_', filelist[counter])
      id<- rep(substr(filelist[counter], 1, where.id-1), length(mean.band))
      final.band<- data.frame(ID=id, lat=rep(lat,length(mean.band)), long=rep(long,length(mean.band)), start.date=rep(min(ds$date),length(mean.band)), end.date=rep(max(ds$date),length(mean.band)),
                              dist.bands=distance.bands, min.band=band.min, max.band=band.max, mean.band=mean.band, sd.band=sd.band, band.yield=band.yield, no.fill.data=nofill, poor.quality.data=poorquality)
      band.data.by.site[[counter]]<- final.band
      band<- rbind(band,final.band[,9])
    }
    print('Writing summaries and collecting data...')
    if(Dir != '.') {
      write.table(band.data.by.site[[1]],file=paste(Dir,'MODIS Summary ',Sys.Date(),'.csv',sep=''),sep=',',row.names=FALSE)
      if(length(filelist) > 1){ for(i in 2:length(filelist)){ write.table(band.data.by.site[[i]],file=paste(Dir,'MODIS Summary.csv',sep=''),sep=',',append=TRUE,row.names=FALSE,col.names=FALSE) } }
    } else {
      write.table(band.data.by.site[[1]],file=paste('MODIS Summary ',Sys.Date(),'.csv',sep=''),sep=',',row.names=FALSE)
      if(length(filelist) > 1){ for(i in 2:length(filelist)){ write.table(band.data.by.site[[i]],file=paste('MODIS Summary ',Sys.Date(),'.csv',sep=''),sep=',',append=TRUE,row.names=FALSE,col.names=FALSE) } }
    }
    timeseries<- matrix(unique(cbind(details$lat[!is.na(details$lat)],details$long[!is.na(details$lat)],details$end.date[!is.na(details$lat)],details$start.date[!is.na(details$lat)])),ncol=4)
    modis<- data.frame(timeseries,band)
    names(modis)<- c('lat','long','start.date','end.date',1:ncol(band))  
    nas<- matrix(NA, nrow=nrow(details),ncol=ncol(band))
    res<- data.frame(details,nas)
    div.set<- matrix(c(round(res$lat,digits=7),round(res$long,digits=5),round(res$end.date),round(res$start.date)),ncol=4)
    for(i in 1:nrow(band)){
      mod.set<- matrix(c(round(modis$lat[i],digits=7),round(modis$long[i],digits=5),round(modis$end.date[i]),round(modis$start.date[i])),ncol=4)
      row.matches<- apply(div.set,1,match,mod.set)
      res[which(!is.na(apply(row.matches,2,sum))),(ncol(details)+1):ncol(res)]<- modis[i,5:ncol(modis)]   
    }
    if(Dir != '.'){
      write.table(res,file=paste(Dir,'MODIS Data ',Sys.Date(),'.csv',sep=''),sep=',',col.names=TRUE,row.names=FALSE)
    } else {
      write.table(res,file=paste('MODIS Data ',Sys.Date(),'.csv',sep=''),sep=',',col.names=TRUE,row.names=FALSE)
    }
    print(paste('Done! Check files "MODIS Summary ',Sys.Date(),'.csv" and "MODIS Data ',Sys.Date(),'.csv".',sep=''))  
    return(res)
  }