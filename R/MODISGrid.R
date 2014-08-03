MODISGrid <-
function(Dir = ".", DirName = "MODIS_GRID", SubDir = TRUE, NoDataFill)
{
  # DEFINE
  NUM_METADATA_COLS <- 10
  
  if(Dir == '.') cat('Files downloaded will be written to ', file.path(getwd(), DirName), '.\n', sep = '')
  if(Dir != '.') cat('Files downloaded will be written to ', file.path(SaveDir, DirName), '.\n', sep = '')
  
  # Create directory for storing new grid files.
  if(!file.exists(file.path(Dir, DirName))) dir.create(path = file.path(Dir, DirName))
  
  # Find all MODIS data files in Dir
  file.list <- list.files(path = Dir, pattern = ".asc$")
  file.list <- file.list[grepl("___", file.list)]
  if(length(file.list) == 0) stop("Could not find any MODIS data files in directory specified.")
  
  for(i in 1:length(file.list)){
    
    cat("Creating new GIS ASCII files from MODIS data file", i, "out of", length(file.list), "\n")
    
    data.file <- read.csv(file.path(Dir, file.list[i]), header = FALSE, as.is = TRUE)
    names(data.file) <- c("nrow", "ncol", "xll", "yll", "pixelsize", "row.id", "product.code", "MODIS.acq.date",
                          "where", "MODIS.proc.date", 1:(ncol(data.file) - NUM_METADATA_COLS))
    
    # Create directory for this data file if SubDir = TRUE
    sub.dir <- substr(file.list[i], 1, regexpr(".asc$", file.list[i])-1)
    if(SubDir & !file.exists(file.path(Dir, DirName, sub.dir))) dir.create(path = file.path(Dir, DirName, sub.dir))
    
    for(n in 1:nrow(data.file)){
      
      data.band <- substr(data.file$row.id[n],
                          gregexpr(".", data.file$row.id[n], fixed = TRUE)[[1]][5] + 1, 
                          nchar(data.file$row.id[n]))
      data.date <- data.file$MODIS.acq.date[n]
      
      path <- ifelse(SubDir,
                     file.path(Dir, DirName, sub.dir,
                               paste0("GRID_", sub.dir, "_", data.band, "_", data.date, ".asc")),
                     file.path(Dir, DirName,
                               paste0("GRID_", sub.dir, "_", data.band, "_", data.date, ".asc"))
                     )
      
      write(c(sprintf("ncols\t\t %i", data.file$ncol[n]),
              sprintf("nrows\t\t %i", data.file$nrow[n]),
              sprintf("xllcorner\t %.2f", data.file$xll[n]),
              sprintf("yllcorner\t %.2f", data.file$yll[n]),
              sprintf("cellsize\t %s", as.character(data.file$pixelsize[n])),
              sprintf("NODATA_value\t %s", as.character(NoDataFill))
              ),
            file = path
            )
      
      grid.data <- matrix(data.file[n,(NUM_METADATA_COLS+1):ncol(data.file)],
                          nrow = data.file$nrow[n], ncol = data.file$ncol[n], byrow = TRUE)
      write.table(grid.data, file = path, append = TRUE, col.names = FALSE, row.names = FALSE)
    }
  }
}