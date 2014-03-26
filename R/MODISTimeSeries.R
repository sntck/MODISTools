MODISTimeSeries <-
function(Dir, Band)
{
  if(!file.exists(Dir)) stop("Character string input for Dir argument does not resemble an existing file path.")
  
  file.set <- list.files(path = Dir, pattern = ".asc")
  
  file.ids <- sapply(file.set, function(x) 
                    any(grepl(pattern = Band, x = read.csv(file = x, header = FALSE, as.is = TRUE)[ ,1]))
              )
  file.set <- file.set[file.ids]
  
  if(length(file.set) < 1) stop("No downloaded files found in the requested directory for the requested data band.")
  
  data.collector <- vector(mode = "list", length = length(file.set))
  ts.row.names <- vector(mode = "list", length = length(file.set))
  ts.col.names <- vector(mode = "list", length = length(file.set))
  nrow.recorder <- ncol.recorder <- numeric(length = length(file.set))
  
  for(i in 1:length(file.set)){
    data.file <- read.csv(file.set[i], header = FALSE, as.is = TRUE)
    data.file <- data.file[grepl(pattern = Band, x = data.file[ ,1]), ]
    
    data.collector[[i]] <- data.file[ ,6:ncol(data.file)]
    
    nrow.recorder[i] <- nrow(as.matrix(data.file[ ,6:ncol(data.file)]))
    ncol.recorder[i] <- ncol(as.matrix(data.file[ ,6:ncol(data.file)]))
    
    ts.col.names[[i]] <- paste(unique(data.file[ ,4]), "_pixel", 1:ncol.recorder[i], sep = "")
    ts.row.names[[i]] <- data.file[ ,3]
  }
  
  res <- matrix(nrow = max(nrow.recorder), ncol = sum(ncol.recorder))
  rownames(res) <- ts.row.names[[which(nrow.recorder == max(nrow.recorder))[1]]]
  colnames(res) <- unlist(ts.col.names)
  
  for(j in 1:length(data.collector)){
    res[1:nrow.recorder[j],(sum(1, ncol.recorder[1:j]) - ncol.recorder[j]):sum(ncol.recorder[1:j])] <- 
      as.matrix(data.collector[[j]])
  }
  
  return(res)
}