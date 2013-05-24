FindID <-
function(ID, Data)
{
  if(!is.object(ID) | !is.object(Data)){
    stop("ID and Data inputs must both be objects currently in your R workspace.")
  }
  if(!any(names(ID) == names(Data))){
    stop("ID is not a subset of Data. Some names must match for matching rows to be found in separate objects.")
  }
  match.set <- Data[ ,match(names(ID), names(Data))]
  row.matches <- apply(match.set, 1, match, ID)
  return(unique(Data$ID[which(!is.na(apply(row.matches, 2, sum)))]))
}

