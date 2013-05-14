FindID <-
function(ID, Data)
{
  match.set <- Data[ ,match(names(ID), names(Data))]
  row.matches <- apply(match.set, 1, match, ID)
  return(unique(Data$ID[which(!is.na(apply(row.matches, 2, sum)))]))
}

