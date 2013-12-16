FindID <-
function(ID, Data)
{
  if(!is.object(ID) | !is.object(Data)) stop("ID and Data inputs must both be objects currently in your R workspace.")
  
  if(!all(names(ID) %in% names(Data))) stop("ID is not a subset of Data. All names of ID must match with rows in Data.")
  
  match.set <- Data[ ,match(names(ID), names(Data))]
  row.matches <- apply(match.set, 1, match, ID)
  
  if(length(which(!is.na(apply(row.matches, 2, sum)))) == 0,
     return("No matches found."),
     return(which(!is.na(apply(row.matches, 2, sum))))) 
}