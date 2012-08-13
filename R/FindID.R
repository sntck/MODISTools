FindID <-
function(x,dat)
{
  match.set<- dat[,match(names(x), names(dat))]
  row.matches<- apply(match.set,1,match,x)
  return(unique(dat$ID[which(!is.na(apply(row.matches,2,sum)))]))
}

