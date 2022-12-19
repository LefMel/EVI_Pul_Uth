## function for runif
miss_uni = function(x,a,b){
  if(is.na(b))b=a
  
  if (a > b){
    y = floor((runif(x,b,a)))
  }
  else{
    y = floor(runif(x,a,b))
  }
  
  return (y)
}


