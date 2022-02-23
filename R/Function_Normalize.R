normalize <- function(x) {
  return (0.95*(x - min(x)) / (max(x) - min(x)))+0.01
}

normalize_predict<- function(x,min_x,max_x) {
  return (0.95*(x - min_x) / (max_x - min_x))+0.01
}

re_normalize_predict<-function(x, min_x,max_x){
  y=(x-0.01)*(max_x-min_x)/0.95+min_x
  y=round(y)
  return(y)
}

normalize_train <- function(x) {
  return (0.95*(x - min(x)) / (max(x) - min(x)))+0.01
}
