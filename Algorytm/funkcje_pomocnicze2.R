DrawSeries2 <- function(series, breaks){
  
  plot(series[1, ], type='l', xlab = 'Czas', ylab = 'Wartość', main = 'Szereg i znalezione przełomy')
  
  for(i in 2:nrow(series)){
    lines(series[i, ])
  }
  
  for(i in 2:length(breaks)){
    abline(v=breaks[i], col='red')
  }
  
}

GetData <- function(df){
  df <- df[-1]
  result <- sapply(2:length(df),function(i){
    x <- 100*(df[i] - df[i-1])/df[i-1]
  })
  result <- t(as.data.frame(result))
}

PrintParameters <- function(series, genome){
  genome <- unlist(genome)
  bp <- which(genome!=-1)
  print(length(bp))
  if(length(bp) != 1){
    for(i in 2:(length(bp))){
      vec <- series[bp[i-1]:(bp[i]-1)]
      order = as.integer(length(vec)-1)
      model <- (ar.yw(vec, order.max = order, aic = FALSE))
      print(paste('Period ',(i-1)))
      print(paste('Variance: ',model[['var.pred']]))
      print(paste('Params: ',model[['ar']]))
    }
    vec <- series[bp[length(bp)-1]:length(series)]
    order = as.integer(length(vec)-1)
    model <- (ar.yw(vec, order.max = order, aic = FALSE))
    print(paste('Period ',(length(bp))))
    print(paste('Variance: ',model[['var.pred']]))
    print(paste('Params: ',model[['ar']]))
  }
  else{
    model <- (ar.yw(series, order.max = genome[1], aic = FALSE))
    print(paste('Period ',(length(bp))))
    print(paste('Variance: ',model[['var.pred']]))
    print(paste('Params: ',model[['ar']]))
  }
}


SaveParameters <- function(series, genome){
  genome <- unlist(genome)
  bp <- which(genome!=-1)
  print(length(bp))
  lista_dopasowan = list()
  if(length(bp) != 1){
    for(i in 2:(length(bp))){
      vec <- series[bp[i-1]:(bp[i]-1)]
      order = as.integer(length(vec)-1)
      model <- (ar.yw(vec, order.max = order, aic = FALSE))
      # zawsze pierwszy element to ktory segment, druga czesc to wariancja, reszta to parametry
      lista_dopasowan[[i-1]] = c(i-1, model[['var.pred']], model[['ar']])
    }
    vec <- series[bp[length(bp)-1]:length(series)]
    order = as.integer(length(vec)-1)
    model <- (ar.yw(vec, order.max = order, aic = FALSE))
    lista_dopasowan[[length(bp)]] = c(length(bp), model[['var.pred']], model[['ar']])
    
  }
  else{
    model <- (ar.yw(series, order.max = genome[1], aic = FALSE))
    lista_dopasowan[[1]] = c(period = length(bp), variance = model[['var.pred']], params = model[['ar']])
  }
  return(lista_dopasowan)
}

# moda gdy jest nieparzyscie segmentow

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# moda gdy jest parzyscie segmentow
getMode2 <- function(x){
  df = as.data.frame(table(x))
  
  if(nrow(df)==2 & df[2,1]==df[2,2]){
    label = sample(c(df[1,1], df[1,2]), 1) # jesli jest po rowno, to losujemy label
  }else{
    label = getmode(x)
  }
  
  return(label)
}

# finalne glosowanie
getLabels <- function(df){
  
  if(ncol(df) %% 2 == 0){
    
    moda = apply(df, 1, getMode2)
    
  } else{
    moda=apply(df, 1, getmode)
  }
  return(moda)
  
}

