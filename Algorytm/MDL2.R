###----------------------- funkcje pomocnicze ------------------- #

Log2Plus <- function(x){
  y <- max(0,log(x,2))
  return(y)
}

LogPlus <- function(x){
  y <- max(0,log(x))
  return(y)
}

LogMinus <- function(x){

  if(is.nan(x)==TRUE){
    v=0
  }else if(x < 1){
    v = 0
  }else{
    v = log(x)
  }
  return(v)
}

findSigma <- function(ind){
  
  vec <- entry[params[ind-2]:(params[ind]-1)]
  
  sigma <- ar.yw(vec, order.max = params[ind-1], aic = FALSE)
  
  sigma <- sigma[['var.pred']]
  
  return(sigma)
}

findSigma <- function(ind){
  
  vec <- entry[params[ind-2]:(params[ind]-1)]
  
  sigma <- ar.yw(vec, order.max = params[ind-1], aic = FALSE)
  
  sigma <- sigma[['var.pred']]
  
  return(sigma)
}

calculateValue <- function(resid, varcovar){
  single = LogMinus(det(varcovar)) + t(resid) %*% solve(varcovar, tol=NULL) %*% resid # wartosc 5 czlonu dla pojedynczego t
  #print('poszczegolne czlony calculateValue')
  #print(paste0('1 ', LogMinus(det(varcovar))))
  #print(paste0('2 ', t(resid) %*% solve(varcovar, tol=NULL) %*% resid))
  return(single)
}

calculateValue2 <- function(resid, varcovar){
  single = LogMinus(det(varcovar)) + t(resid) %*% MASS::ginv(varcovar) %*% resid # wartosc 5 czlonu dla pojedynczego t
  #print('poszczegolne czlony calculateValue')
  #print(paste0('1 ', LogMinus(det(varcovar))))
  #print(paste0('2 ', t(resid) %*% solve(varcovar, tol=NULL) %*% resid))
  return(single)
}

prepareData <- function(df){
  result <- sapply(2:length(df),function(i){
    x <- 100*(df[i] - df[i-1])/df[i-1]
  })
}



# dopasowywanie modelu wielu zmiennych

fitModel <- function(szereg, maxorder){
  
  model=ar.yw(szereg, order.max=maxorder)
  
  return(model)
  
}

fitModel2 <- function(szereg, maxorder){
  out <- tryCatch(fitModel(szereg, maxorder), error = function(e) TRUE)
  return(out)
}

# sprawdzanie czy da sie odwrocic macierz wariancji - kowariancji
inverseMatrix <- function(model){
  inverse = solve(model$var.pred, tol=NULL)
  return(inverse)
}
inverseMatrix2 <- function(model){
  out <- tryCatch(inverseMatrix(model), error = function(e) TRUE)
  return(out)
}

pseudoinverseMatrix <- function(model){
  inverse = MASS::ginv(model$var.pred)
  return(inverse)
}
pseudoinverseMatrix2 <- function(model){
  out <- tryCatch(pseudoinverseMatrix(model), error = function(e) TRUE)
  return(out)
}

#------------------------------- MDL ------------------------- #

MDL2 <- function(params, multi_series){
  
  
  
  n <- ncol(multi_series) #dlugosc szeregu
  d <- nrow(multi_series)
  m <- params[1] # liczba segmentow
  
  p <- params[seq(3,length(params),2)] # rzędy procesów AR w poszczególnych segmentach
  idxvect <- c(seq(4,length(params),2)) #indeksy gdzie w wekt. params są nasze punkty przełomów
  pozycje_przelomow <- params[idxvect] # punkty przelomow
  
  nj <- diff(c(1,pozycje_przelomow))
  
  pierwszy_czlon <- log(m)
  
  #print(paste0('pierwszy ', pierwszy_czlon))
  
  drugi_czlon <- m*log(n)
  #print(paste0('drugi ', drugi_czlon))
  
  trzeci_czlon <- sum(sapply(p, LogPlus))
  #print(paste0('trzeci ', trzeci_czlon))
  
  czwarty_czlon <- sum((p*d^2 + d + d*(d+1)/2)*log(nj)/2)
  #print(paste0('czwarty ', czwarty_czlon))
  
  # do piatego czlonu potrzebny jest model
  modelowanie <- lapply(idxvect, function(ind){ #wyliczamy estymatory
    
    vec <- multi_series[, c(params[ind-2]:(params[ind]-1))]
    
    maxorder=ncol(vec)-1
    
    #print('vec')
    #print(vec)
    #print(paste0('maxorder domyślny:', maxorder))
    
    # szukamy maxorder z ktorym da sie dopasowac model
    while(is.logical(fitModel2(vec, maxorder))==TRUE & maxorder > 1){
      maxorder = maxorder - 1
    }
    
    #print(paste0('maxorder znaleziony:', maxorder))
    
    model <- fitModel2(vec, maxorder)
    
    # sprawdzamy czy udalo sie dopasowac model
    if(is.logical(model)==TRUE){
      inverse = TRUE
    }else{
      inverse = inverseMatrix2(model)
    }
    
    # sprawdzamy czy macierz var-covar jest odwracalna, jesli tak to ja odwracamy
    
    return(inverse) })
  
  #print(paste0('modelowanie ', modelowanie))
  
  if(sum(unlist(lapply(modelowanie,is.logical))) > 0){
    
    est <- NA
    #print('est is na')
  } else{
    
    
    est <- sapply(idxvect, function(ind){ #wyliczamy estymatory
      
      vec <- multi_series[, c(params[ind-2]:(params[ind]-1))]
      
      maxorder=ncol(vec)-1
      
      #print('vec')
      #print(vec)
      #print(paste0('maxorder domyślny:', maxorder))
      
      # szukamy maxorder z ktorym da sie dopasowac model
      while(is.logical(fitModel2(vec, maxorder))==TRUE){
        maxorder = maxorder - 1
      }
      
      #print(paste0('maxorder znaleziony:', maxorder))
      
      model <- fitModel2(vec, maxorder)
      
      
      # tu juz wiemy, ze wszystkie macierze varcovar sa odwracalne, wiec nie trzeba tego sprawdzac
      
      varcovar1 = model$var.pred
      #print(paste0('var: ', varcovar1))
      resid1 = model$resid[complete.cases(model$resid), ]
      #print(paste0('res ', resid1))
      value = sum(apply(resid1, 1, calculateValue, varcovar1))
      print(paste0('value: ', value))
      
      return(value) })
    #print('est is not na')
    #print(paste0('est',est))
    
  }
  
  piaty_czlon = 0.5*sum(est)
  
  if(is.na(piaty_czlon)==TRUE){
    piaty_czlon = Inf
  }
  
  #print(paste0('piaty ', piaty_czlon))
  
  mdl = pierwszy_czlon + drugi_czlon + trzeci_czlon + czwarty_czlon + piaty_czlon
  print(paste0('mdl: ', mdl))
  
  return(mdl)
  
  
  
}


pseudoMDL2 <- function(params, multi_series){
  
  
  
  n <- ncol(multi_series) #dlugosc szeregu
  d <- nrow(multi_series)
  m <- params[1] # liczba segmentow
  
  p <- params[seq(3,length(params),2)] # rzędy procesów AR w poszczególnych segmentach
  idxvect <- c(seq(4,length(params),2)) #indeksy gdzie w wekt. params są nasze punkty przełomów
  pozycje_przelomow <- params[idxvect] # punkty przelomow
  
  nj <- diff(c(1,pozycje_przelomow))
  
  pierwszy_czlon <- log(m)
  
  #print(paste0('pierwszy ', pierwszy_czlon))
  
  drugi_czlon <- m*log(n)
  #print(paste0('drugi ', drugi_czlon))
  
  trzeci_czlon <- sum(sapply(p, LogPlus))
  #print(paste0('trzeci ', trzeci_czlon))
  
  czwarty_czlon <- sum((p*d^2 + d + d*(d+1)/2)*log(nj)/2)
  #print(paste0('czwarty ', czwarty_czlon))
  
  # do piatego czlonu potrzebny jest model
  modelowanie <- lapply(idxvect, function(ind){ #wyliczamy estymatory
    
    vec <- multi_series[, c(params[ind-2]:(params[ind]-1))]
    
    maxorder=ncol(vec)-1
    
    #print('vec')
    #print(vec)
    #print(paste0('maxorder domyślny:', maxorder))
    
    # szukamy maxorder z ktorym da sie dopasowac model
    while(is.logical(fitModel2(vec, maxorder))==TRUE & maxorder > 1){
      maxorder = maxorder - 1
    }
    
    #print(paste0('maxorder znaleziony:', maxorder))
    
    model <- fitModel2(vec, maxorder)
    
    # sprawdzamy czy udalo sie dopasowac model
    if(is.logical(model)==TRUE){
      inverse = TRUE
    }else{
      inverse = pseudoinverseMatrix2(model)
    }
    
    # sprawdzamy czy macierz var-covar jest odwracalna, jesli tak to ja odwracamy
    
    return(inverse) })
  
  #print(paste0('modelowanie ', modelowanie))
  
  if(sum(unlist(lapply(modelowanie,is.logical))) > 0){
    
    est <- NA
    #print('est is na')
  } else{
    
    
    est <- sapply(idxvect, function(ind){ #wyliczamy estymatory
      
      vec <- multi_series[, c(params[ind-2]:(params[ind]-1))]
      
      maxorder=ncol(vec)-1
      
      #print('vec')
      #print(vec)
      #print(paste0('maxorder domyślny:', maxorder))
      
      # szukamy maxorder z ktorym da sie dopasowac model
      while(is.logical(fitModel2(vec, maxorder))==TRUE){
        maxorder = maxorder - 1
      }
      
      #print(paste0('maxorder znaleziony:', maxorder))
      
      model <- fitModel2(vec, maxorder)
      
      
      # tu juz wiemy, ze wszystkie macierze varcovar sa odwracalne, wiec nie trzeba tego sprawdzac
      
      varcovar1 = model$var.pred
      #print(paste0('var: ', varcovar1))
      resid1 = model$resid[complete.cases(model$resid), ]
      #print(paste0('res ', resid1))
      value = sum(apply(resid1, 1, calculateValue2, varcovar1))
      print(paste0('value: ', value))
      
      return(value) })
    #print('est is not na')
    #print(paste0('est',est))
    
  }
  
  piaty_czlon = 0.5*sum(est)
  
  if(is.na(piaty_czlon)==TRUE){
    piaty_czlon = Inf
  }
  
  #print(paste0('piaty ', piaty_czlon))
  
  mdl = pierwszy_czlon + drugi_czlon + trzeci_czlon + czwarty_czlon + piaty_czlon
  print(paste0('mdl: ', mdl))
  
  return(mdl)
  
  
  
}

