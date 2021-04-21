GenomeToParams <- function(genome){
  tau <- which(genome != -1)
  params <- vector()
  params <- c(params,length(tau))
  params <- c(params, sapply(tau, function(x){c(x,genome[x])}))
  params <- c(params,length(genome)+1)
}

Log2Plus <- function(x){
  y <- max(0,log(x,2))
}

GenerateRandomGenome <- function(n, maxOrder){
  head <- sample.int(maxOrder,1,replace = TRUE)
  
  tail <- sample.int(5,n-1,replace = TRUE)
  tail <- sapply(tail,function(t){
    x <- 0
    if(t == 1){
      x <- sample.int(maxOrder,1, replace = TRUE)
    }else{
      x <- -1
    }
    x  
  })
  tail[length(tail)] = -1
  genome <- c(head,tail)
}


GeneratePopulation <- function(popSize, genomeSize, maxOrder){
  population <- vector(mode = "list", length = popSize)
  for(i in 1:popSize){
    population[i] <- list(GenerateRandomGenome(genomeSize,maxOrder))
  }
  
  population <- sapply(population,function(p){
    FixGenome(p)
  })
  
  population
}

FixGenome <- function(genome){
  newGenome <- unlist(genome, use.names=FALSE)
  params <- GenomeToParams(genome)
  idxvect <- c(seq(4,length(params),2))
  w <- sapply(idxvect,function(ind){
    subseq <- params[ind] - params[ind-2]
    order <- params[ind-1]
    
    if(subseq <= order){
      order <- subseq - 1
    }
    
    order
  })
  for(i in 1:length(w)){
    newGenome[params[i*2]] <- w[i]
  }
  for(i in 2:length(newGenome)){
    if(newGenome[i-1] == 0 & newGenome[i] != -1){
      newGenome[i] = -1
      newGenome[i-1] = 1
    }
  }
  
  newGenome
}

GetRandomRank <- function(size){ # losuje indeks rozmnazanego genomu
  vect <- cumsum(rev(1:size))
  r <- sample.int(vect[size],1,replace = TRUE)
  
  rank <- which(vect >= r)
  rank <- as.integer(rank[1])
}


MDL <- function(params, entry){
  n <- length(entry)
  m <- params[1]
  res <- log(m,2) + m * log(n,2)
  p <- params[seq(3,length(params),2)]
  idxvect <- c(seq(4,length(params),2))
  res <- res + sum(sapply(p,function(ind){
    res <- Log2Plus(ind)
  }))
  #print(res)
  res <- res + sum((p+2)/2 * sapply(idxvect, function(ind){
    result <- Log2Plus(params[ind] - params[ind-2])
  }))
  #print(res)
  sigmas <- sapply(idxvect, function(ind){
    vec <- entry[params[ind-2]:(params[ind]-1)]
    #print(vec)
    sigma <- ar.yw(vec, order.max = params[ind-1], aic = FALSE)
    #print('sigma')
    sigma <- sigma[['var.pred']]
    #print(sigma)
  })
  nj <- sapply(idxvect, function(ind){
    result <- params[ind] - params[ind-2]
  })
  log <- sapply(sigmas, function(s){
    result <- Log2Plus(2*pi*s)
  })
  res <- res + sum(nj*log/2)
  #print(res)
  res <- res + n/2
  #print(res)
}



RunGA <- function(series,popSize, genomeSize, toNextGen,maxOrder, crossProb, 
                  noChangeProb, noBPProb, maxIter, noChangeStop){
  noImpr <- 0
  check <- 0
  population <- GeneratePopulation(popSize,genomeSize,maxOrder)
  scores <- sapply(1:popSize,function(i){
    g <- unlist(population[,i], use.names=FALSE)
    params <- GenomeToParams(g)
    #print(params)
    print('obliczam MDL')
    scores <- MDL2(params,series)
    print(scores)
  })
  population <- t(population)
  population <- cbind(population, scores)
  df <- data.frame(population)
  bestGenome <- c()
  bestScore <- Inf
  bestVector <- c()
  
  for(iter in 1:maxIter){
    if(noImpr == noChangeStop){ # koniec jak nie ma zmian
      break
    }
    df <- df[order(df$scores),]
    if((df$scores)[1] < bestScore){
      bestGenome <- df[1,]
      bestScore <- (df$scores)[1]
      noImpr <- 0
    }
    bestVector <- c(bestVector,bestScore)
    print(df)
    for(i in 1:(popSize - toNextGen)){ # popsize - toNextGen nowych dzieci
      cross <- runif(1,0,1)
      if(cross < crossProb){ #crossover
        ind1 <- GetRandomRank(genomeSize)
        ind2 <- GetRandomRank(genomeSize)
        c1 <- df[ind1,]
        c2 <- df[ind2,]
        child <- rep(0,length(c1))
        #print(child)
        
        #here we choose from which parent we get value
        for(i in 1:(length(c1)-1)){
          first <- sample.int(2,1,replace=TRUE)
          if(first == 1){
            child[i] = c1[i]
          }else{
            child[i] = c2[i]
          }
        }
        gen <- unlist(child[1:(length(child)-1)])
        child[1:(length(child)-1)] <- FixGenome(gen)
        #print('cross')
        #print(ind1)
        #print(ind2)
        #print(c1)
        #print(c2)
        df <- rbind(df,child)
        
      }else{ #mutation
        ind1 <- GetRandomRank(genomeSize)
        c1 <- df[ind1,]
        change <- runif(1,0,1)
        child <- rep(0,length(c1))
        for(i in 2:(length(c1)-2)){ 
          noChange <- runif(1,0,1)
          if(noChange < noChangeProb){ #bez zmian
            child[i] <- c1[i] #bez zmian
          }else if(noChange >= noChangeProb & noChange < noChangeProb + noBPProb){
            child[i] = -1 # zmiana na bez BP
          }else{ # zmiana na BP o losowym rzedzie
            order <- sample.int(maxOrder,1,replace = TRUE)
            child[i] <- order  
          }
        }
        order <- sample.int(maxOrder,1,replace = TRUE)
        child[1] <- order
        child[length(c1)-1] = -1
        
        #genom musi byc poprawny
        gen <- unlist(child[1:(length(child)-1)])
        child[1:(length(child)-1)] <- FixGenome(gen)
        df <- rbind(df,child)
        #print('mut')
      }
    }
    noImpr <- noImpr + 1
    scores <- sapply((popSize + 1):(2*popSize - toNextGen), function(i){
      g <- unlist(df[i,1:genomeSize], use.names=FALSE)
      #print(g)
      params <- GenomeToParams(g)
      scores <- MDL2(params,series)
    })
    df[(popSize + 1):(2*popSize - toNextGen),genomeSize +1] = scores
    df <- df[-((toNextGen+1):popSize),]
    
    #print(df)
    check <- check + 1
    print(check)
  }
  #koncowka
  df <- df[order(df$scores),]
  if((df$scores)[1] < bestScore){
    bestGenome <- df[1,]
    bestScore <- (df$scores)[1]
  }
  #print(df)
  #print(bestGenome)
  bestScore
  retList <- list(bestGenome[1:(length(bestGenome)-1)],bestVector)
}
