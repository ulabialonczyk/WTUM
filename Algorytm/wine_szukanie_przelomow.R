GetData <- function(df){
  result <- sapply(2:length(df),function(i){
    x <- 100*(df[i] - df[i-1])/df[i-1]
  })
  result <- t(as.data.frame(result))
}

source('/Users/urszulabialonczyk/Documents/WTUM/structbreak.R')

#--------------------------------------- wczytanie danych ----------------------------------------------------#

wine_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/Wine_TRAIN.txt')

wine_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/Wine_TEST.txt')


#--------------------------------------- ustalenie parametrów ---------------------------------------------#

popSize <- 250

toNextGen <- 10
maxOrder <-4
crossProb <- 0.5
noChangeProb <- 0.99
noBPProb <- 0.007 # noChangeProb + noBPProb mniejsze od 1
maxIter <- 50#10000
noChangeStop <- 40#100

seriesWine <- wine_train[, 2:length(wine_train)]

listaSeriesWine <- list()

for(i in 1:nrow(seriesWine)){
  listaSeriesWine[[i]] = seriesWine[i, ]
}


lista_PunktowPrzelomu <- lapply(listaSeriesWine, function(x){
  
  seriesWine1 = GetData(x)
  
  genomeSizeWine <- nrow(seriesWine1)
  
  xSeriesWine <- RunGA(seriesWine1,popSize, genomeSizeWine,toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)
  
  przelomy = which(xSeriesWine[[1]] != -1)
  
  return(przelomy)
})

lista_PunktowPrzelomu

max(unlist(lapply(lista_PunktowPrzelomu, length))) # maksymalna liczba punktow przelomu:21

macierz_przelomow = matrix(NA, nrow = 57, ncol = 21) #nrow=nrow(data), ncol=max(liczba punktow przelomu)

for(i in 1:length(lista_PunktowPrzelomu)){
  macierz_przelomow[i, ] = c(lista_PunktowPrzelomu[[i]], rep(NA, 21-length(lista_PunktowPrzelomu[[i]])))
}

write.csv(macierz_przelomow, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/macierz_przelomow_wine.csv', row.names = FALSE)
