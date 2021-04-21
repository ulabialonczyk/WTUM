# moze zrobic komitet klasyfikatorow: 
# jeden to bedzie taki, ktory osobno rozpatruje kazda zmienna
# drugi sprobuje zrobic multivariate
library(ggplot2)

source('/Users/urszulabialonczyk/Documents/WTUM/genetic_algorithm.R')
source('/Users/urszulabialonczyk/Documents/WTUM/funkcje_pomocnicze2.R')

#--------------------------------------- wczytanie danych ----------------------------------------------------#

coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Coffee/Coffee_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Coffee/Coffee_TEST.txt')


#--------------------------------------- ustalenie parametrów ---------------------------------------------#

popSize <- 300

toNextGen <- 10
maxOrder <-4
crossProb <- 0.5
noChangeProb <- 0.99
noBPProb <- 0.007 # noChangeProb + noBPProb mniejsze od 1
maxIter <- 100#10000
noChangeStop <- 50#100



#------------------------------------ szukanie przełomów ------------------------------------#

seriesCoffee <- coffee_train[, 2:length(coffee_train)]

lista_SaveParameters <- list()

lista_PunktowPrzelomu <- list()

for(i in 1:nrow(coffee_train)){
  
  num = i
  print(num)
  
  seriesCoffee1 = GetData(seriesCoffee[i, ])
  
  genomeSizeCoffee <- length(seriesCoffee1)
  
  xSeriesCoffee <- RunGA(seriesCoffee1,popSize, genomeSizeCoffee,toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)
  
  PrintParameters(seriesCoffee1,xSeriesCoffee[1])
  
  lista_SaveParameters[[i]] <- SaveParameters(seriesCoffee1,xSeriesCoffee[1])
  
  lista_PunktowPrzelomu[[i]] <- which(xSeriesCoffee[[1]] != -1)
  
  DrawSeries(seriesCoffee1,xSeriesCoffee[1], num)
  
 
}

lista_PunktowPrzelomu

max(unlist(lapply(lista_PunktowPrzelomu, length))) # maksymalna liczba punktow przelomu:15

macierz_przelomow = matrix(NA, nrow = 28, ncol = 15) #nrow=nrow(data), ncol=max(liczba punktow przelomu)

for(i in 1:length(lista_PunktowPrzelomu)){
  macierz_przelomow[i, ] = c(lista_PunktowPrzelomu[[i]], rep(NA, 15-length(lista_PunktowPrzelomu[[i]])))
}

write.csv(macierz_przelomow, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/Coffee/macierz_przelomow.csv', row.names = FALSE)

