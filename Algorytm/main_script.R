source('/Users/urszulabialonczyk/Documents/WTUM/MDL2.R')
source('/Users/urszulabialonczyk/Documents/WTUM/funkcje_pomocnicze2.R')
source('/Users/urszulabialonczyk/Documents/WTUM/genetic_algorithm.R')


#----------------------------------- wczytanie danych -----------------------------------------------#
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Coffee/Coffee_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Coffee/Coffee_TEST.txt')

#-----------------------------------parametry---------------------------------------------------#
#meat
popSize <- 50

toNextGen <- 10
maxOrder <- 4
crossProb <- 0.5
noChangeProb <- 0.99
noBPProb <- 0.007 # noChangeProb + noBPProb mniejsze od 1
maxIter <- 20#10000
noChangeStop <- 20#100

# ----------------------- przygotowanie danych ------------------------- #

dane=t(apply(coffee_train, 1, prepareData))
dane = dane[, 2:ncol(dane)]

# ------------------------ wykonanie algorytmu ------------------------ #
set.seed(42)
result = RunGA(dane,popSize,ncol(dane),toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)

przelomy = which(result[[1]] != -1)
write.csv(przelomy, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/Coffee/przelomy.csv', row.names = FALSE)


#----------------------------------- kolejne dane -------------------------------#

meat_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Meat/Meat_TRAIN.txt')

meat_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Meat/Meat_TEST.txt')

popSize <- 450

dane2=t(apply(meat_train, 1, prepareData))
dane2 = dane2[, 2:ncol(dane2)]


set.seed(42)
result2 = RunGA(dane2,popSize,ncol(dane2),toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)

przelomy2 = which(result2[[1]] != -1)
write.csv(przelomy2, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/Meat/przelomy.csv', row.names = FALSE)

#----------------------------------- kolejne dane -------------------------------#

gun_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/GunPoint/GunPoint_TRAIN.txt')

gun_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/GunPoint/GunPoint_TEST.txt')

popSize <- 200

dane3=t(apply(gun_train, 1, prepareData))
dane3 = dane3[, 2:ncol(dane3)]


set.seed(42)
result3 = RunGA(dane3,popSize,ncol(dane3),toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)

# tu wszedzie wychodzi inf, nawet z 

przelomy3 = which(result3[[1]] != -1)
write.csv(przelomy2, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/GunPoint/przelomy.csv', row.names = FALSE)

#----------------------------------- kolejne dane -------------------------------#

diatom_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/DiatomSizeReduction/DiatomSizeReduction_TRAIN.txt')

diatom_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/DiatomSizeReduction/DiatomSizeReduction_TEST.txt')

popSize <- 400

dane4=t(apply(diatom_train, 1, prepareData))
dane4 = dane4[, 2:ncol(dane4)]



set.seed(42)
result4 = RunGA(dane4,popSize,ncol(dane4),toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)

# tez wychodza same nieodwracalne - problemem jest macierz var-covar, probuje pseudoinverse
#source('/Users/urszulabialonczyk/Documents/WTUM/genetic_algorithm2.R')


set.seed(40)
result4 = RunGA(dane4,popSize,ncol(dane4),toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)


przelomy4 = which(result4[[1]] != -1)
write.csv(przelomy4, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/DiatomSizeReduction/przelomy.csv', row.names = FALSE)

#----------------------------------- kolejne dane -------------------------------#

herring_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Herring/Herring_TRAIN.txt')

herring_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Herring/Herring_TEST.txt')

popSize <- 550

dane5=t(apply(herring_train, 1, prepareData))
dane5 = dane5[, 2:ncol(dane5)]

set.seed(42)
result5 = RunGA(dane5,popSize,ncol(dane5),toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)

# tu wszedzie wychodzi inf, nawet z 

przelomy3 = which(result3[[1]] != -1)
write.csv(przelomy2, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/Herring/przelomy.csv', row.names = FALSE)

#-------

#----------------------------------- kolejne dane -------------------------------#

wine_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/Wine_TRAIN.txt')

wine_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/Wine_TEST.txt')

popSize <- 250

dane6=t(apply(wine_train, 1, prepareData))
dane6 = dane6[, 2:ncol(dane6)]

set.seed(42)
result6 = RunGA(dane6,popSize,ncol(dane6),toNextGen,maxOrder,crossProb,noChangeProb,noBPProb,maxIter,noChangeStop)

przelomy6 = which(result6[[1]] != -1)
write.csv(przelomy6, '/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/przelomy.csv', row.names = FALSE)

#-------
