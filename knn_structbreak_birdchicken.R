source('/Users/Ula/Desktop/wtum/funkcje_pomocnicze2.R')

#----------------------------------- wczytanie danych: gunpoint -----------------------------------------------#
coffee_train <- read.table('/Users/Ula/Desktop/wtum/BirdChicken_TRAIN.txt')

coffee_test <- read.table('/Users/Ula/Desktop/wtum/BirdChicken_TEST.txt')

macierz_przelomow_coffee <- read.csv('/Users/Ula/Desktop/wtum/macierz_przelomow_bird.csv')

scalona_macierz_przelomow = apply(macierz_przelomow_coffee[, -1], 1, function(x){
  
  ncol = ncol(macierz_przelomow_coffee)
  
  diff= diff(x)
  
  ids = which(diff < 8)
  
  scalone_przelomy = x[-(ids + 1)]
  
  scalone_przelomy = c(scalone_przelomy, rep(NA, ncol-length(scalone_przelomy)))
  
  return(scalone_przelomy)
})

scalona_macierz_przelomow = t(scalona_macierz_przelomow)

rows=apply(scalona_macierz_przelomow, 1, function(x){
  print(all(is.na(x)))
})
idx = which(rows==TRUE) # wiersze macierzy z samymi NA

scalona_macierz_przelomow= scalona_macierz_przelomow[-idx, ]


#----------------------------------- rysowanie szeregu i prze³omów -----------------------------------#

coffee_train_val = GetData(coffee_train)
coffee_train_labels = as.factor(as.character(coffee_train[, 1]))


coffee_test_val = GetData(coffee_test)
coffee_test_labels = coffee_test[, 1]
coffe_labels_factor = as.factor(as.character(coffee_test[, 1]))




# --------------------------------- szukanie etykiet ---------------------------#

#------------------------------- ZWYKLE KNN ------------------------------------- #
knn_zwykle_coffee1 = knn(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1)

acc1z=sum(knn_zwykle_coffee1==coffe_labels_factor)/length(coffe_labels_factor)

# k=3
knn_zwykle_coffee3= knn(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3)

acc3z=sum(knn_zwykle_coffee3==coffe_labels_factor)/length(coffe_labels_factor)

# k=5
knn_zwykle_coffee5= knn(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5)

acc5z=sum(knn_zwykle_coffee5==coffe_labels_factor)/length(coffe_labels_factor)

# k=7
knn_zwykle_coffee7= knn(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7)

acc7z=sum(knn_zwykle_coffee7==coffe_labels_factor)/length(coffe_labels_factor)

# k=9
knn_zwykle_coffee9= knn(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9)

acc9z=sum(knn_zwykle_coffee9==coffe_labels_factor)/length(coffe_labels_factor)

# komitet 3 najlepsze

komitetz = cbind(knn_zwykle_coffee3, knn_zwykle_coffee5, knn_zwykle_coffee7, knn_zwykle_coffee9)

predykcje_komitet_z = getLabels(komitetz)

acckomz = sum(predykcje_komitet_z==coffe_labels_factor)/length(coffe_labels_factor)


# ----------------------------- METODA 2 ---------------------------------- #
macierz_przelomow_coffee

# dla kazdego wiersza powyzszej macierzy chcemy: 
# zrobic przelomy = c(wiersz, nrow(coffee_train_val))
# puscic knnStructbreak(train, test, label, k, przelomy)
# puscic getlabel dla powyzszego
# takie get label mozna zapisac w macierzy, potem znowu sie pusci getLabel dla calej macierzy

# w tym zbiorze danych psuje coœ przy zmiennej V78, knn zwraca NaN. Zarowno w treningowym i testowym
# ta zmienna to same 0, wiec ja wyrzucam, nic nie tracimy.

# dla k=1


macierz_klasyfikatorów1 = apply(macierz_przelomow_coffee[ ,-1],1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

#---------------#


labels1=getLabels(macierz_klasyfikatorów1)

m2_acc_k1 = sum(labels1 == coffee_test_labels)/20

acc_poszczegolnych_k1=apply(macierz_klasyfikatorów1==coffee_test_labels, 2, sum)/20 # 0.9067 najlepiej

# k = 3

macierz_klasyfikatorów3 = apply(macierz_przelomow_coffee[, -1],1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels3=getLabels(macierz_klasyfikatorów3)

m2_acc_k3= sum(labels3 == coffee_test_labels)/20


acc_poszczegolnych_k3=apply(macierz_klasyfikatorów3==coffee_test_labels, 2, sum)/20 # najlepszy 0.88


#k = 5
macierz_klasyfikatorów5 = apply(macierz_przelomow_coffee[, -1],1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels5=getLabels(macierz_klasyfikatorów5)

m2_acc_k5 = sum(labels5 == coffee_test_labels)/20

acc_poszczegolnych_k5=apply(macierz_klasyfikatorów5==coffee_test_labels, 2, sum)/20


# k = 7
macierz_klasyfikatorów7 = apply(macierz_przelomow_coffee[, -1],1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels7=getLabels(macierz_klasyfikatorów7)

m2_acc_k7 = sum(labels7 == coffee_test_labels)/20

acc_poszczegolnych_k7=apply(macierz_klasyfikatorów7==coffee_test_labels, 2, sum)/20

# k = 9
macierz_klasyfikatorów9 = apply(macierz_przelomow_coffee[, -1],1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels9=getLabels(macierz_klasyfikatorów9)

m2_acc_k9 = sum(labels9 == coffee_test_labels)/20

acc_poszczegolnych_k9=apply(macierz_klasyfikatorów9==coffee_test_labels, 2, sum)/20

wyniki_poszczegolnych = rbind(acc_poszczegolnych_k1, acc_poszczegolnych_k3, acc_poszczegolnych_k5, acc_poszczegolnych_k7, acc_poszczegolnych_k9)

write.csv(wyniki_poszczegolnych, '/Users/Ula/Desktop/wtum/wyniki_poszczegolnych_coffee.csv')


# komitet poszczegolnych
acc_poszczegolnych_k1[c(6, 11)]
acc_poszczegolnych_k3[4]

#14, 32, 40
komitet_najlepszych = getLabels(cbind(macierz_klasyfikatorów1[, c(1, 2,5,6)]))
m2_acc_komitet_najlepszych = sum(komitet_najlepszych==coffee_test_labels)/20
table(komitet_najlepszych, coffee_test_labels)

table(macierz_klasyfikatorów1[, 11],coffee_test_labels) #0.8

# komitet z 1, 3, 5
etykiety_komitet = getLabels(cbind(labels1, labels7, labels5))
m2_acc_komitet = sum(etykiety_komitet==coffee_test_labels)/20

##------------------------------------- DLA SCALONEJ MACIERZY PRZELOMOW ---------------------- #

macierz_klasyfikatorów1s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels1s=getLabels(macierz_klasyfikatorów1s)

m2_acc_k1s = sum(labels1s == coffee_test_labels)/20

acc_poszczegolnych_k1s=apply(macierz_klasyfikatorów1s==coffee_test_labels, 2, sum)/20 # 0.96

# k = 3

macierz_klasyfikatorów3s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels3s=getLabels(macierz_klasyfikatorów3s)


m2_acc_k3s= sum(labels3s == coffee_test_labels)/20

acc_poszczegolnych_k3s=apply(macierz_klasyfikatorów3s ==coffee_test_labels, 2, sum)/20


#k = 5
macierz_klasyfikatorów5s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels5s=getLabels(macierz_klasyfikatorów5s)

m2_acc_k5s = sum(labels5s == coffee_test_labels)/20

acc_poszczegolnych_k5s=apply(macierz_klasyfikatorów5s==coffee_test_labels, 2, sum)/20


# k = 7
macierz_klasyfikatorów7s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels7s=getLabels(macierz_klasyfikatorów7s)

m2_acc_k7s = sum(labels7s == coffee_test_labels)/20

acc_poszczegolnych_k7s=apply(macierz_klasyfikatorów7s==coffee_test_labels, 2, sum)/20

# k = 9
macierz_klasyfikatorów9s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels9s=getLabels(macierz_klasyfikatorów9s)

m2_acc_k9s = sum(labels9s== coffee_test_labels)/20

acc_poszczegolnych_k9s=apply(macierz_klasyfikatorów9s==coffee_test_labels, 2, sum)/20

wyniki_poszczegolnych_s = rbind(acc_poszczegolnych_k1s, acc_poszczegolnych_k3s, acc_poszczegolnych_k5s, acc_poszczegolnych_k7s, acc_poszczegolnych_k9s)

write.csv(wyniki_poszczegolnych_s, '/Users/Ula/Desktop/wtum/wyniki_poszczegolnych_beetlefly_scalone.csv')


# komitet poszczegolnych

# k=1: 17, 42
# k=3: 17, 35
# k=7: 20, 4, 11
# k=9: 11, (20, 21, 30, (31, 

komitet_najlepszych = getLabels(cbind(macierz_klasyfikatorów1s[, c(10, 11)], macierz_klasyfikatorów3s[, 11]))

m2_acc_komitet_najlepszych_s = sum(komitet_najlepszych==coffee_test_labels)/20

table(komitet_najlepszych, coffee_test_labels)
# komitet z 1, 3, 5
etykiety_komitet = getLabels(cbind(labels3s, labels1s, labels5s))
m2_acc_komitet = sum(etykiety_komitet==coffee_test_labels)/20
