source('/Users/Ula/Desktop/wtum/funkcje_pomocnicze2.R')

#----------------------------------- wczytanie danych: gunpoint -----------------------------------------------#
coffee_train <- read.table('/Users/Ula/Desktop/wtum/Meat_TRAIN.txt')

coffee_test <- read.table('/Users/Ula/Desktop/wtum/Meat_TEST.txt')

przelomy = read.table('/Users/Ula/Desktop/wtum/przelomy_meat.csv')
przelomy = c(1, 3, 14, 17, 20, 68, 75, 79, 86, 136, 180, 236, 238, 241, 273, 281, 312, 322, 337, 347, 377, 381, 386, 439)

macierz_przelomow_coffee <- read.csv('/Users/Ula/Desktop/wtum/macierz_przelomow_meat.csv')

scalona_macierz_przelomow = apply(macierz_przelomow_coffee[, -1], 1, function(x){
  
  ncol = ncol(macierz_przelomow_coffee)
  
  diff= diff(x)
  
  ids = which(diff < 8)
  
  scalone_przelomy = x[-(ids + 1)]
  
  scalone_przelomy = c(scalone_przelomy, rep(NA, ncol-length(scalone_przelomy)))
  
  return(scalone_przelomy)
})

scalona_macierz_przelomow = t(scalona_macierz_przelomow)

#---------------------------------- rysowanie szeregu i prze³omów -----------------------------------#

coffee_train_val = GetData(coffee_train)
coffee_train_labels = as.factor(as.character(coffee_train[, 1]))


coffee_test_val = GetData(coffee_test)
coffee_test_labels = coffee_test[, 1]
coffe_labels_factor = as.factor(as.character(coffee_test[, 1]))


DrawSeries2(t(coffee_train_val), przelomy)
# mozna na 3 sposoby zrobic: tak jak znalezione przelomy, wyrzucajac te o duzej wariancji i krotkie, scalic krotkie z dluzszymi
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

komitetz = cbind(knn_zwykle_coffee3, knn_zwykle_coffee5, knn_zwykle_coffee1)

predykcje_komitet_z = getLabels(komitetz)

acckomz = sum(predykcje_komitet_z==coffe_labels_factor)/length(coffe_labels_factor)


# ----------------------------- METODA 1 ---------------------------------- #

#k=1
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = przelomy)

predykcje1k = getLabels(knn_coffee)
acc1k=sum(predykcje1k==coffee_test_labels)/length(coffee_test_labels)

predykcje_1kv = getLabels(knn_coffee[, -c(5, 11, 14, 20)])
acc1kv=sum(predykcje_1kv==coffee_test_labels)/length(coffee_test_labels)


#k=3
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = przelomy)

predykcje3k = getLabels(knn_coffee) 
acc3k=sum(predykcje3k==coffee_test_labels)/length(coffee_test_labels)

predykcje_3kv = getLabels(knn_coffee[, -c(5, 11, 14, 20)])
acc3kv=sum(predykcje_3kv==coffee_test_labels)/length(coffee_test_labels)


#k=5
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = przelomy)

predykcje5k = getLabels(knn_coffee) 
acc5k=sum(predykcje5k==coffee_test_labels)/length(coffee_test_labels)

predykcje_5kv = getLabels(knn_coffee[, -c(5, 11, 14, 20)])
acc5kv=sum(predykcje_5kv==coffee_test_labels)/length(coffee_test_labels)


#k=7
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = przelomy)

predykcje7k = getLabels(knn_coffee) 
acc7k=sum(predykcje7k==coffee_test_labels)/length(coffee_test_labels)

predykcje_7kv = getLabels(knn_coffee[, -c(5, 11, 14, 20)])
acc7kv=sum(predykcje_7kv==coffee_test_labels)/length(coffee_test_labels)


#k=9
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = przelomy)

predykcje9k = getLabels(knn_coffee) 
acc9k=sum(predykcje9k==coffee_test_labels)/length(coffee_test_labels)

predykcje_9kv = getLabels(knn_coffee[, -c(5, 11, 14, 20)])
acc9kv=sum(predykcje_9kv==coffee_test_labels)/length(coffee_test_labels)


# komitet 3 najlepszych klasyfikatorow: k=1,3,7

komitet=cbind(predykcje5k, predykcje3k, predykcje7k)
predykcje_komitet = getLabels(komitet)

acckom=sum(predykcje_komitet==coffee_test_labels)/length(coffee_test_labels) #0.92

# po wyrzuceniu odcinków z najwiêksz¹ wariancj¹

komitetv = cbind(predykcje_1kv, predykcje_3kv, predykcje_5kv)
predykcje_komitetv = getLabels(komitetv)
acckomv=sum(predykcje_komitetv==coffee_test_labels)/length(coffee_test_labels)


# sprawdzenie ktore odcinki maja najwieksza sume wariancji (formalnie, bo to widac tez z obrazka)

odcinki = c(przelomy, nrow(coffee_train_val))
for(i in 1:(length(odcinki)-1)){
  
  odcinek = t(coffee_train_val)[, odcinki[i]:odcinki[i+1]]
  print(paste0('Odcinek:', i))
  print(sum(var(odcinek)))
}
# odcinki 5, 11, 14, 20 maja najwieksza wariancje - tych odcinkow nie bierzemy pod uwagê przy knn

#---------------- polaczenie krotkich odcinkow z dluzszymi------------

przelomy2 = c(1, 14, 20,68, 86, 136, 180, 238, 273, 281, 312, 322, 337, 347, 377, 386, 439)

#k=1
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = przelomy2)

predykcje1k2 = getLabels(knn_coffee)
acc1k2=sum(predykcje1k2==coffee_test_labels)/length(coffee_test_labels)

#k=3
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = przelomy2)

predykcje3k2 = getLabels(knn_coffee) 
acc3k2=sum(predykcje3k2==coffee_test_labels)/length(coffee_test_labels)

#k=5
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = przelomy2)

predykcje5k2 = getLabels(knn_coffee) 
acc5k2=sum(predykcje5k2==coffee_test_labels)/length(coffee_test_labels)


#k=7
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = przelomy2)

predykcje7k2 = getLabels(knn_coffee) 
acc7k2=sum(predykcje7k2==coffee_test_labels)/length(coffee_test_labels)


#k=9
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = przelomy2)

predykcje9k2 = getLabels(knn_coffee) 
acc9k2=sum(predykcje9k2==coffee_test_labels)/length(coffee_test_labels)

#k=11
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=11, breaks = przelomy2)

predykcje11k2 = getLabels(knn_coffee) 
acc9112=sum(predykcje11k2==coffee_test_labels)/length(coffee_test_labels)


# komitet 3 najlepszych klasyfikatorow: k=1,3,7

komitet2=cbind(predykcje5k2, predykcje9k2, predykcje7k2)
predykcje_komitet2 = getLabels(komitet2)

acckom2=sum(predykcje_komitet2==coffee_test_labels)/length(coffee_test_labels) #0.92


# ----------------------------- METODA 2 ---------------------------------- #
macierz_przelomow_coffee = macierz_przelomow_coffee[, -1]

# dla kazdego wiersza powyzszej macierzy chcemy: 
# zrobic przelomy = c(wiersz, nrow(coffee_train_val))
# puscic knnStructbreak(train, test, label, k, przelomy)
# puscic getlabel dla powyzszego
# takie get label mozna zapisac w macierzy, potem znowu sie pusci getLabel dla calej macierzy

# w tym zbiorze danych psuje coœ przy zmiennej V78, knn zwraca NaN. Zarowno w treningowym i testowym
# ta zmienna to same 0, wiec ja wyrzucam, nic nie tracimy.

# dla k=1

macierz_klasyfikatorów1 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

#---------------#


labels1=getLabels(macierz_klasyfikatorów1)

m2_acc_k1 = sum(labels1 == coffee_test_labels)/60

acc_poszczegolnych_k1=apply(macierz_klasyfikatorów1==coffee_test_labels, 2, sum)/60 # 0.9067 najlepiej

# k = 3

macierz_klasyfikatorów3 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels3=getLabels(macierz_klasyfikatorów3)

m2_acc_k3= sum(labels3 == coffee_test_labels)/60


acc_poszczegolnych_k3=apply(macierz_klasyfikatorów3==coffee_test_labels, 2, sum)/60 # najlepszy 0.88


#k = 5
macierz_klasyfikatorów5 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels5=getLabels(macierz_klasyfikatorów5)

m2_acc_k5 = sum(labels5 == coffee_test_labels)/60

acc_poszczegolnych_k5=apply(macierz_klasyfikatorów5==coffee_test_labels, 2, sum)/60 # najlepszy 0.87


# k = 7
macierz_klasyfikatorów7 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels7=getLabels(macierz_klasyfikatorów7)

m2_acc_k7 = sum(labels7 == coffee_test_labels)/60

acc_poszczegolnych_k7=apply(macierz_klasyfikatorów7==coffee_test_labels, 2, sum)/60

# k = 9
macierz_klasyfikatorów9 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels9=getLabels(macierz_klasyfikatorów9)

m2_acc_k9 = sum(labels9 == coffee_test_labels)/60

acc_poszczegolnych_k9=apply(macierz_klasyfikatorów9==coffee_test_labels, 2, sum)/60

wyniki_poszczegolnych = rbind(acc_poszczegolnych_k1, acc_poszczegolnych_k3, acc_poszczegolnych_k5, acc_poszczegolnych_k7, acc_poszczegolnych_k9)

write.csv(wyniki_poszczegolnych, '/Users/Ula/Desktop/wtum/wyniki_poszczegolnych_meat.csv')


# komitet poszczegolnych
# najlepsze: k5, k7 zm.4, k3 zm.6, k5 zm. 15, k5, zm. 20, k9 zm. 20

najlepsze = cbind(macierz_klasyfikatorów5[, c(4, 15, 20)], macierz_klasyfikatorów9[, 20], macierz_klasyfikatorów3[, 6], macierz_klasyfikatorów7[, 4])
#14, 32, 40
komitet_najlepszych = getLabels(najlepsze)
m2_acc_komitet_najlepszych = sum(komitet_najlepszych==coffee_test_labels)/60 #0.94

table(komitet_najlepszych,coffee_test_labels)

# komitet z 1, 3, 5
etykiety_komitet = getLabels(cbind(labels9, labels7, labels5))
m2_acc_komitet = sum(etykiety_komitet==coffee_test_labels)/60


##------------------------------------- DLA SCALONEJ MACIERZY PRZELOMOW ---------------------- #

macierz_klasyfikatorów1s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels1s=getLabels(macierz_klasyfikatorów1s)

m2_acc_k1s = sum(labels1s == coffee_test_labels)/60

acc_poszczegolnych_k1s=apply(macierz_klasyfikatorów1s==coffee_test_labels, 2, sum)/60 # 0.96

# k = 3

macierz_klasyfikatorów3s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels3s=getLabels(macierz_klasyfikatorów3s)


m2_acc_k3s= sum(labels3s == coffee_test_labels)/60

acc_poszczegolnych_k3s=apply(macierz_klasyfikatorów3s ==coffee_test_labels, 2, sum)/60


#k = 5
macierz_klasyfikatorów5s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels5s=getLabels(macierz_klasyfikatorów5s)

m2_acc_k5 = sum(labels5s == coffee_test_labels)/60

acc_poszczegolnych_k5s=apply(macierz_klasyfikatorów5s==coffee_test_labels, 2, sum)/60


# k = 7
macierz_klasyfikatorów7s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels7s=getLabels(macierz_klasyfikatorów7s)

m2_acc_k7s = sum(labels7s == coffee_test_labels)/60

acc_poszczegolnych_k7s=apply(macierz_klasyfikatorów7s==coffee_test_labels, 2, sum)/60

# k = 9
macierz_klasyfikatorów9s = apply(scalona_macierz_przelomow,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels9s=getLabels(macierz_klasyfikatorów9s)

m2_acc_k9s = sum(labels9s== coffee_test_labels)/60

acc_poszczegolnych_k9s=apply(macierz_klasyfikatorów9s==coffee_test_labels, 2, sum)/60

wyniki_poszczegolnych_s = rbind(acc_poszczegolnych_k1s, acc_poszczegolnych_k3s, acc_poszczegolnych_k5s, acc_poszczegolnych_k7s, acc_poszczegolnych_k9s)

write.csv(wyniki_poszczegolnych_s, '/Users/Ula/Desktop/wtum/wyniki_poszczegolnych_meat_scalone.csv')


# komitet poszczegolnych

# k=1: 17, 42
# k=3: 17, 35
# k=7: 20, 4, 11
# k=9: 11, (20, 21, 30, (31, 

komitet_najlepszych = getLabels(cbind(macierz_klasyfikatorów9s[, c(31,20)]))

m2_acc_komitet_najlepszych_s = sum(komitet_najlepszych==coffee_test_labels)/54

table(komitet_najlepszych, coffee_test_labels)
# komitet z 1, 3, 5
etykiety_komitet = getLabels(cbind(labels3s, labels7s, labels5s))
m2_acc_komitet = sum(etykiety_komitet==coffee_test_labels)/60


