source('/Users/Ula/Desktop/wtum/funkcje_pomocnicze2.R')

#----------------------------------- wczytanie danych: gunpoint -----------------------------------------------#
coffee_train <- read.table('/Users/Ula/Desktop/wtum/SonyAIBORobotSurface1_TRAIN.txt')

coffee_test <- read.table('/Users/Ula/Desktop/wtum/SonyAIBORobotSurface1_TEST.txt')

przelomy_coffee <- read.csv('/Users/Ula/Desktop/wtum/przelomy_SonyAIBORobotSurface50.csv')

#----------------------------------- rysowanie szeregu i prze³omów -----------------------------------#

coffee_train_val = GetData(coffee_train)
coffee_train_labels = as.factor(as.character(coffee_train[, 1]))


coffee_test_val = GetData(coffee_test)
coffee_test_labels = coffee_test[, 1]
coffe_labels_factor = as.factor(as.character(coffee_test[, 1]))

przelomy = przelomy_coffee$x

# niektore kolumny psuja
coffee_test_val = coffee_test_val[-c(20,21), ]
coffee_train_val = coffee_train_val[-c(20, 21), ]



# --------------------------------- szukanie etykiet ---------------------------#
knn_zwykle_coffee1 = knn(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=1)

acc1z=sum(knn_zwykle_coffee1==coffe_labels_factor)/length(coffe_labels_factor)

# k=3
knn_zwykle_coffee3= knn(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=3)

acc3z=sum(knn_zwykle_coffee3==coffe_labels_factor)/length(coffe_labels_factor)

# k=5
knn_zwykle_coffee5= knn(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=5)

acc5z=sum(knn_zwykle_coffee5==coffe_labels_factor)/length(coffe_labels_factor)

# k=7
knn_zwykle_coffee7= knn(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=7)

acc7z=sum(knn_zwykle_coffee7==coffe_labels_factor)/length(coffe_labels_factor)

# k=9
knn_zwykle_coffee9= knn(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=9)

acc9z=sum(knn_zwykle_coffee9==coffe_labels_factor)/length(coffe_labels_factor)

# komitet 3 najlepsze

komitetz = cbind(knn_zwykle_coffee3, knn_zwykle_coffee5, knn_zwykle_coffee1)

predykcje_komitet_z = getLabels(komitetz)

acckomz = sum(predykcje_komitet_z==coffe_labels_factor)/length(coffe_labels_factor)


# ---------------------------------- METODA 1 ---------------------------------------- #

#k=1
knn_coffee=knnStructbreak(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=1, breaks = przelomy[-c(4:5)])
predykcje1k = getLabels(knn_coffee)
acc1k=sum(predykcje1k==coffee_test_labels)/length(coffee_test_labels)

#k=3
knn_coffee=knnStructbreak(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=3, breaks = przelomy[-c(4,5)])

predykcje3k = getLabels(knn_coffee) 
acc3k=sum(predykcje3k==coffee_test_labels)/length(coffee_test_labels)



#k=5
knn_coffee=knnStructbreak(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=5, breaks = przelomy[-c(4,5)])

predykcje5k = getLabels(knn_coffee) 
acc5k=sum(predykcje5k==coffee_test_labels)/length(coffee_test_labels)

#k=7
knn_coffee=knnStructbreak(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=7, breaks = przelomy[-c(4,5)])

predykcje7k = getLabels(knn_coffee) 
acc7k=sum(predykcje7k==coffee_test_labels)/length(coffee_test_labels)



#k=9
knn_coffee=knnStructbreak(t(coffee_train_val)[, c(1:40, 42:44, 46:58, 61:62)], t(coffee_test_val)[, c(1:40, 42:44, 46:58, 61:62)], coffee_train_labels, k=9, breaks = przelomy[-c(4,5)])

predykcje9k = getLabels(knn_coffee) 
acc9k=sum(predykcje9k==coffee_test_labels)/length(coffee_test_labels)



# komitet 3 najlepszych klasyfikatorow: k=1,3,7

komitet=cbind(predykcje1k, predykcje3k)
predykcje_komitet = getLabels(komitet)

acckom=sum(predykcje_komitet==coffee_test_labels)/length(coffee_test_labels) #0.92
table(predykcje1k, coffee_test_labels)
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
# odcinki 8, 9, 18, 20, 21, 32, 33, 35 maja najwieksza wariancje - tych odcinkow nie bierzemy pod uwagê przy knn

#---------------- polaczenie krotkich odcinkow z dluzszymi------------

przelomy2 = c(1, 24, 53, 75,89, 109, 128, 147, 163, 180,193, 205, 230, 247, 267, 289,322,340, 368,376,
              395, 413, 422, 447,463, 480,499,526, 542)

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

komitet2=cbind(predykcje7k2, predykcje3k2, predykcje1k2)
predykcje_komitet2 = getLabels(komitet2)

acckom2=sum(predykcje_komitet2==coffee_test_labels)/length(coffee_test_labels) #0.92


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
coffee_test_val= coffee_test_val[-76, ]
coffee_train_val = coffee_train_val[-76, ]

transposed = t(coffee_test_val)

macierz_klasyfikatorów1 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

#---------------#


labels1=getLabels(macierz_klasyfikatorów1)

m2_acc_k1 = sum(labels1 == coffee_test_labels)/150

acc_poszczegolnych_k1=apply(macierz_klasyfikatorów1==coffee_test_labels, 2, sum)/150 # 0.9067 najlepiej

# k = 3

macierz_klasyfikatorów3 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels3=getLabels(macierz_klasyfikatorów3)

m2_acc_k3= sum(labels3 == coffee_test_labels)/150


acc_poszczegolnych_k3=apply(macierz_klasyfikatorów3==coffee_test_labels, 2, sum)/150 # najlepszy 0.88


#k = 5
macierz_klasyfikatorów5 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels5=getLabels(macierz_klasyfikatorów5)

m2_acc_k5 = sum(labels5 == coffee_test_labels)/150

acc_poszczegolnych_k5=apply(macierz_klasyfikatorów5==coffee_test_labels, 2, sum)/150 # najlepszy 0.87


# k = 7
macierz_klasyfikatorów7 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels7=getLabels(macierz_klasyfikatorów7)

m2_acc_k7 = sum(labels7 == coffee_test_labels)/150

acc_poszczegolnych_k7=apply(macierz_klasyfikatorów7==coffee_test_labels, 2, sum)/150

# k = 9
macierz_klasyfikatorów9 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels9=getLabels(macierz_klasyfikatorów9)

m2_acc_k9 = sum(labels9 == coffee_test_labels)/150

acc_poszczegolnych_k9=apply(macierz_klasyfikatorów9==coffee_test_labels, 2, sum)/150

wyniki_poszczegolnych = rbind(acc_poszczegolnych_k1, acc_poszczegolnych_k3, acc_poszczegolnych_k5, acc_poszczegolnych_k7, acc_poszczegolnych_k9)

write.csv(wyniki_poszczegolnych, '/Users/Ula/Desktop/wtum/wyniki_poszczegolnych_coffee.csv')


# komitet poszczegolnych
acc_poszczegolnych_k1

#14, 32, 40
komitet_najlepszych = getLabels(macierz_klasyfikatorów1[, c(14, 32, 40)])
m2_acc_komitet_najlepszych = sum(komitet_najlepszych==coffee_test_labels)/150 #0.94

table(komitet_najlepszych,coffee_test_labels)

# komitet z 1, 3, 5
etykiety_komitet = getLabels(cbind(labels1, labels3, labels5))
m2_acc_komitet = sum(etykiety_komitet==coffee_test_labels)/150
