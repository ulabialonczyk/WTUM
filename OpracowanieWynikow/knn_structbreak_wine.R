source('/Users/Ula/Desktop/wtum/funkcje_pomocnicze2.R')

#----------------------------------- wczytanie danych (nazywaja sie coffee, ale to zbior wine)-----------------------------------------------#
coffee_train <- read.table('/Users/Ula/Desktop/wtum/Wine_TRAIN.txt')

coffee_test <- read.table('/Users/Ula/Desktop/wtum/Wine_TEST.txt')

przelomy_coffee <- read.table('/Users/Ula/Desktop/wtum/przelomy_wine.csv') # jako factory sie zapisaly

macierz_przelomow_coffee <- read.csv('/Users/Ula/Desktop/wtum/macierz_przelomow_wine.csv')

#----------------------------------- rysowanie szeregu i prze³omów -----------------------------------#

coffee_train_val = GetData(coffee_train)
coffee_train_labels = as.factor(as.character(coffee_train[, 1]))


coffee_test_val = GetData(coffee_test)
coffee_test_labels = coffee_test[, 1]
coffe_labels_factor = as.factor(as.character(coffee_test[, 1]))


DrawSeries2(t(coffee_train_val), c(1,23,79,131, 183))
DrawSeries2(t(coffee_test_val), c(1,23,79,131, 183))


# --------------------------------- szukanie etykiet ---------------------------#
przelomy_coffee = c(1,23,79,131, 183)

#k=1
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = przelomy_coffee)

predykcje1k = getLabels(knn_coffee)
acc1k=sum(predykcje1k==coffee_test_labels)/length(coffee_test_labels)
table(predykcje1k, coffee_test_labels) #20 7 7 20

predykcje_1kv = getLabels(knn_coffee[, -c(2,4)])
acc1kv=sum(predykcje_1kv==coffee_test_labels)/length(coffee_test_labels)


#k=3
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = przelomy_coffee)

predykcje3k = getLabels(knn_coffee) 
acc3k=sum(predykcje3k==coffee_test_labels)/length(coffee_test_labels)

predykcje_3kv = getLabels(knn_coffee[, -c(2,4)])
acc3kv=sum(predykcje_3kv==coffee_test_labels)/length(coffee_test_labels)


#k=5
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = przelomy_coffee)

predykcje5k = getLabels(knn_coffee) 
acc5k=sum(predykcje5k==coffee_test_labels)/length(coffee_test_labels)

predykcje_5kv = getLabels(knn_coffee[, -c(2,4)])
acc5kv=sum(predykcje_5kv==coffee_test_labels)/length(coffee_test_labels)


#k=7
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = przelomy_coffee)

predykcje7k = getLabels(knn_coffee) 
acc7k=sum(predykcje7k==coffee_test_labels)/length(coffee_test_labels)

predykcje_7kv = getLabels(knn_coffee[, -c(2,4)])
acc7kv=sum(predykcje_7kv==coffee_test_labels)/length(coffee_test_labels)


#k=9
knn_coffee=knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = przelomy_coffee)

predykcje9k = getLabels(knn_coffee) 
acc9k=sum(predykcje9k==coffee_test_labels)/length(coffee_test_labels)

predykcje_9kv = getLabels(knn_coffee[, -c(2,4)])
acc9kv=sum(predykcje_9kv==coffee_test_labels)/length(coffee_test_labels)


# komitet 3 najlepszych klasyfikatorow: k=1,3,7

komitet=cbind(predykcje1k, predykcje3k, predykcje7k)
predykcje_komitet = getLabels(komitet)

acckom=sum(predykcje_komitet==coffee_test_labels)/length(coffee_test_labels)

# po wyrzuceniu odcinków z najwiêksz¹ wariancj¹

komitetv = cbind(predykcje_5kv, predykcje_7kv, predykcje_9kv)
predykcje_komitetv = getLabels(komitetv)
acckomv=sum(predykcje_komitetv==coffee_test_labels)/length(coffee_test_labels)


# sprawdzenie ktore odcinki maja najwieksza sume wariancji (formalnie, bo to widac tez z obrazka)

odcinki = c(przelomy_coffee, nrow(coffee_train_val))
for(i in 1:(length(odcinki)-1)){
  
  odcinek = t(coffee_train_val)[, odcinki[i]:odcinki[i+1]]
  print(paste0('Odcinek:', i))
  print(sum(var(odcinek)))
}
# odcinki 2,4 maja najwieksza wariancje - tych odcinkow nie bierzemy pod uwagê przy knn

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

komitetz = cbind(knn_zwykle_coffee3, knn_zwykle_coffee5, knn_zwykle_coffee7)

predykcje_komitet_z = getLabels(komitetz)

acckomz = sum(predykcje_komitet_z==coffee_test_labels)/length(coffe_labels_factor)


# ----------------------------- METODA 2 ---------------------------------- #
macierz_przelomow_coffee

# dla kazdego wiersza powyzszej macierzy chcemy: 
# zrobic przelomy = c(wiersz, nrow(coffee_train_val))
# puscic knnStructbreak(train, test, label, k, przelomy)
# puscic getlabel dla powyzszego
# takie get label mozna zapisac w macierzy, potem znowu sie pusci getLabel dla calej macierzy

# dla k=1

macierz_klasyfikatorów1 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels1=getLabels(macierz_klasyfikatorów1)
macierz_etykiet = labels1

for(i in 2:nrow(macierz_przelomow_coffee)){
  
  breaks = macierz_przelomow_coffee[i, ][!is.na(macierz_przelomow_coffee[i, ])]
  
  knn = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=1, breaks = breaks)
  
  macierz_etykiet = cbind(macierz_etykiet, getLabels(knn))
  
  
  
}
etykietyk1 = getLabels(macierz_etykiet)
m2_acc_k1 = sum(etykietyk1 == coffee_test_labels)/54

# klasyfikator 5, k=1; klasyfikator 28, k=1; klasyfikator 10, k=1; 26 k=1; 23 k=1

#komitet_najlepszych = getLabels(macierz_etykiet[, c(5, 26, 28, 23, 10)])
#sum(komitet_najlepszych==coffee_test_labels)/28

acc_poszczegolnych_k1=apply(macierz_etykiet==coffee_test_labels, 2, sum)/54

# k = 3

macierz_klasyfikatorów3 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels3=getLabels(macierz_klasyfikatorów3)
macierz_etykiet = labels3

for(i in 2:nrow(macierz_przelomow_coffee)){
  
  breaks = macierz_przelomow_coffee[i, ][!is.na(macierz_przelomow_coffee[i, ])]
  
  knn = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=3, breaks = breaks)
  
  macierz_etykiet = cbind(macierz_etykiet, getLabels(knn))
  
  
  
}
etykietyk3 = getLabels(macierz_etykiet)
m2_acc_k3= sum(etykietyk3 == coffee_test_labels)/54

macierz_etykiet

acc_poszczegolnych_k3=apply(macierz_etykiet==coffee_test_labels, 2, sum)/54


#k = 5
macierz_klasyfikatorów5 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels5=getLabels(macierz_klasyfikatorów5)
macierz_etykiet = labels5

for(i in 2:nrow(macierz_przelomow_coffee)){
  
  breaks = macierz_przelomow_coffee[i, ][!is.na(macierz_przelomow_coffee[i, ])]
  
  knn = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=5, breaks = breaks)
  
  macierz_etykiet = cbind(macierz_etykiet, getLabels(knn))
  
  
  
}
etykietyk5 = getLabels(macierz_etykiet)
m2_acc_k5 = sum(etykietyk5 == coffee_test_labels)/54

acc_poszczegolnych_k5=apply(macierz_etykiet==coffee_test_labels, 2, sum)/54


# k = 7
macierz_klasyfikatorów7 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels7=getLabels(macierz_klasyfikatorów7)
macierz_etykiet = labels7

for(i in 2:nrow(macierz_przelomow_coffee)){
  
  breaks = macierz_przelomow_coffee[i, ][!is.na(macierz_przelomow_coffee[i, ])]
  
  knn = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=7, breaks = breaks)
  
  macierz_etykiet = cbind(macierz_etykiet, getLabels(knn))
  
  
  
}
etykietyk7= getLabels(macierz_etykiet)
m2_acc_k7 = sum(etykietyk7 == coffee_test_labels)/54

acc_poszczegolnych_k7=apply(macierz_etykiet==coffee_test_labels, 2, sum)/54

# k = 9
macierz_klasyfikatorów9 = apply(macierz_przelomow_coffee,1, function(x){
  
  x = x[!is.na(x)]
  knn1 = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = x)
  labels1 = getLabels(knn1)
  
  return(labels1)
  
})

labels9=getLabels(macierz_klasyfikatorów9)
macierz_etykiet = labels9

for(i in 2:nrow(macierz_przelomow_coffee)){
  
  breaks = macierz_przelomow_coffee[i, ][!is.na(macierz_przelomow_coffee[i, ])]
  
  knn = knnStructbreak(t(coffee_train_val), t(coffee_test_val), coffee_train_labels, k=9, breaks = breaks)
  
  macierz_etykiet = cbind(macierz_etykiet, getLabels(knn))
  
  
  
}
etykietyk9 = getLabels(macierz_etykiet)
m2_acc_k9 = sum(etykietyk9== coffee_test_labels)/54

acc_poszczegolnych_k9=apply(macierz_etykiet==coffee_test_labels, 2, sum)/54

wyniki_poszczegolnych = rbind(acc_poszczegolnych_k1, acc_poszczegolnych_k3, acc_poszczegolnych_k5, acc_poszczegolnych_k7, acc_poszczegolnych_k9)

write.csv(wyniki_poszczegolnych, '/Users/Ula/Desktop/wtum/wyniki_poszczegolnych_wine.csv')


# komitet poszczegolnych

# klasyfikator 5, k=1; klasyfikator 28, k=1; klasyfikator 10, k=1; 26 k=1; 23 k=1



# komitet z 3,7,9
etykiety_komitet = getLabels(cbind(etykietyk3, etykietyk7, etykietyk9))
m2_acc_komitet = sum(etykiety_komitet==coffee_test_labels)/54
