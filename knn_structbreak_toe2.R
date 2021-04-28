source('/Users/Ula/Desktop/wtum/funkcje_pomocnicze2.R')

#----------------------------------- wczytanie danych: gunpoint -----------------------------------------------#
coffee_train <- read.table('/Users/Ula/Desktop/wtum/ToeSegmentation2_TRAIN.txt')

coffee_test <- read.table('/Users/Ula/Desktop/wtum/ToeSegmentation2_TEST.txt')


#----------------------------------- rysowanie szeregu i prze³omów -----------------------------------#

coffee_train_val = GetData(coffee_train)
coffee_train_labels = as.factor(as.character(coffee_train[, 1]))


coffee_test_val = GetData(coffee_test)
coffee_test_labels = coffee_test[, 1]
coffe_labels_factor = as.factor(as.character(coffee_test[, 1]))

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

predykcje_komitet_z = getLabels(komitetz) - 1

acckomz = sum(predykcje_komitet_z==coffe_labels_factor)/length(coffe_labels_factor)

table(predykcje_komitet_z, coffe_labels_factor)
