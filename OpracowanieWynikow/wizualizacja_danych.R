source('/Users/urszulabialonczyk/Documents/WTUM/funkcje_pomocnicze2.R')

# ---------------------------------- COFFEE -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets//Coffee/Coffee_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets//Coffee/Coffee_TEST.txt')

przelomy_coffee <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets//Coffee/przelomy_coffee.csv') # jako factory sie zapisaly

macierz_przelomow_coffee <- read.csv('/Users/Ula/Desktop/wtum/macierz_przelomow.csv')

DrawSeries2(t(coffee_train_val), c(1,28,52,79,85,107,132, 157, 182, 206, 227, 247, 259))
DrawSeries2(t(coffee_test_val), c(1,28,52,79,85,107,132, 157, 182, 206, 227, 247, 259))


# ---------------------------------- WINE -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/Wine_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/Wine_TEST.txt')

przelomy_coffee <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/przelomy_wine.csv') # jako factory sie zapisaly

macierz_przelomow_coffee <- read.csv('/Users/Ula/Desktop/wtum/macierz_przelomow.csv')

DrawSeries2(t(coffee_train_val), c(1,28,52,79,85,107,132, 157, 182, 206, 227, 247, 259))
DrawSeries2(t(coffee_test_val), c(1,28,52,79,85,107,132, 157, 182, 206, 227, 247, 259))

# ---------------------------------- Meat -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Meat/Meat_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Wine/Wine_TEST.txt')

przelomy_coffee <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Meat/przelomy_meat.csv') # jako factory sie zapisaly

przelomy_scalone = c(1,14,20,68,86,136,180,238,273,281,312,322,337,347,377,386,439)

macierz_przelomow_coffee <- read.csv('/Users/Ula/Desktop/wtum/macierz_przelomow_meat.csv')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)



DrawSeries2(t(coffee_train_val), as.numeric(przelomy_coffee$V1[-1]))
DrawSeries2(t(coffee_test_val), as.numeric(przelomy_coffee$V1[-1]))

DrawSeries2(t(coffee_train_val), przelomy_scalone)
DrawSeries2(t(coffee_test_val), przelomy_scalone)


# ---------------------------------- Olive -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/OliveOil/OliveOil_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/OliveOil/OliveOil_TEST.txt')

przelomy_coffee <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/OliveOil/przelomy_olive50.csv') # jako factory sie zapisaly


coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)



DrawSeries2(t(coffee_train_val), as.numeric(przelomy_coffee$V1[-1]))
DrawSeries2(t(coffee_test_val), as.numeric(przelomy_coffee$V1[-1]))

przelomy_scalone = c(1,24,53,75,89,109,128,147,163,180,193,205,230,247,267,289,322,340,368,376,395,413,422,447,463,480,499,526,542)
DrawSeries2(t(coffee_train_val), przelomy_scalone)
DrawSeries2(t(coffee_test_val), przelomy_scalone)


# ---------------------------------- Toe1 -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/ToeSegmentation1/ToeSegmentation1_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/ToeSegmentation1/ToeSegmentation1_TEST.txt')


coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

przelomy= c(1, 4, 6, 18, 56, 60,62,94,96,126,129,168,199,223,226,247)


DrawSeries2(t(coffee_train_val), przelomy)
DrawSeries2(t(coffee_test_val), przelomy)

# ---------------------------------- GUN -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/GunPoint/GunPoint_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/GunPoint/GunPoint_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

macierz_przelomow_coffee <- read.csv('/Users/urszulabialonczyk/Documents/WTUM/Datasets/GunPoint/macierz_przelomow_gun.csv')

DrawSeries2(t(coffee_train_val), macierz_przelomow_coffee[14, 1:11])
DrawSeries2(t(coffee_test_val), macierz_przelomow_coffee[14, 1:11])

# ---------------------------------- DIATOM -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/DiatomSizeReduction/DiatomSizeReduction_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/DiatomSizeReduction/DiatomSizeReduction_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

macierz_przelomow_coffee <- read.csv('/Users/urszulabialonczyk/Documents/WTUM/Datasets/DiatomSizeReduction/macierz_przelomow_diatom.csv')

DrawSeries2(t(coffee_train_val), macierz_przelomow_coffee[1, 1:24])
DrawSeries2(t(coffee_test_val), macierz_przelomow_coffee[1, 1:24])

# ---------------------------------- HERRING -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Herring/Herring_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Herring/Herring_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

macierz_przelomow_coffee <- read.csv('/Users/urszulabialonczyk/Documents/WTUM/Datasets/Herring/macierz_przelomow_herring.csv')

DrawSeries2(t(coffee_train_val), macierz_przelomow_coffee[2, 1:34])
DrawSeries2(t(coffee_test_val), macierz_przelomow_coffee[2, 1:34])


# ---------------------------------- BEETLE -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/BeetleFly/BeetleFly_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/BeetleFly/BeetleFly_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

przelomy= c(1, 10, 23, 34, 45, 52,63,94,109,117,158,169,176,185,189,210,222,257,263,278,283,292,299,309,314,322,335,341,353,360,399,406,417,424,431,439,451,462,467,472,503,508)

DrawSeries2(t(coffee_train_val),przelomy)
DrawSeries2(t(coffee_test_val), przelomy)

# ---------------------------------- TOE2 -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/ToeSegmentation2/ToeSegmentation2_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/ToeSegmentation2/ToeSegmentation2_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)


DrawSeries2(t(coffee_train_val),500)
DrawSeries2(t(coffee_test_val), 500)


# ---------------------------------- BIRD -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/BirdChicken/BirdChicken_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/BirdChicken/BirdChicken_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

przelomy = c(1,12,19,25,33,38,65,70,100,112,126,148,155,159,168,174,193,223,228,239,286,322,326,330,333,340,345,355,368,390,407,420,424,428,432,441,448,454)
DrawSeries2(t(coffee_train_val),przelomy)
DrawSeries2(t(coffee_test_val), przelomy)

# ---------------------------------- ECG -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/TwoLeadECG/TwoLeadECG_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/TwoLeadECG/TwoLeadECG_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

macierz_przelomow_coffee <- read.csv('/Users/urszulabialonczyk/Documents/WTUM/Datasets/TwoLeadECG/macierz_przelomow_ecg.csv')


DrawSeries2(t(coffee_train_val),macierz_przelomow_coffee[2, 1:6])
DrawSeries2(t(coffee_test_val), macierz_przelomow_coffee[2, 1:6])

# ---------------------------------- ECG -------------------------- #
coffee_train <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/SonyAIBORobotSurface1/SonyAIBORobotSurface1_TRAIN.txt')

coffee_test <- read.table('/Users/urszulabialonczyk/Documents/WTUM/Datasets/SonyAIBORobotSurface1/SonyAIBORobotSurface1_TEST.txt')

coffee_train_val = GetData(coffee_train)
coffee_test_val = GetData(coffee_test)

przelomy <- read.csv('/Users/urszulabialonczyk/Documents/WTUM/Datasets/SonyAIBORobotSurface1/przelomy_SonyAIBORobotSurface50.csv')


DrawSeries2(t(coffee_train_val),as.numeric(przelomy$x))
DrawSeries2(t(coffee_test_val), as.numeric(przelomy$x))

