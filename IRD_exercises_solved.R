#Shemet Yaraslau
sito <- function(n){
  if (n>1){ #warunek na [2, n]
    A <- 2:n #zadaje macierz
    count <- 2 #liczba 2 na pewno pierwsza 
    while(count < sqrt(n)){ #póki bieżąca podejrzana pierwsza liczba jest mniejsza od pierwiastka z n 
      A[(A >= count **2) & (A %% count == 0)] <- 0 #te, liczby, co są większe od 2*i, a są wielokrotnością biezacej podejrzanej (czyli reszta z dzielenia jest rowna 0)
      count <- min(A[A>count]) #zmieniamy biezacy poziom na nastepna minimalna 
    }
    A <- A[A!=0] #zostawiamy same pierwsze (innym nadałem wartosc 0 a.k.a FALSE)
    cat('Liczby pierwsze z przedziału: ', A)
  } else {
    cat('Podaj liczbę większą niż 2')
  }
}

sito(1)
sito(7)
sito(44)  

#2
library(lattice)
ethanol <- ethanol
stats <- ethanol %>% 
  group_by(C) %>% 
  summarise(min_NOx = min(NOx),
            median_NOx = median(NOx),
            mean_NOx = mean(NOx),
            q3_NOx = quantile(NOx, prob=0.75),
            max_NOx = max(NOx)) %>% 
  arrange(desc(mean_NOx))
stats
#c=12 najwyzsza srednia dla NOx
ggplot(ethanol)+
  geom_point(aes(x=E, y=NOx, col=C))+
  xlab('Jakosc mieszanki')+
  ylab('Ilosc tlenku azotu')+
  ggtitle('Ilosc tlenku azotu w zaleznosci od stopnia kompresji silnika i mieszanki')

#3
rm(list = ls())
library(caret)
data('GermanCredit')
data <- GermanCredit
rm(GermanCredit)
data$Class <- as.factor(data$Class)
set.seed(108887)
test_prop <- 0.2
test.set.index <- (runif(nrow(data)) < test_prop)
tdf.test <- data[test.set.index, ]
tdf.train <- data[!test.set.index, ]  

tree <- rpart(Class ~ .,
              data = tdf.train,
              method = "class")
rf <- randomForest(Class ~., 
                   data = tdf.train, ntree=250)
varImpPlot(rf)
rf$importance
#Amount


CM <- list()
CM[["tree"]] <- table(predict(tree, new = tdf.test, type = "class"), tdf.test$Class)
CM[["rf"]] <- table(predict(rf, new = tdf.test, type = "class"), tdf.test$Class)
EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  
  sensitivity <- true_positive / condition_positive 
  miss_rate <- 1-sensitivity
  specificity <- true_negative / condition_negative
  fall_out <- 1-specificity
  PLR=sensitivity/fall_out
  return(list(miss_rate=miss_rate,
              PLR = PLR))
}
sapply(CM, EvaluateModel)
#las losowy

preds <- list()
preds[["rf"]] <- as.vector(predict(rf, newdata = tdf.test, type = "prob")[, 2])
(performance(prediction(preds[["rf"]], tdf.test$Class), "auc")@y.values[[1]])
#AUC wynosi 76.31 
for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], tdf.test$Class), "tpr", "fpr"), lwd = 2, colorize = F, col = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

abline(coef = c(0, 1), lty = 2, lwd = 0.5)

legend(0.6, 0.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = rep(1, length(preds))
)


#4
install.packages('Ecdat')
library(Ecdat)
data <- UnempDur
set.seed(108887)
test_prop <- 0.25
test.set.index <- (runif(nrow(data)) < test_prop)
tdf.test <- data[test.set.index, ]
tdf.train <- data[!test.set.index, ] 

lin_m <- lm(tenure ~ ., data = tdf.train)

d.regr <- rpart(tenure ~., data = tdf.train, cp = 0.015)
rpart.plot(d.regr, under=FALSE, fallen.leaves = FALSE)
#jesli wiek jest wiekszy od 47, a zlogarytmowane wynagrodzenie mniejsze od 5.9, to tenure wyniesie 6.2, przy czym takie prognozy stanowia 8% calego zbioru

modele <- list('lin_m'=lin_m, 'd.regr'=d.regr)
OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("Pierwiastek błędu średniokwadratowego RMSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) ))
  
  print("Średni błąd absolutny MAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/nrow(dane) ))
}
OcenaModeli(modele, tdf.test, 'tenure')
#regresja liniowa

#Mazur Aliaksandr
Мазур PL, [30/01/2023 20:25]
# Zadanie 1
twins <- function(liczba_1, liczba_2){
  if (liczba_1 - liczba_2 == 2 | liczba_1 - liczba_2 == -2 & (liczba_1 %% 2) == 1 & (liczba_2 %% 2) == 1 & liczba_1 != 1 & liczba_1/3 == 1 | (liczba_1/3)+2 == 1 | (liczba_1/3)+4 == 1 & (liczba_2/3) == 1 | (liczba_2/3)+2 == 1 | (liczba_1/3)+4 == 1){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

twins(3,5)

# Zadanie 2 
# a
library("caret")
data("GermanCredit")
library("dplyr")
dane <- GermanCredit
dane %>% 
  group_by(ResidenceDuration) %>%
  summarise(mediana = median(Age),
            srednia = mean(Age),
            minimum = min(Age),
            maksimum = max(Age))
# ResidenceDuration dla wartości 4 charakteryzuje się najwyższym średnim wiekiem 

# b 
library("ggplot2")
dane$ForeignWorker <-  as.factor(dane$ForeignWorker)
ggplot(dane, aes(x = ForeignWorker, y = Amount, col = Class)) +
  geom_boxplot() +
  ggtitle("Wartosc kredytow w zaleznosci od ich klasy, w podziale\n na pracownikow krajowych oraz zagranicznych (ForeignWorker")

# Zadanie 3
install.packages("geepack")
library("geepack")
data("seizure")

# a
seizure$trt <- as.factor(seizure$trt)
set.seed(109081)
test_prop <- 0.3 
test.set.index <- (runif(nrow(seizure)) < test_prop)
seizure.test <- seizure[test.set.index, ]
seizure.train <- seizure[!test.set.index, ]

# b 
tree <- rpart(trt ~ .,
              data = seizure.train,
              method = "class",
              control = list(maxdepth = 4))

rf <- randomForest(trt ~., 
                   data = seizure.train,
                   ntree = 300)

# c
varImpPlot(rf)
rf$importance
# Y4 ma najmniejszy wpływ na zmienną prognozowaną 

# d 
CM <- list()
CM[["tree"]] <- table(predict(tree, new = seizure.test, type = "class"), seizure.test$trt)
CM[["rf"]] <- table(predict(rf, new = seizure.test, type = "class"), seizure.test$trt)
EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  false_positive <- classif_mx[1, 2]
  false_negative <- classif_mx[2, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive
  NPV <- true_negative/(false_negative + true_negative)
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(NPV = NPV,
              F1 = F1))
}

sapply(CM, EvaluateModel)
# Las losowy lepiej prognozuje zmienną trt

# e
preds <- list()
preds[["rf"]] <- as.vector(predict(rf, newdata = seizure.test, type = "prob")[, 2])
plot(performance(prediction(preds[["rf"]], seizure.test$trt), "tpr", "fpr"), lwd = 2, colorize = T) 

for (i in 1:length(preds)){
  plot(performance(prediction(preds[[i]], seizure.test$trt), "tpr", "fpr"), lwd = 2, colorize = F, col = i, add = ifelse(i == 1, FALSE, TRUE)) 
}

abline(coef = c(0, 1), lty = 2, lwd = 0.5)

legend(0.6, 0.4, 
       legend = names(preds),
       col = 1:length(preds), 
       lty = rep(1, length(preds))
)

# AUC (Area Under Curve) - pole pod krzywa ROC
(performance(prediction(preds[["rf"]], seizure.test$trt), "auc")@y.values[[1]])

# Zadanie 4
data("esoph")

# a
set.seed(109081)
test_prop <- 0.2
test.set.index <- (runif(nrow(esoph)) < test_prop)
esoph.test <- esoph[test.set.index, ]
esoph.train <- esoph[!test.set.index, ]

# b
reg_lin <- lm(ncases ~ ., 
              data = esoph.train)

tree <- rpart(ncases ~ .,
              data = esoph.train,
              method = "anova",
              cp = 0.08)
# c
tree
rpart.plot(tree)
# Reguład decyzyjna

# d 
modele <- list("tree" = tree, "reg_lin" = reg_lin)
OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("Błąd średniokwadratowegy MSE")
  print(sapply(modele, function(x) sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) )
  
  print("Względny błąd absolutny RAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/sum(abs(dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))) ))
  
}

OcenaModeli(modele, esoph.test, 'ncases')

#Bogdan Yanovich
# Zadanie 1 skumulowane
prz = c(1, 2, 3, 4, 5)

scummulowanyVector = function(vec) {
  len = length(vec)
  newVector = c()
  prevSum = 0
  
  for (i in 1:len) {
    value = vec[i] + prevSum
    prevSum = value
    newVector[i] = value
  }
  return (newVector)
}
answSkum = scummulowanyVector(prz)
answSkum

# Zadanie 2
library(ISLR)
library(dplyr)
library(tidyverse)
Auto <- Auto
Auto %>%
  group_by(year = 78) %>%
  summarise(mean_horsepower=mean(horsepower), max_acceleration = max(acceleration), min_weight = min(weight))
#Srednia moc silnika jest równa 104 hp.

ggplot(Auto, aes(x=Auto$weight, y=Auto$mpg, color = Auto$acceleration)) + geom_point() +   
  geom_smooth(method=lm) +
  labs(title="Wykres zużycia paliwa w zależności od wagi samochodu",
       x="Zużycie paliwa (mpg)", y = "Waga")

# Zadanie 3
library(rpart)
library(mlbench)
library(rpart.plot)
data("BreastCancer") 
CNS <- BreastCancer %>%
  na.omit(CNS)

set.seed(109072)
sample_size = floor(0.8*nrow(CNS))

picked = sample(seq_len(nrow(CNS)),size = sample_size)
train = CNS[picked,]
test = CNS[-picked,]

d.klas1 <- rpart(Class~., data= train %>% select(c(Cell.shape, Mitoses, Class)), method='class')
d.klas2 <- rpart(Class~., data= train %>% select(-c(Id)), method='class')


rpart.plot(d.klas1, under=FALSE, fallen.leaves = FALSE)
rpart.plot(d.klas2, under=FALSE, fallen.leaves = FALSE)

CM <- list()
CM[["d.klas1"]] <- table(predict(d.klas1, new = test, type = "class"), test$Class)
CM[["d.klas2"]] <- table(predict(d.klas2, new = test, type = "class"), test$Class)


EvaluateModel <- function(classif_mx)
{
  true_positive <- classif_mx[1,1]
  true_negative <- classif_mx[2,2]
  condition_positive <- sum(classif_mx[ ,1])
  condition_negative <- sum(classif_mx[ ,2])
  predicted_positive <- sum(classif_mx[1, ])
  predicted_negative <- sum(classif_mx[2, ])
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  sensitivity <- true_positive / condition_positive
  NPV <- true_negative / (false_negative + true_negative)
  return(list(accuracy = accuracy,
              sensitivity = sensitivity
  ))
}

EvaluateModel(CM[["d.klas1"]])
EvaluateModel(CM[["d.klas2"]])


# Zadanie 4
library(mlbench)
install.packages('cvms')
library(cvms)
VCL <- Vehicle
VCL$Class <- ifelse(VCL$Class == 'bus' | VCL$Class == 'van', 'big', 'low')
VCL$Class

set.seed(109072)
sample_size2 = floor(0.6 * nrow(VCL))
picked = sample(seq_len(nrow(VCL)),size = sample_size2)
train = VCL[picked,]
test = VCL[-picked,]
train

d.klas3 <- rpart(Class~., data= train, method='class', minbucket = 50)
rpart.plot(d.klas3, under=FALSE, fallen.leaves = FALSE)

# Jeżeli Distance Circularity jest większy lub równy 89 to prawdopodobieństwo, że będzie to mały samochód wynosi 86%

Саня, [30/01/2023 20:57]
# Zadanie 1
przyklad = c(2, 1, 3, 1, 8, 1, 1, 0)

vectorMean = function(vec) {
  len = length(vec)
  newVector = c()
  
  if (len < 2) return ("Wektor liczy za mało elementów, aby zwrócić wynik działania")
  for (i in 1:len-1) {
    avg = (vec[i] + vec[i+1])/2
    newVector[i] = avg
  }
  
  return (newVector)
}

answ = vectorMean(przyklad)
# Zadanie 1 skumulowane
prz = c(1, 2, 3, 4, 5)

scummulowanyVector = function(vec) {
  len = length(vec)
  newVector = c()
  prevSum = 0
  
  for (i in 1:len) {
    value = vec[i] + prevSum
    prevSum = value
    newVector[i] = value
  }
  return (newVector)
}
answSkum = scummulowanyVector(prz)
# Zadanie 1 twins
primeNumber = function(num) {
  if (num == 2) {
    return (TRUE)
  } else if (any(num %% 2:(num-1) == 0)) {
    return (FALSE)
  } else { 
    return (TRUE)
  }
}

twins = function(a, b) {
  if (abs(a - b) == 2) {
    
    if (primeNumber(a) & primeNumber(b)) {
      return (TRUE)
    } else {
      return (FALSE)
    }
    
  } else {
    return (FALSE)
  }
}
twins(31,28)
twins(31,29)

# Zadanie 2
library(AER)
library(dplyr)
library(ggplot2)
data("HousePrices")
# punkt 1
HousePrices %>%
  group_by(aircon, stories) %>%
  summarise(mean_price = mean(price),
            sd_price = sd(price),
            max_bedrooms = max(bedrooms))

# punkt 2
compFun = function(a) {
  if (a <= median(HousePrices$lotsize)) {
    a = 0
  } else {
    a = 1
  }
}
dzialka_2 = sapply(HousePrices$lotsize, compFun)

# punkt 3 
# zamiast color można użyć fill 
ggplot(HousePrices, aes(x = HousePrices$prefer, y = HousePrices$price, color = HousePrices$aircon)) +
  geom_boxplot() +
  xlab("Preferowana lokalizacja?") +
  ylab("Cena") +
  scale_color_discrete(name = "Klimatyzacja", labels = c("Nie", "Tak")) +
  scale_x_discrete(labels = c("Nie", "Tak"))

# Zadanie 3
library(AER)
data("DoctorVisits")
# 1
DoctorVisits$visits <- as.factor(ifelse(DoctorVisits$visits > 0, 1, 0))
# 2 
set.seed(109163)
sample <- sample(c(TRUE, FALSE), nrow(DoctorVisits), replace=TRUE, prob=c(0.8,0.2))
train  <- DoctorVisits[sample, ]
test   <- DoctorVisits[!sample, ]


# 3
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa

tree <- rpart(visits ~ ., # zależna - y; żeby użyć wszystkich zmiennych objaśniających można napisać "y ~ .,"
              data=train, # data.train - zbiór treningowy
              cp = 0.003) # 
library(randomForest)
train$visits <- as.character(train$visits)
train$visits <- as.factor(train$visits)
rf <- randomForest(visits ~., data = train, ntree = 500, mtry = 4)

Саня, [30/01/2023 20:57]
CM <- list()
CM[["tree"]] <- table(predict(tree, new = test, type = "class"), test$visits)
CM[["rf"]] <- table(predict(rf, new = test, type = "class"), test$visits)
ocena <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}
lapply(CM, ocena)
sapply(CM, ocena)
# 4
library(caret) # 
library(ROCR)
pred_score <- predict(tree, newdata = test, type = "prob")[,2]
pred_object <- prediction(pred_score, test$visits)
performance_object <- performance(pred_object, "tpr", "fpr")
plot(performance_object, colorize = TRUE)
auc <- performance(pred_object, "auc")@y.values
auc
# Zadanie 4
# 1
data("HousePrices")
set.seed(109163)
sample <- sample(c(TRUE, FALSE), nrow(HousePrices), replace=TRUE, prob=c(0.7,0.3))
train  <- HousePrices[sample, ]
test   <- HousePrices[!sample, ]
#2
lm = lm(price ~ ., data = train)
tree = rpart(price ~ lotsize + bathrooms + garage + prefer + bedrooms, data = train, minbucket = 50)
rpart.plot(tree, under = FALSE, tweak = 1.2, fallen.leaves = TRUE)
mae_lm <- mean(abs(test$price - predict(lm, newdata = test)))
rsq_lm <- summary(lm)$r.squared
mae_dt <- mean(abs(test$price - predict(tree, newdata = test)))
rsq_dt <- 1 - mean((test$price - predict(tree, newdata = test))^2)/var(test$price)
mae_lm
rsq_lm
mae_dt
rsq_dt