install.packages('stargazer')

library(readxl)
library(pscl)
library(lmtest)
library(ggplot2)
library(stats)
library(dplyr)
library(car)
library(corrplot)
library(car)
library(bucky)
library(stargazer)
library(caret)


diploma_data <- read_excel("/Users/d.d.semenov/Desktop/diploma_data/offers_data.xlsx")
dim(diploma_data)
diploma_data <- diploma_data[,-c(1)]
dim(diploma_data)
View(diploma_data)
colnames(diploma_data)
diploma_data['segment_size'] = diploma_data['segment_size']/1000000
summary(diploma_data)
attach(diploma_data)

#
# Изучим корреляции
#

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

diploma_data <- diploma_data[,c('datediff', 'total_cashback_p', 'number_of_merch', 'segment_size', 'target', 'count_offers', 'profit', 'total_roas')]
# matrix of the p-value of the correlation
p.mat <- cor.mtest(M)
head(p.mat[, 1:5])

colnames(diploma_data) <- c('Продолжительность акции', 'Ставка кешбэка', 'Количество торговых сетей участников'
                            , 'Размер сегмента', 'Выделение целевой аудитории', 'Количество акций, запущенных параллельно'
                            , 'Прибыль', 'ROAS')

M <- cor(diploma_data[, c('Продолжительность акции', 'Ставка кешбэка', 'Количество торговых сетей участников'
                          , 'Размер сегмента', 'Выделение целевой аудитории', 'Количество акций, запущенных параллельно'
                          , 'Прибыль', 'ROAS')])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 1, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
?corrplot

corrplot(cor(diploma_data[, c('datediff', 'total_cashback_p', 'number_of_merch', 'segment_size', 'target', 'count_offers', 'profit', 'total_roas')]))

library(ggplot2)
# Basic scatter plot

(ggplot(diploma_data, aes(x=total_cashback_p, y=total_roas, group=total_cashback_p))
  + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE)
  + scale_fill_grey()
  + theme_classic()
)

#
# Построим модель
#

model1 <- lm(log(profit)~datediff+I(total_cashback_p^2)+number_of_merch+segment_size+target+count_offers, data=diploma_data)
summary(model1)
robust.summary(model1)
vif(model1)
resettest(model1)
AIC(model1)

stargazer(model1, type = "text", out='text.html')

model123 <- lm(profit~datediff+I(total_cashback_p^2)+number_of_merch+segment_size+target+count_offers, data=diploma_data)
summary(model123)
robust.summary(model123)


plot(model1)

AIC(model1)

plot(diploma_data$count_offers,diploma_data$total_roas)



#
#1 Мультколлинеарность
#

#Есть ли мультиколлинеарность?
vif(model1)

#Мультиколлинеарности нет.

#
#2 Гетероскедастичность
#

#Построим диагностический график.
plot(model1, which = 3)

#Большая скученнрсть точек на графике. Явно есть гетероскедастичность.

#Тест Бреуша-Пагана:
bptest(model1)

#Гипотеза о том что в данных присутствует гетероскедастичность отвергается при уровне значимости 1%.


#
#3 Нормальность остатков
#

#Построим диагностический график квантиль-квантиль.
plot(model1, which = 2)

#Есть расхождение справа.

#Тест Бокса-Кокса.
boxCox(model1)

#Нужен логарифм.
#Графики для всех переменных.
crPlots(model1)


#
#4 Пропуск степеней
#

#Тест Рамсея:
resettest(model1)

#В модели не пропущены степени.

#
#5 Выбросы.
#

#Влиятельные наблюдения.
plot(model1, which = 4)

e <- resid(model1)
g <- abs(e/sd(e))

barplot(c(g))

#В целом выборка достаточно однородная. Удаление наблюдений не требуется.

#
#6 Значимость переменных
#

summary(model1)
robust.summary(model1)

#Все переменные значимы на 1% уровне даже без поправки на гетероскедастиность.

#
#7 Кросс-валидация
#


test_data <- diploma_data[diploma_data['profit'] >0,]
i = 0
res = c()
while (i < 100) {
  model_train <- train(log(profit)~datediff+I(total_cashback_p^2)+number_of_merch+segment_size+target+count_offers, data=test_data, trControl=trainControl(method="cv"), method="lm")
  res[i] = model_train$results$Rsquared
  i = i + 1
}
res
mean(res)
#Rsquared: 0.



model1 <- lm(total_roas~datediff+I(total_cashback_p^2)+total_cashback_p+number_of_merch+segment_size+target+count_offers, data=diploma_data)
summary(model1)
robust.summary(model1)
stargazer(model1, type = "text", out='text.html')

#
#1 Мультколлинеарность
#

#Есть ли мультиколлинеарность?
vif(model1)

#Мультиколлинеарности нет.

#
#2 Гетероскедастичность
#

#Построим диагностический график.
plot(model1, which = 3)

#Большая скученнрсть точек на графике. Явно есть гетероскедастичность.

#Тест Бреуша-Пагана:
bptest(model1)

#Гипотеза о том что в данных присутствует гетероскедастичность подтверждается при уровне значимости 1%.


#
#3 Нормальность остатков
#

#Построим диагностический график квантиль-квантиль.
plot(model1, which = 2)

#Есть расхождение справа.

#Тест Бокса-Кокса.
boxCox(model1)

#Нужен логарифм.
#Графики для всех переменных.
crPlots(model1)


#
#4 Пропуск степеней
#

#Тест Рамсея:
resettest(model1)

#В модели пропущены степени.

#При добавлении степеней до 3 ко всем регрессорам результаты тесты оставлвись неизменны.
#Тут уж ничего не поделать.

#
#5 Выбросы.
#

#Влиятельные наблюдения.
plot(model1, which = 4)

e <- resid(model1)
g <- abs(e/sd(e))

barplot(c(g))

#В целом выборка достаточно однородная. Удаление наблюдений не требуется.

#
#6 Значимость переменных
#

summary(model1)
robust.summary(model1)

#Все переменные значимы на 1% уровне даже без поправки на гетероскедастиность.

#
#7 Кросс-валидация
#

model_train <- train(profit~datediff+total_cashback_p+number_of_merch+I(segment_size^2), data=diploma_data, trControl=trainControl(method="cv"), method="lm")
model_train

#Rsquared: 0.32
