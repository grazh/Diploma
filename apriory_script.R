for(i in c('arules', 'DT', 'plotly', 'arulesViz'
           , 'visNetwork', 'igraph', 'kableExtra', 'knitr'
           , 'stringr', 'mlbench', 'ggplot2', 'nnet', 'readxl')) {
  install.packages(i, dependencies=TRUE)
}

library(arules)
library(DT)
library(fastDummies)
library(plotly)
library(DataCombine)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)
library(knitr)
library(stringr)
library(mlbench)
library(ggplot2)
library(nnet)
library(readxl)

###############
###############
###############
# Categories  #
###############
###############
###############

diploma_data <- as.matrix(read.csv("/Users/d.d.semenov/Desktop/diploma_data/MBA Categories/cat_translated_matrix_encoded.csv", sep=';'))
diploma_data <- diploma_data[,-c(1)]
# diploma_data <- diploma_data[,-c(1, 2, 3)]
View(diploma_data[1:1000,])

matrix_market <- as.data.frame(diploma_data)
matrix_market


Data1 <- as(diploma_data, "itemMatrix")

#сколько раз каждый продукт встречается? 
r <- itemFrequency(Data1)
sort(r, decreasing = TRUE)

?itemFrequency
#пусть правило имеет вид X -> Y
#support, поддержка правила = сколько раз встречается все множество: P(X & Y)
#confidence, доверие правила = сколько все правило встречается
#при наличии левой части: P(X & Y|X)
#coverage, покрытие - поддержка левой части правила, т.е. сколько раз левая часть встречается в данных
#lift, лифт - во сколько раз повышается вероятность встретить правую часть, если
#в множестве есть левая часть: P(Y|X)/P(Y). Это основной показатель качества правила. 

#визуализация частот
itemFrequencyPlot(Data1)

#можем оставить только то, что встречается чаще 0.5
itemFrequencyPlot(Data1, support = 0.5)


#два наиболее часто встречающихся варианта
itemFrequencyPlot(Data1, topN = 5)

#визуализация разреженной матрицы
image(Data1[1:100,])

#построение модели
rules <- apriori(Data1, parameter = list(support = 0.1,
                                        confidence = 0.3, minlen = 2, maxlen=2))

rules <- sort(rules, by=c("lift"))

plot(rules, method="graph", 
     control=list(nodeCol="yellow", edgeCol="blue"), engine = "htmlwidget")

inspect(rules)


Mod_veget <- subset(rules, rhs %in% "produkty_pitaniya_molochnye_produkty_syry_yaica" & lhs %in% c('produkty_pitaniya_konditerskie_izdeliya'
                                                                                                    , 'produkty_pitaniya_konservirovannye_produkty'
                                                                                                    , 'produkty_pitaniya_ovoschi_frukty'
                                                                                                    , 'produkty_pitaniya_makarony_krupy_bakaleya'
                                                                                                    , 'produkty_pitaniya_napitki_voda_soki'
                                                                                                    , 'produkty_pitaniya_hleb_i_hlebobulochnye_izdeliya'
                                                                                                    , 'produkty_pitaniya_myaso_ptica'
                                                                                                    , 'drugoe_drugoe'
                                                                                                    , 'produkty_pitaniya_drugoe')) 
Mod_veget
?subset
test <- subset(rules, lift > 1.5) 
test

plot(rules, method="graph", 
     control=list(nodeCol="yellow", edgeCol="blue"), engine = "htmlwidget")

plot(Mod_veget, method="graph", 
     control=list(nodeCol="yellow", edgeCol="blue"), engine = "htmlwidget")

#векторная визуализация
plot(Mod_veget, method = "paracoord")

#матрица правил
plot(Mod_veget, method = "grouped matrix")

####################################
## Анализ потребительской корзины ##
####################################



##########
##########
##########
# Brands #
##########
##########
##########


diploma_data_1 <- as.matrix(read.csv("/Users/d.d.semenov/Desktop/diploma_data/MBA Brands/brand_translated_matrix_encoded.csv", sep=';'))
diploma_data_1 <- diploma_data_1[,-c(1)]
View(diploma_data_1[1:1000,])

Data1_1 <- as(diploma_data_1, "itemMatrix")

#сколько раз каждый продукт встречается? 
itemFrequency(Data1_1)

#пусть правило имеет вид X -> Y
#support, поддержка правила = сколько раз встречается все множество: P(X & Y)
#confidence, доверие правила = сколько все правило встречается
#при наличии левой части: P(X & Y|X)
#coverage, покрытие - поддержка левой части правила, т.е. сколько раз левая часть встречается в данных
#lift, лифт - во сколько раз повышается вероятность встретить правую часть, если
#в множестве есть левая часть: P(Y|X)/P(Y). Это основной показатель качества правила. 

rules <- apriori(Data1_1, parameter = list(support = 0.001,
                                         confidence = 0.1, minlen = 2)) 
rules <- sort(rules, by=c("lift"))


#какие правила удалось выявить?
inspect(rules)

plot(rules, method="graph", 
     control=list(nodeCol="yellow", edgeCol="blue"), engine = "htmlwidget")


Mod_veget <- subset(rules, lift > 2 & rhs %in% "prostokvashino") 
Mod_veget

plot(Mod_veget, method="graph", 
     control=list(nodeCol="yellow", edgeCol="blue"), engine = "htmlwidget")


####################################
#что можно сказать о качестве правил?
plot(Mod_veget, method="graph", 
     control=list(nodeCol="yellow", edgeCol="blue"), engine = "igraph")

#интерактивная визуализация
plot(Mod_veget, method="graph", 
     control=list(nodeCol="yellow", edgeCol="blue"), engine = "htmlwidget")

#векторная визуализация
plot(rules, method = "paracoord")

#матрица правил
plot(rules, method = "grouped matrix")

####################################
## Анализ потребительской корзины ##
####################################

#визуализируем разреженную матрицу
image(Groceries[1:100, ])

#наиболее популярные товары
itemFrequencyPlot(Groceries, topN = 10)

#все товары, которые встречаются чаще 8% случаев
itemFrequencyPlot(Groceries, support = 0.08, col = rainbow(5))

#построим модель
mod_groc <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.3, minlen = 2))
mod_groc

#есть ли сильные правила?
plot(mod_groc, method = "two-key plot")

#посмотрим на правила
inspect(mod_groc)

#отсортируем по lift
inspect(sort(mod_groc, by = "lift"))

#выберем те, у кого lift больше 1 и визуализируем
mod_10 <- subset(mod_groc, lift > 2.5)
mod_10

#построим граф связей
plot(mod_10, method = "graph", control = list(nodeCol = "yellow", edgeCol = "blue"), engine = "igraph")

#сферичная форма
plot(mod_10, method = "graph", control = list(nodeCol = "yellow", edgeCol = "blue",
                                              layout=igraph::in_circle()), engine = "igraph")

#покрасим в зависимости от lift
plot(mod_10, method = "graph", control = list(edgeCol = "blue",
                                              layout=igraph::in_circle()), engine = "igraph")

#добавим интерактивность
plot(mod_10, method = "graph", control = list(nodeCol = "yellow", edgeCol = "blue"), 
     engine = "htmlwidget")


#параллельные координаты
plot(mod_10, method = "paracoord", legend = TRUE)
#яркость надежность
#толщина поддержка
inspect(mod_10)


#правиле в матричном виде
plot(mod_10, method = "grouped matrix", measure = "support")
plot(mod_10, method = "grouped matrix", measure = "lift")

#все правила содержащие ягоды
Mod_berries <- subset(mod_groc, items %in% "berries") 
inspect(Mod_berries)

#все правила, содержащие молоко слева
Mod_milk_left <- subset(mod_groc, lhs %in% "whole milk") 
inspect(Mod_milk_left)

#все правила, содержащие молоко справа
Mod_milk_right <- subset(mod_groc, rhs %in% "whole milk") 
inspect(Mod_milk_right)

#все правила, содержащие молоко справа c lift больше 2
Mod_milk_right_lift <- subset(mod_groc, rhs %in% "whole milk" & lift > 2) 
inspect(Mod_milk_right_lift)

#все правила, содержащие овощи
Mod_veget <- subset(mod_groc, items %in% "vegetables") 

#таких нет. Но некоторые правила содержат слово "vegetables". Найдем их
Mod_veget <- subset(mod_groc, items %pin% "vegetables") 
Mod_veget

#правила, который точно совпадают с искомым множеством
Mod_berries_yogurt <- subset(mod_groc, items %ain% c("berries", "yogurt")) 
inspect(Mod_berries_yogurt)

