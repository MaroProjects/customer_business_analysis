rm(list=ls())
getwd()
setwd("C:/Moje Projekty w R/Statystyka w biznesie")

my.data <- read.csv("Projekt_dane.txt", sep="\t", 
                    header = TRUE, dec = ",", 
                    stringsAsFactors=FALSE, 
                    colClasses=c("Client_ID"="character"),
                    encoding="UTF-8")


#######################################################################################################
#                   W przypadku wczytywania wszystkiego naraz wyskakują błędy
#                   sugeruje wczytywać dane sekcjami, dzięki temu żadne zmienne nie powinny uciec
#################################################Przygotowanie do analizy###################################
my.data <- dplyr::filter(my.data, Client_ID!="UNKNOWN" & !is.na(Client_ID) & Client_ID!="") 
my.data <- dplyr::filter(my.data, Product_family!="UNKNOWN" & !is.na(Product_family) & 
                           Product_family!="-" & Product_family!="998" & Product_family!="999") 
my.data <- dplyr::filter(my.data, SalesValue>10) #usuwamy zakupy o małych wartościach (tutaj można zmodyfikować próg odcięcia)


library(dplyr)
library(reshape2)

my.data$Product_family2 <- substr(my.data$Product_family, 1, 1) #"wycinamy" tylko pierwszy znak z nazwy

my.data$Product_family2 <- ifelse(my.data$Product_family2 %in% c("G", "H", "I", "J", "K", "L", "M", "O",
                                                                 "S", "T", "U", "V"), "Other", my.data$Product_family2)
#######################################################################################################
#                                 Analiza pareto dla produktów
#######################################################################################################


require(qcc)

pogrupowane <- aggregate(my.data$SalesValue, 
                   by=list(Product=my.data$Product_family2), 
                   sum)
pogrupowane$tysiac <- pogrupowane$x/1000

pareto.chart(pogrupowane$tysiac)
# istnieje problem ze zmienną G, dlatego stworzę tą grafikę ręcznie
sum(paretorecznie)


#z jakiegos powodu trzeba jeszcze raz wczytac te dane recznie
paretorecznie <- c(2387.8394, 5041.9376, 1750.5969, 1043.9459, 271.0988, 980.0060, 2271.1345)
names(paretorecznie) <- c("A", "B", "C", "D", "E", "F", "Other")



pareto.chart(paretorecznie,  col=heat.colors(length(paretorecznie)), main = "Pareto Chart for Products", ylab = "Sales value [thousands]")

#######################################################################################################
#                                 Analiza Pareto dla klientów
#######################################################################################################

product.data <- aggregate(my.data$SalesValue, by=list(Client = my.data$Client_ID), sum)

product.data<- filter(product.data, product.data$x>1000)



product.data <- product.data[order(product.data$x, decreasing=TRUE),] #sortujemy dane wg wartości sprzedaży
product.data$no <- 1:nrow(product.data)
product.data$Sales.thous <- product.data$x/1000

product.data$Cum.sales.thous <- cumsum(product.data$Sales.thous)

total.sales <- sum(product.data$Sales.thous) #łączną wartość sprzedaży zapiszemy jako osoby skalar (wektor)

product.data$Cum.sales.rel <- (product.data$Cum.sales.thous/total.sales)*100


ggplot(product.data, aes(no, Sales.thous)) + 
  geom_col(col = "orange") +
  geom_line(aes(no, Cum.sales.rel/0.55), col="red3", size=1)+
  scale_y_continuous("Sales value [ thous.]", 
                     sec.axis = sec_axis(~.*0.55, name = "Cumulative Percentage [%]"), limits = c(0,200)) + 
  theme(axis.title.y.right = element_text(color = "orange3"), axis.title.y.left = element_text(color = "red3")) + 
  labs(title = "Pareto Charts for clients (over 1000$ spent on shopping)", x = "Number of client")

#######################################################################################################
#                                 Analiza Pareto Dla wartości sprzedaży
#######################################################################################################



df <- data.frame(numbers = my.data$SalesValue/1000)

# Group numbers into bins
df <- df %>%
  mutate(group = cut(numbers, breaks=c(0,1,2,3,4,6,8,50,150,300), labels=c("0-1", "1-2","2-3","3-4", "4-6","6-8","8-50","50-150","150-300"))) %>%
  group_by(group)
df$string <-  as.character(df$group)
my.data$string <- df$string

product.data2 <- aggregate(my.data$SalesValue/1000, by=list(Client = my.data$string), sum)


paretorecznie2 <- as.vector(product.data2$x)
names(paretorecznie2) <- c("0-1", "1-2","2-3","3-4", "4-6","6-8","8-50","50-150","150-300")


pareto.chart(paretorecznie2, col=heat.colors(length(paretorecznie2)), main = "Pareto chart for customer basket")


#######################################################################################################
#                                         Chmura miejsc
#######################################################################################################

library(wordcloud)

my.data$jedynki <- rep(1, length(my.data$SalesID))

miejsca <- aggregate(my.data$jedynki, by=list(miejscax = my.data$SalesDistributorProvince), sum)


par(mar = c(1,1,1,1))
wordcloud(miejsca$miejscax, miejsca$x, scale=c(3,.7), max.words = 15)

#######################################################################################################
#                                         Reguły zakupowe
#######################################################################################################

library("arules")
library("arulesViz")


my.data2 <- aggregate(my.data$SalesValue,
                     by = list(my.data$Client_ID, my.data$Product_family), sum)
colnames(my.data2) <- c("Client_ID", "Product_family", "SalesValue")


pivot <- dcast(my.data2, Client_ID ~ Product_family,sum, 
               value.var="SalesValue",margins=FALSE)

rownames(pivot) <- pivot[,"Client_ID"] 
pivot <- dplyr::select(pivot,-Client_ID) 

pivot.binary<-ifelse(pivot<20,0,1)

pivot.tr <- as(pivot.binary, "transactions")
summary(pivot.tr)
itemFrequency(pivot.tr, type="relative")
itemFrequency(pivot.tr, type="absolute")
sum(pivot.binary[,1]) #check

rules1 <- apriori(pivot.tr,  
          parameter = list(minlen=2,maxlen=2, supp=0.005, conf=0.4))
inspect(rules1)

plot(rules1, method="graph")
plot(rules1, method = "grouped")

#######################################################################################################
#                                         Segmentacja i dodatkowe wykresy
#######################################################################################################
library(tidyverse)
my.data3 <- aggregate(my.data$SalesValue,
                      by = list(my.data$Client_ID, my.data$SalesDate), sum)

daneRFM <- my.data3 %>% pivot_wider(names_from = Group.2 ,values_from = x, values_fill = list(x = NA) )
k <- ncol(daneRFM) #tak oznaczymy numer ostatniej kolumny z miesiącami

daneRFM$Frequency <- rowSums(!is.na(daneRFM[,2:k]))
summary(daneRFM$Frequency)
daneRFM <- dplyr::filter(daneRFM, Frequency!=0) #usuwamy klientów, którzy nic nie kupili
colnames(daneRFM) <- c("numer_ID", "d02", "d04","d05","d06","d07","d09","d10","d11","d12","d13","d14","d15","d16","d17","d18","d19","d20","d21","d23","d24","d25","d26","d27","d28","d30","d31","Frequency")

# tam 30 dni

daneRFM$last.purchase <- ifelse(!is.na(daneRFM$d31),1,NA)

for(i in 1:(k-2)){ #iterujemy od 1 do k-2=29
  daneRFM$last.purchase <- ifelse(!is.na(daneRFM[,k-i]) & is.na(daneRFM$last.purchase),i+1,daneRFM$last.purchase)
}

daneRFM$Total.value <- rowSums(daneRFM[,2:k], na.rm = TRUE)
daneRFM$Mean.value <- daneRFM$Total.value / daneRFM$Frequency

summary(daneRFM$Total.value)
summary(daneRFM$Mean.value)

hist(daneRFM$Total.value)
hist(log(daneRFM$Total.value))

ggplot(daneRFM) + geom_histogram(aes(x = log(daneRFM$Total.value)), color = "red3", fill = "red3") + labs(title = "Histogram dla zmiennej suma wartości zakupów z 30 dni", x="", y="liczba obserwacji") 
daneRFM$Monetary5 <- dplyr::ntile(daneRFM$Total.value, 5)
daneRFM$Recency5 <- dplyr::ntile(daneRFM$last.purchase, 5)
daneRFM$Frequency5 <- dplyr::ntile(daneRFM$Frequency, 5)

ggplot(daneRFM, aes(as.numeric(Frequency5), Recency5, colour=Monetary5)) +
  geom_point() + geom_jitter() + #bez jitter() punkty nakładałyby się na siebie
  scale_colour_gradient2(name="Monetary value") +
  labs(x="Frequency", y="Recency", title = "Segmentacja RFM") +  theme(plot.title = element_text(hjust = 0.5))


ggplot(my.data) + geom_histogram(aes(log(SalesQuantity)), bins = 40)

my.data$dtyg <- my.data$SalesDay %% 7

my.data$count <- 1

licznoscdni <- aggregate(my.data$count, by = list(my.data$dtyg), sum)

tabeladni <- data.frame(
  nrdzien = c("0", "1", "2", "3", "4", "5", "6"),
  nazwadzien = c("sobota","niedziela","poniedziałek","wtorek","środa","czwartek","piątek")
)

licznoscdni$dzientyg <- tabeladni$nazwadzien[match(licznoscdni$Group.1,tabeladni$nrdzien)]

colnames(licznoscdni) <- c("reszta", "liczebnosc", "dzientyg")

ggplot(licznoscdni, aes(x = dzientyg, y = liczebnosc, fill = dzientyg)) + geom_bar(stat = "identity", alpha = 1.2)  + scale_fill_brewer(palette = "Pastel1") + 
  labs(title="Wykres frekwencji zakupowanych", x = "Dzień tygodnia", y = "Liczebność") + 
  theme(plot.title = element_text(hjust = 0.5))


summary(my.data$SalesQuantity)
sd(my.data$SalesQuantity)

#######################################################################################################
#                                         Analiza skupień metodą Silhouette
#######################################################################################################
library(dplyr)
library(tidyr)
library(cluster)



my.data3 <- aggregate(my.data$SalesValue,
                      by = list(my.data$Client_ID, my.data$SalesDate), sum)

daneRFM <- my.data3 %>% pivot_wider(names_from = Group.2, values_from = x, values_fill = list(x = NA))
k <- ncol(daneRFM)

daneRFM$Frequency <- rowSums(!is.na(daneRFM[, 2:k]))
daneRFM <- dplyr::filter(daneRFM, Frequency != 0)

colnames(daneRFM) <- c("numer_ID", "d02", "d04", "d05", "d06", "d07", "d09", "d10", "d11", "d12", "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d23", "d24", "d25", "d26", "d27", "d28", "d30", "d31", "Frequency")

daneRFM$last.purchase <- ifelse(!is.na(daneRFM$d31), 1, NA)
for (i in 1:(k - 2)) {
  daneRFM$last.purchase <- ifelse(!is.na(daneRFM[, k - i]) & is.na(daneRFM$last.purchase), i + 1, daneRFM$last.purchase)
}

daneRFM$Total.value <- rowSums(daneRFM[, 2:k], na.rm = TRUE)
daneRFM$Mean.value <- daneRFM$Total.value / daneRFM$Frequency


k_clusters <- 5  
kmeans_result <- kmeans(daneRFM[, c("Frequency", "last.purchase", "Total.value")], centers = k_clusters)


daneRFM$cluster <- kmeans_result$cluster


silhouette_result <- silhouette(daneRFM$cluster, dist(daneRFM[, c("Frequency", "last.purchase", "Total.value")]))

mean_silhouette_score <- mean(silhouette_result[, 3])
print(paste("Średnia wartość Silhouette Score:", mean_silhouette_score))


d <-  fviz_silhouette(silhouette_result)






