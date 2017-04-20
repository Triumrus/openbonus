library("data.table")
getwd()
setwd("C:/Users/user/Desktop/опен бонус")
pokupki<- fread("донателло покупки за год.csv",integer64 = 'character',stringsAsFactors = T)
klienti<- fread("донателло клиенты за год.csv")


str(pokupki)
pokupki$`Дата покупки`<- as.Date(pokupki$`Дата покупки`,"%d.%m.%Y %H:%M")

pokupki$month <- month(pokupki$`Дата покупки`)

t<- seq(as.Date("01.02.2017","%d.%m.%Y"),as.Date("01.05.2016","%d.%m.%Y"), by = "-1 month")
t<- data.table(t)
t$nomer <- 1:nrow(t)
t<- t[order(t,decreasing = F)]
t<- data.table(t)
# t$t <- as.Date(t,"%Y-%m-%d")
# t$t<- as.numeric(t$t)
pokupki$pust <- 0
pokupki<- pokupki[`Дата покупки`>"2016-05-01"]

pokupki[which(pokupki$`Дата покупки` > t[1,1])]$pust <- t[1,2]
pokupki[which(pokupki$`Дата покупки` > t[2,1])]$pust <- t[2,2]
pokupki[which(pokupki$`Дата покупки` > t[3,1])]$pust <- t[3,2]
pokupki[which(pokupki$`Дата покупки` > t[4,1])]$pust <- t[4,2]
pokupki[which(pokupki$`Дата покупки` > t[5,1])]$pust <- t[5,2]
pokupki[which(pokupki$`Дата покупки` > t[6,1])]$pust <- t[6,2]
pokupki[which(pokupki$`Дата покупки` > t[7,1])]$pust <- t[7,2]
pokupki[which(pokupki$`Дата покупки` > t[8,1])]$pust <- t[8,2]
pokupki[which(pokupki$`Дата покупки` > t[9,1])]$pust <- t[9,2]
pokupki[which(pokupki$`Дата покупки` > t[10,1])]$pust <- t[10,2]
# pokupki[which(pokupki$`Дата покупки` > t[11,1])]$pust <- t[11,2]



levels(as.factor(pokupki$`Группа покупателя`))
levels(as.factor(pokupki$`Точка продаж`))
pokupki$`Официанты` <- NULL
pokupki$`Кассир` <- NULL
pokupki$`Уникальная покупка` <- NULL
pokupki$`Сумма оплаты` <-  as.numeric(pokupki$`Сумма оплаты`)
pokupki$`Начислено` <-  as.numeric(pokupki$`Начислено`)
pokupki$`Скидка` <-  as.numeric(pokupki$`Скидка`)

uniq<- pokupki[month==2,unique(`Телефон`)]
shivie<- pokupki[month==3,unique(`Телефон`)]
length(uniq)
length(shivie)
length(which(uniq%in%shivie))

# a<- shivie[-which(shivie%in%uniq)]


pokupki<- pokupki[
  `Дата покупки`>="2016-05-01" & `Дата покупки`<"2017-03-01" & `Телефон` %in% uniq
  ,.(
    sum_pokup=sum(`Сумма покупки`,na.rm = T),
    sum_oplay=sum(`Сумма оплаты`,na.rm = T),
    sum_nach=sum(`Начислено`,na.rm = T),
    sum_spisan=sum(`Списано`,na.rm = T)
    
    
  )
  ,by=.(`pust`,`Телефон`,`Группа покупателя`,`Точка продаж`)]


uniq<- as.data.table(uniq)
uniq$label <- 0
uniq[-which(uniq%in%shivie),]$label <- 1


library("reshape2")
names(pokupki)
str(pokupki)
pokupki$pust<- as.factor(pokupki$pust)
pokupki<- as.data.frame(pokupki)
names(pokupki) <- c("pust","telefon","group","tochka","sum_pokup"      ,   "sum_oplay" ,        "sum_nach"     ,     "sum_spisan" )
data <- melt(pokupki,id=c("telefon","pust","group","tochka"))
data <- data.table(data)
data <- data[order(pust,group,tochka,value)]


data <- dcast(data,telefon~variable+pust+group+tochka)
data <- apply(data, 2,function(x) replace(x, is.na(x) | is.nan(x) | is.infinite(x), -1))
data <- as.data.table(data)


data$pred <- predict(clf2,newdata = data.matrix(data))
setkey(data,telefon)
key(data)
setkey(uniq,uniq)
key(uniq)

data<- merge(x=data,y=uniq,by.x = "telefon",by.y = "uniq",all.x = T)
ROC(test=data$pred,stat= data$label,plot = "ROC")

ttt<- data[,.(pred,label)][order(pred,decreasing = T)]
plot(ttt$pred)
ttt$predlab<- ifelse(ttt$pred>=succes,1,0)
(table(ttt$predlab,ttt$label)[1,1]+table(ttt$predlab,ttt$label)[2,2])/
sum(table(ttt$predlab,ttt$label))
