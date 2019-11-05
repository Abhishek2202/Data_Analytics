data2<-read.csv(file.choose(), header = TRUE)
data2<-data2[which(data2$LAND.SQUARE.FEET!="0" & data2$GROSS.SQUARE.FEET!="0" & data2$YEAR.BUILT!="0" & data2$SALE.PRICE!="$0"),]#attach(data2) # If you choose to attach, leave out the "data=." in lm regression
data2$SALE.PRICE<-sub("\\$","",data2$SALE.PRICE) 
data2$SALE.PRICE<-as.numeric(gsub(",","", data2$SALE.PRICE)) 
data2$GROSS.SQUARE.FEET<-as.numeric(gsub(",","", data2$GROSS.SQUARE.FEET)) 
data2$LAND.SQUARE.FEET<-as.numeric(gsub(",","", data2$LAND.SQUARE.FEET)) 

minprice<-1000
data2<-data2[which(data2$SALE.PRICE>=minprice),]

plot(data2$GROSS.SQUARE.FEET,data2$SALE.PRICE) 
plot(data2$YEAR.BUILT, data2$SALE.PRICE) 
hist(log(data2$GROSS.SQUARE.FEET))
hist(log(data2$LAND.SQUARE.FEET))
hist(log(data2$SALE.PRICE))
#Model1
m1 <- lm(data2$SALE.PRICE ~ data2$GROSS.SQUARE.FEET)
m1
plot(data2$GROSS.SQUARE.FEET, data2$SALE.PRICE)
abline(m1, col="red")
summary(m1)

# Model 2
m2<-lm(data2$SALE.PRICE~0+data2$GROSS.SQUARE.FEET+data2$LAND.SQUARE.FEET)
m2
plot(data2$GROSS.SQUARE.FEET, data2$SALE.PRICE)
abline(m2, col="red")
summary(m2)

#Model3
m3<-lm(data2$SALE.PRICE~0+data2$GROSS.SQUARE.FEET+data2$LAND.SQUARE.FEET+factor(data2$NEIGHBORHOOD))
abline(m3, col="red")
summary(m3)

#Model4
m4<-lm(data2$SALE.PRICE~0+data2$GROSS.SQUARE.FEET+data2$LAND.SQUARE.FEET+factor(data2$NEIGHBORHOOD)+ factor(data2$BUILDING.CLASS.CATEGORY))
abline(m4)
summary(m4) 

#2.a
N1<-data.frame(data2$GROSS.SQUARE.FEET, data2$SALE.PRICE)
pEH <- predict(m4,N1,interval="prediction")
cEH <- predict(m4,N1,interval="confidence")
pEH
cEH
plot(pEH,cEH)

#2.b
cor.test(data2$GROSS.SQUARE.FEET, data2$SALE.PRICE)
cor.test(data2$LAND.SQUARE.FEET, data2$SALE.PRICE)

