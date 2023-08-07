cat("\f")
data=read.csv(file.choose())
View(data)

head(data)

#to view the number of rows
nrow(data)

#to view the number of columns
ncol(data)

#to display the column names
colnames(data)

#summary of the dataset
summary(data)

#summary of the particular price column
summary(data$Price)

#mean
meanprice=mean(data$Price)
print(meanprice)
#median
medianprice=median(data$Price)
print(medianprice)
#mode
getmode=function(priceMode){
  uniqd=unique(priceMode)
  uniqd[which.max(tabulate(match(priceMode, uniqd)))]
}
mp=data$Price
result1=getmode(mp)
print(result1)


#subsetting
battery=data$battery
price=data$Price
#check the missing value
is.na(battery)
is.na(price)
#building a linear regression model
mobilepm=lm(price~battery)
summary(mobilepm)
attributes(mobilepm)
coef(mobilepm)

#one sample t-test
t.test(data$Front_Cam)

#two sample t-test
t.test(data$Front_Cam,data$RearCam)
#visualization
plot(battery,price, col="green",main="Battery vs mobile price",abline(mobilepm),xlab = "battery",ylab = "mobile price")

#check model
x=data.frame(battery=3500)
resultmp=predict(mobilepm,x)
print(resultmp)

a=data$ram
y=lm(price~a)
print(y)
b=data.frame(a=3.000)
z=predict(y,b)
print(z)
m=data$battery
barplot(head(m,20),xlab="battery",ylab="mah",col="blue",  
        main="battery bar chart",border="red") 
v=data$ram
hist(v,xlab = "Weight",ylab="Frequency",col = "blue",border = "black",xlim = c(0,4), ylim = c(0,9), breaks = 10)

j=data$thickness
plot(j,ylab = "thickeness",main="polt for thickness",col="red")

