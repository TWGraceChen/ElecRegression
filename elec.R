# 匯入用電資料
# https://data.gov.tw/dataset/29935
ele <- read.csv("台灣電力公司_各縣市住宅、服務業及機關用電統計資料.csv")
ele <- data.frame(year = substr(ele$日期,1,4),
                  month = substr(ele$日期,6,7),
                  city = ele$縣市,
                  gen = ele$住宅部門售電量.度.)
ele <- ele[ele$city != '合計',]
ele$city <- as.character(ele$city)
ele$city <- gsub('台','臺',ele$city)


# 匯入戶數
# https://www.ris.gov.tw/app/portal/346
library(readxl)
house <- NULL
y <- unique(ele$year)
for (i in 1:length(y)) {
  m <- unique(ele$month[ele$year==y[i]])
  for (j in 1:length(m)) {
    tmp <- read_excel(paste("m0s8-",as.numeric(as.character(y[i])) - 1911,m[j],".xls",sep=""),
                      sheet = '總計',skip=4,col_names = FALSE,
                      col_types = c("text","numeric","numeric","numeric","numeric"))
    house <- rbind(house,cbind(tmp,y[i],m[j],tmp[4]/tmp[5] * 100))
  }
}

house <- house[c(1,2,3,8,6,7)]
colnames(house) <- c("city","house","people","gender_ratio","year","month")
house <- house[!is.na(house$city),]
house$city <- gsub(' ','',house$city)



# 匯入氣象資料
# https://www.cwb.gov.tw/V8/C/C/Statistics/monthlydata.html
y <- unique(ele$year)
weather <- NULL
for (i in 1:length(y)) {
  m <- unique(ele$month[ele$year==y[i]])
  for (j in 1:length(m)) {
    tmp <- read.csv(paste("weather",substr(y[i],3,4),m[j],".csv",sep=""),sep="\t",header=FALSE)
    weather <- rbind(weather,cbind(tmp,y[i],m[j]))
  }
}



colnames(weather) <- c("station","平均溫度","最高溫度","最低溫度","雨量","最大十分鐘風","最大瞬間風",
                       "平均濕度","最小濕度","氣壓","降水日數","日照時數","year","month")

splitslash <- function(content){
  l <- strsplit(as.character(content), "/", fixed = TRUE)
  t <- NULL
  for (i in 1:length(l)) {
    t <- c(t,l[[i]][1])
  }
  as.numeric(t)
}

weather$最高溫度 <- splitslash(weather$最高溫度)
weather$最低溫度 <- splitslash(weather$最低溫度)
weather$最大十分鐘風 <- splitslash(weather$最大十分鐘風)
weather$最大瞬間風 <- splitslash(weather$最大瞬間風)
weather$最小濕度 <- splitslash(weather$最小濕度)
weather$雨量 <- as.numeric(weather$雨量)
weather$雨量[is.na(weather$雨量)] <- 0
weather$氣壓 <- as.numeric(weather$氣壓)
weather$氣壓[is.na(weather$氣壓)] <- 0



#合併資料
data <- merge(x = ele, y = house, by = c("city","year","month"))
city <- unique(data$city)
for (i in 1:length(city)) {
  data$station[data$city==city[i]] <- switch(city[i],臺北市="臺北",臺中市="臺中",臺南市="臺南",臺東縣="臺東",
                                             嘉義縣="嘉義",嘉義市="嘉義",屏東縣="恆春",花蓮縣="花蓮",宜蘭縣="宜蘭",
                                             新竹縣="新竹",新竹市="新竹",高雄市="高雄" ,新北市="板橋",基隆市="基隆",
                                             南投縣="日月潭",桃園市="新屋",雲林縣="嘉義",彰化縣="臺中",苗栗縣="新竹",
                                             金門縣=NA,連江縣=NA,澎湖縣=NA)
}


data <- merge(x=data,y=weather,by = c("station","year","month"))
data <- data[!is.na(data$station),]
data <- data[,c(2:19)]
colnames(data) <- c("年份","月份","縣市","售電量","戶數","人口數","性別比",
                    "平均溫度","最高溫度","最低溫度","雨量","最大十分鐘風","最大瞬間風","平均濕度",
                    "最小濕度","氣壓","降水日數","日照時數")
#==================================================
#分出訓練和測試
train <- data[data$年份 != '2020',2:18]
test <- data[data$年份 == '2020',2:18]


# 敘述統計
summary(train[3:17])

# 每月總售電量
tapply(data$售電量,list(data$月份,data$年份),FUN=sum)
par(family='STKaiti')
plot(aggregate(data[data$年份 == '2019',4],list(data[data$年份 == '2019',2]),FUN=sum),type='l',col='red',lwd=2,
     xlab='月份',ylab='售電量',main='2016至2019年每月總售電量',
     ylim=c(min(aggregate(data$售電量,list(data$年份,data$月份),FUN=sum)$x),max(aggregate(data$售電量,list(data$年份,data$月份),FUN=sum)$x)))
lines(aggregate(data[data$年份 == '2018',4],list(data[data$年份 == '2018',2]),FUN=sum),type='l',col='orange',lwd=2)
lines(aggregate(data[data$年份 == '2017',4],list(data[data$年份 == '2017',2]),FUN=sum),type='l',col='darkgreen',lwd=2)
lines(aggregate(data[data$年份 == '2016',4],list(data[data$年份 == '2016',2]),FUN=sum),type='l',col='blue',lwd=2)
legend('topleft',c('2016','2017','2018','2019'),col=c('blue','darkgreen','orange','red'),lwd=2)


# heatmap
heatmap(tapply(train$售電量,list(train$縣市,train$月份),FUN=mean),
        Colv = NA, Rowv = NA, scale="row",main='各縣市每月份售電量分佈',
        xlab='月份',ylab='縣市')



#相關
cor(train[3:17])


# 所有連續變數散佈圖矩陣
par(family='STKaiti')
pairs(train[3:17], pch = 19,cex=0.2)
# 戶數散佈圖矩陣
pairs(train[3:7], pch = 19,cex=0.2)
# 氣候散佈圖矩陣
pairs(cbind(train[3],train[8:18]), pch = 19,cex=0.2)


#box-cox變數轉換https://blog.csdn.net/fitzgerald0/article/details/75212215
library(MASS)
b=boxcox(售電量~., data=train)
I=which(b$y==max(b$y))
b$x[I]

# 轉換後繪圖
pairs(cbind(train[3]^b$x[I],train[4:7]),pch=19,cex=0.2)
#train$售電量 <- train$售電量^b$x[I]

# 建立模型
null = lm(售電量 ~ 1, data = train)  
full = lm(售電量 ~ ., data = train)

lm1 = step(null,scope=list(lower=null, upper=full), direction="both")
summary(lm1)

# 取得各模型
summary(lm(售電量 ~ 1, data = train))
summary(lm(售電量 ~ 人口數, data = train))
summary(lm(售電量 ~ 人口數 + 月份, data = train))
summary(lm(售電量 ~ 人口數 + 月份 + 最高溫度, data = train))
summary(lm(售電量 ~ 人口數 + 月份 + 最高溫度 + 日照時數, data = train))
summary(lm(售電量 ~ 人口數 + 月份 + 最高溫度 + 日照時數 + 雨量, data = train))




# 共線性檢查
library(car)
vif(full)
cor(train[c(4,6,7)])


# 變數選擇
lm.train <- lm(售電量 ~ 人口數+月份, data = train)
summary(lm.train)

par(mfrow=c(2,2))
plot(lm.train)
dev.off()


#殘差檢定
## 殘差常態性檢定
shapiro.test(lm.train$residual)

## 殘差獨立性檢定
require(car)
durbinWatsonTest(lm.train)

## 殘差變異數同質性檢定
ncvTest(lm.train)

#用測試集驗證
R_squared <- function(actual, predict){
  mean_of_obs <- rep(mean(actual), length(actual))
  
  SS_tot <- sum((actual - mean_of_obs)^2)
  SS_reg <- sum((predict - mean_of_obs)^2)
  #SS_res <- sum((actual - predict)^2)
  R_squared <- SS_reg/SS_tot   #1 - (SS_res/SS_tot)
  R_squared
}
R_squared(test$售電量, predict(lm.train,test))


#==================================================
train_avg <- cbind(train[1:2],每戶售電量=train$售電量/train$戶數,train[7:17])
test_avg <- cbind(test[1:2],每戶售電量=test$售電量/test$戶數,test[7:17])

# 氣候散佈圖矩陣
pairs(train_avg[3:14], pch = 19,cex=0.2)


#box-cox變數轉換
b=boxcox(每戶售電量~., data=train_avg)
I=which(b$y==max(b$y))
b$x[I]

# 轉換後繪圖
pairs(cbind(trans_每月售電量=train_avg$每戶售電量^b$x[I],train_avg[4:14]),pch=19,cex=0.2)

# 轉換
#train_avg$每戶售電量 <- train_avg$每戶售電量^b$x[I]


# 建立模型
null_avg = lm(每戶售電量 ~ 1, data = train_avg)  
full_avg = lm(每戶售電量 ~ ., data = train_avg)

lm2 = step(null_avg,scope=list(lower=null_avg, upper=full_avg), direction="both")
summary(lm2)


# 取得各模型
summary(lm(每戶售電量 ~ 月份  , data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市  , data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度  , data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度 + 降水日數  , data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度 + 降水日數 + 平均濕度  , data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度 + 降水日數 + 平均濕度 + 最小濕度  , data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度 + 降水日數 + 平均濕度 + 最小濕度 + 最大瞬間風 , data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度 + 降水日數 + 平均濕度 + 最小濕度 + 最大瞬間風 + 最大十分鐘風, data = train_avg))
summary(lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度 + 降水日數 + 平均濕度 + 最小濕度 + 最大瞬間風 + 最大十分鐘風 + 雨量, data = train_avg))

# 變數選擇
lm.train_avg <- lm(每戶售電量 ~ 月份 + 縣市 + 最高溫度, data = train_avg)
summary(lm.train_avg)

par(mfrow=c(2,2))
plot(lm.train_avg)
dev.off()



#殘差檢定
## 殘差常態性檢定
shapiro.test(lm.train_avg$residual)

## 殘差獨立性檢定
require(car)
durbinWatsonTest(lm.train_avg)

## 殘差變異數同質性檢定
ncvTest(lm.train_avg)



#用測試集驗證
R_squared(test_avg$每戶售電量, predict(lm.train_avg,test_avg))



qqPlot(lm.train_avg, labels = FALSE, simulate = TRUE, main = "Q-Q Plot")


