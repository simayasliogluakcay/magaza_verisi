# Veri çekme
data2 <- read.csv("index_1.csv")


## Değişkenlerimizi görüntüleyelim
str(data2) 
## Veri setinin ilk 6 satırını görüntüleyelim
head(data2)
## Betimsel istatistiklerine bakalım;
summary(data2)
## money harici sayısal değişkenim olmadığı için datetime'dan saati, date'den günü sayısal veriye çevirelim.
data2$hour <- as.numeric(substr(data2$datetime, 12, 13))
data2$day <- as.numeric(substr(data2$date, 9, 10))
## Kontrol edelim
head(data2[, c("datetime","hour")])
head(data2[, c("date","day")])



## Eksik veri kontrolü
colSums(is.na(data2))
## Eksik veri görülmedi



## Aykırı değer tespiti
## Kutu grafiği
boxplot(data2[, c("money", "hour", "day")])
## Aykırı değer görülmemiştir.

## z puanı hesaplama
z_scores2 <- scale(data2[, c("money", "hour", "day")])
z_scores2
## Aykırı değer belirleme
outliers2 <- abs(z_scores2)>3
outliers2
## Hepsi False olduğu için aykırı değer görülmemiştir.

## Alt ve üst çeyrekleri hesaplama
Q1_money <- quantile(data2$money, 0.25)
Q1_hour <- quantile(data2$hour, 0.25)
Q1_day <- quantile(data2$day, 0.25)

Q3_money <- quantile(data2$money, 0.75)
Q3_hour <- quantile(data2$hour, 0.75)
Q3_day <- quantile(data2$day, 0.75)

## IQR hesaplama
IQR_money <- Q3_money - Q1_money
IQR_hour <- Q3_hour - Q1_hour
IQR_day <- Q3_day - Q1_day

## Alt sınırlar
lower_bound_money <- Q1_money - 1.5 * IQR_money
lower_bound_hour <- Q1_hour - 1.5 * IQR_hour
lower_bound_day <- Q1_day - 1.5 * IQR_day

## Üst sınırlar
upper_bound_money <- Q3_money + 1.5 * IQR_money
upper_bound_hour <- Q3_hour + 1.5 * IQR_hour
upper_bound_day <- Q3_day + 1.5 * IQR_day

## Aykırı değer sayıları
sum(data2$money < lower_bound_money | data2$money > upper_bound_money)
sum(data2$hour < lower_bound_hour | data2$hour > upper_bound_hour)
sum(data2$day < lower_bound_day | data2$day > upper_bound_day)
## Hiçbir aykırı değer görülmemiştir.



## Dağılımları keşif
## Kutu grafiği oluşturma
boxplot(data2[, c("money", "hour", "day")])
## Aykırı değer görülmemiştir.

## Kantillerden yararlanma
summary(data2)
## Money değişkeninin ortalama ve medyan değerlerinin birbirine yakın olduğu görülmektedir.
## Bu durum dağılımın dengeli olduğunu göstermektedir.

## Korelasyon katsayısını hesaplama
correlation2 <- cor(data2[, c("money","hour","day")])
print(correlation2)
## Money ile hour ve day arasında güçlü bir ilişki vardır.
## Korelasyon katsayılarının düşük olması, değişkenler arasında doğrusal ilişki olmadığını göstermektedir.



## Sayısal değişkenleri seçme
numeric_data2 <- data2[, c("money", "hour", "day")]

## Sayısal değişkenler için özet istatistikler
summary(numeric_data2)


## Korelasyon analizi
correlation_matrix2 <- cor(numeric_data2)
print(correlation_matrix2)
## Money ile hour ve day arasında güçlü bir ilişki görülmemiştir.
## Hour ve day değişkenleri arasında da belirgin bir doğrusal ilişki de  bulunmamaktadır.

## Korelasyon grafiği
library(corrplot)
corrplot(correlation_matrix2, method = "color")
## Değişkenler arasında güçlü bir ilişki olmadığı görülmüştür.



## Para harcaması ile saat ve gün ilişkisini görselleştirme
par(mfrow = c(1,2))
plot(data2$hour, data2$money,main = "Hour vs Money",xlab = "Hour",ylab = "Money",col = "blue",pch = 16)
plot(data2$day, data2$money,main = "Day vs Money",xlab = "Day",ylab = "Money",col = "red",pch = 16)
## money ile hour ve day arasında belirgin bir doğrusal ilişki görülmemiştir.
## Para miktarının saat veya güne göre çok güçlü biçimde değişmediği söylenebilir.

## Kahve türüne göre para dağılımı
boxplot(money ~ coffee_name, data = data2,main = "Coffee Type'a Gore Money Dagilimi",xlab = "Coffee Name",ylab = "Money",las = 2)
## Farklı kahve türlerine göre ödeme tutarlarının değişip değişmediğine bakıldı

## Veriyi standartlaştırma
money_stand <- scale(data2$money)
hour_stand <- scale(data2$hour)
day_stand <- scale(data2$day)

## Standartlaştırılmış veriyi kontrol etme
summary(money_stand)
summary(hour_stand)
summary(day_stand)
## Standartlaştırma sonrasında değişkenlerin ortalamalarının sıfıra yaklaştığı görülmüştür.

