## Rastgele veri oluşturmak için ilgili paketi yükleyelim
install.packages("MASS")
library(MASS)

## Bir şirkette çalışan bireylerin demografik ve ekonomik özelliklerini veri seti yaptık.
## Bu değişkenlerin arasındaki ilişki ve dağılımlarını gözlemleyeceğiz.
## Veri setini oluşturalım;
set.seed(123)
n <- 200 # Gözlem sayısı
yas <- rnorm(n,mean = 35,sd=8)
gelir <- rnorm(n,mean = 3000,sd=800)
performans <- rnorm(n,mean = 70,sd=10)
harcama <- 0.6*gelir+3*performans-2*yas+rnorm(n,mean=0,sd=300) ## Bağımlı değişken tanımladık

## Oluşturulan veri setini veri çerçevesine dönüştürme
data <- data.frame(harcama,yas,gelir,performans)

## Değişkenlerimizi görüntüleyelim
str(data) ## hepsi numerik problem yok
## Veri setinin ilk 6 satırını görüntüleyelim
head(data) ## Yaş değişkenimiz küsüratlı çıktığı için geri dönüp round ekledik düzeltelim
yas <- round(yas)
data <- data.frame(harcama,yas,gelir,performans)
## Tekrar bakalım
head(data)


## Eksik veri var mı bakalım
is_na_yas <- is.na(yas)
is_na_yas ## Eksik veri yok
is_na_gelir <- is.na(gelir)
is_na_gelir ## Eksik veri yok
is_na_performans <- is.na(performans)
is_na_performans ## Eksik veri yok
is_na_harcama <- is.na(harcama)
is_na_harcama ## Eksik veri yok


## Aykırı değer tespiti
## Kutu grafiği
boxplot(data) ## harcama,yas ve performan değişkenlerinde birer aykırı değer görülmüştür.
## z puanı hesaplama
z_scores <- scale(data)
z_scores
## Aykırı değer belirleme
outliers <- abs(z_scores)>3
outliers ## Hepsi False aykırı değer görülmedi
## Alt ve üst çeyrekleri hesaplama
Q1_harcama <- quantile(data$harcama,0.25)
Q1_yas <- quantile(data$yas,0.25)
Q1_gelir <- quantile(data$gelir,0.25)
Q1_performans <- quantile(data$performans,0.25)
Q3_harcama <- quantile(data$harcama,0.75)
Q3_yas <- quantile(data$yas,0.75)
Q3_gelir <- quantile(data$gelir,0.75)
Q3_performans <- quantile(data$performans,0.75)
## Alt ve üst sınırları hesaplanma
IQR_harcama <- Q3_harcama-Q1_harcama
IQR_yas <- Q3_yas-Q1_yas
IQR_gelir <- Q3_gelir-Q1_gelir
IQR_performans <- Q3_performans-Q1_performans
lower_bound_harcama <- Q1_harcama-1.5*IQR_harcama
lower_bound_yas <- Q1_yas-1.5*IQR_yas
lower_bound_gelir <- Q1_gelir-1.5*IQR_gelir
lower_bound_performans <- Q1_performans-1.5*IQR_performans
upper_bound_harcama <- Q3_harcama+1.5*IQR_harcama
upper_bound_yas <- Q3_yas+1.5*IQR_yas
upper_bound_gelir <- Q3_gelir+1.5*IQR_gelir
upper_bound_performans <- Q3_performans+1.5*IQR_performans
sum(data$harcama<lower_bound_harcama | data$harcama>upper_bound_harcama)
sum(data$yas<lower_bound_yas | data$yas>upper_bound_yas)
sum(data$gelir<lower_bound_gelir | data$gelir>upper_bound_gelir)
sum(data$performans<lower_bound_performans | data$performans>upper_bound_performans)
## Harcama ve yaş değişkenlerinde 1’er adet, performans değişkeninde 3 adet aykırı değer tespit edilmiştir.
## Aykırı değer sayısı kritik görünmemektedir ve güvenilirliği bozmamaktadır.



## Dağılımları keşif
## Histogram oluşturma
hist(data)






## Dağılımları keşif
## Kutu grafiği oluşturma
boxplot(data) ## harcama,yas ve performan değişkenlerinde birer aykırı değer görülmüştür.
## Kantillerden yararlanma
summary(data) ## Tüm değişkenlerde ortalama ve medyan değerlerinin birbirine oldukça yakın olduğu görülmektedir.
## Korelasyon katsayısını hesaplama
correlation <- cor(data)
print(correlation) 
## Harcama ile gelir arasında güçlü pozitif bir ilişki vardır. Gelir arttıkça harcamanın arttığı söylenebilir.
## Yaş ve performans ile harcama arasında zayıf veya anlamsız ilişki vardır. En belirgin ilişkinin gelir ve harcama arasında olduğu söylenebilir.




## Multicollinearity
## Çoklu doğrusal regresyon modeli oluşturma
model <- lm(harcama~yas+gelir+performans)
summary(model)
options(scipen = 999)
summary(model)
## Modelin istatistiksel olarak anlamlı olduğu (p<0.001) görülmüştür. Harcama değişkendeki varyansın yaklaşık %73’ü açıklanabilmektedir. 
## Gelir değişkeninin harcama üzerinde güçlü ve anlamlı bir etkisi olduğu görülmüştür (p<0.001). Yaşın etkisi zayıf ama istatistiksel olarak anlamlı bulunmuştur (p<0.05). 
## Performansın ise harcama üzerinde anlamlı bir etkisi olmadığı görülmüştür (p>0.05).


## Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(yas,gelir,performans))
print(correlation_matrix)
## Yaş, gelir ve performans değişkenleri arasında güçlü bir ilişki bulunmaktadır.

install.packages("corrplot2")
library(corrplot)
corrplot(correlation_matrix,method = "color")
## Değişkenler arasında güçlü bir ilişki olmadığı görülmüştür.


## Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) ## Grafikleri yanyana yerleştirdik
plot(yas,harcama,main="yas vs harcama",xlab="yas",ylab="harcama",col="blue",pch=16)
plot(gelir,harcama,main="gelir vs harcama",xlab="gelir",ylab="harcama",col="red",pch=16)
plot(performans,harcama,main="performans vs harcama",xlab="performans",ylab="harcama",col="green",pch=16)
## Yaş ile harcama arasında belirgin bir doğrusal ilişki görülmüştür.
## Gelir arttıkça harcama da artmaktadır.
## Performans ile harcama arasında anlamlı bir ilişki görülmemiştir.



## Veriyi standartlaştırma
yas_stand <- scale(yas)
gelir_stand <- scale(gelir)
performans_stand <- scale(performans)
harcama_stand <- scale(harcama)
## Standartlaştırılmış veriyi kontrol etme
summary(yas_stand)
summary(gelir_stand)
summary(performans_stand)
summary(harcama_stand)
## Veri setinin normal dağılıma uygun olduğu ve aşırı uç değerlerinin bulunmadığını görülmüştür.
