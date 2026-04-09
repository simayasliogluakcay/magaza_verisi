

## Bir giyim mağazanın günlük satış performansını inceleyelim;


magaza_verisi <- data.frame(
gun = c("Pzt","Sali","Cars","Pers","Cuma","Cts","Pazar","Pzt","Sali","Cars","Pers","Cuma","Cts","Pazar"),
kategori = c("Ust","Alt","Dis","Ust","Alt","Dis","Ust","Alt","Dis","Ust","Alt","Dis","Ust","Alt"),
musteri_sayisi = c(45, 38, 30, 52, 47, 60, 58,40, 28, 55, 50, 35, 62, 48),
satis_adedi = c(18, 14, 9, 20, 16, 12, 22,15, 8, 21, 17, 10, 24, 16),
birim_fiyat = c(250, 400, 900, 260, 420, 950, 240,410, 920, 255, NA, 970, 245, 415)
)




## Veri ön inceleme;

View(magaza_verisi)
## Başlık ve sınıf kontrolü için ilk 6 satırı çağıralım;
head(magaza_verisi)
## Veri yapılarına bakalım;
str(magaza_verisi)
## İstatistiksel özetine bakalım, NA(Eksik Veri) varsa tespit edelim;
summary(magaza_verisi)
## gun ve kategori değişkenlerimiz kategorik, diğerleri sayısal değişkenlerdir.
## birim_fiyat değişkenimizde bir kayıp veri görülmüştür.



## Eksik verileri temizleyelim;
magaza_verisi <- na.omit(magaza_verisi)
## Kontol edelim;
summary(magaza_verisi)





## Analizimiz için gerekli değişkeni oluşturalım;
magaza_verisi$ciro <- magaza_verisi$satis_adedi*magaza_verisi$birim_fiyat
## Kontrol edelim;
head(magaza_verisi)



## Sayısal değişkenlerimiz arasındaki ilişkiyi gözlemleyelim;
cor_magazaverisi <- cor(magaza_verisi[,c("musteri_sayisi","satis_adedi","birim_fiyat","ciro")])
cor_magazaverisi
## satis_adedi ve musteri_sayisi değişkenlerinin ciro ile ters korelasyon gösterdi.
## birim_fiyat ile ciro değişkenlerimizin pozitif ve güçlü bir ilişki gösterdiğini gösterdi.
## Bu sonuçlar bize satış gelirinin en çok ürün fiyatlarına bağlı olduğunu göstermiştir.



## Aykırı değer var mı bakalım;
## Kutu grafiği;
boxplot(magaza_verisi)
## Korelasyon grafiğine bakalım;
install.packages("corrplot")
library(corrplot)
corrplot(cor_magazaverisi,method="color")
## ciro değişkeniyle musteri_sayisi arasında zayıf, satis_adedi arasında negatif ilişki görüldü.
## birim_fiyat ile ciro arasında yine çok güçlü bir ilişki görüldü.


## Görselleştirelim;
install.packages("ggplot2")
library(ggplot2)
ggplot(magaza_verisi, aes(x = musteri_sayisi, y = ciro)) +
  geom_point()+
  facet_wrap(~ kategori)
ggplot(magaza_verisi, aes(x = kategori, y = birim_fiyat)) +
  geom_point()
## Müşteri sayısı ile ciro arasındaki ilişki kategorik olarak farklılık göstermiştir.
## Dış giyim hem ciro hem müşteri sayısı olarak yüksek görülmüştür.


## Aykırı değer kontrolü yapalım;
boxplot(magaza_verisi$ciro)
## Tek aykırı değer görülmüştür.
boxplot(magaza_verisi$birim_fiyat)
## Aykırı değer görülmemiştir.

