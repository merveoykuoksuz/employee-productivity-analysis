veri <- read.table("~/Desktop/veri.txt", header = TRUE)
veri$x4 <- factor(veri$x4)
names(veri)
names(veri)<-c("y","x1","x2","x3","x4")
attach(veri)
summary(veri)
#normallik testi
shapiro.test(y)
library(nortest)
ad.test(y)
lillie.test(y)
str(veri)
#Q-Q plot 
qqnorm(y)
qqline(y)
#doğrusallık
pairs(veri)
#ln dönüşümü
lny<-log(y)
#Q-Q plot
qqnorm(lny)
qqline(lny)
#normallik testi
shapiro.test(lny)
#yeni verinin oluşturulması
yeniveri<-cbind(lny,x1,x2,x3,x4)
#doğrusallık
pairs(yeniveri)
#regresyon analizi
sonveri<-lm(lny~x1+x2+x3+x4)
summary(sonveri)
#aykırı değerler
boxplot(y)
influence.measures(sonveri)
inf<-ls.diag(sonveri)
inf
#grafik çizimi ile ilgili 
par(mfrow=c(2,2))
plot(sonveri)
#aykırı değer incelemsi
n<-length(lny)
k<-4
cooksd<-cooks.distance(sonveri)
plot(cooksd,pch="*",cex=2,main="Influential Obs by Cook's Distance")
abline(h=if(n>50)4/n else 4/(n-k-1),col="red")
text(x=1:length(cooksd)+1,y=cooksd,labels = ifelse(cooksd>if (n>50) 4/n else 4/(n-k-1),names(cooksd),""),col="red")

hat<-hatvalues(sonveri)
plot(hat,pch="*", cex=2, main="Leverage Value by Hat Value")
abline(h=2*(k+1)/n, col="red")
text(x=1:length(hat)+1,y=hat,labels = ifelse(hat>2*(k+1)/n,1:length(hat),""),col="red")

stud<-rstudent(sonveri)
plot(stud, pch="*",cex=2, main="Outlier by Studntized residuals",ylab="Studentized Residuals",xlab="Index")
abline(h=c(-3,3),col="red")
text(x=1:length(stud)+1,y=stud, labels=ifelse(stud<-3 | stud > 3,1:length(stud),""),col="red")

std<-rstandard(sonveri)
plot(std,pch="*",cex=2,main="Outlier by Standrdized residuals",ylab="Standrdized Residuals",xlab="Index")
abline(h=c(-2,2),col="red")
text(x=1:length(std)+1,y=std,labels=ifelse(std<-2 | std>2,1:length(std),""),col="red")


# 1. Tespit ettiğin tüm indeksleri bir araya getiriyoruz (Tekrar edenleri 'unique' ile siliyoruz)
tespit_edilenler <- c(7,70,59,71,8,12,18,24,53)
aykiri_indeks <- unique(tespit_edilenler)

# 2. Veriden Çıkarma
veri_temiz <- veri[-aykiri_indeks, ]

# 3. Bağımlı değişkeni temiz veriyle tekrar oluştur
# Önemli: lny'yi veri_temiz içindeki y değerlerinden almalısın
lny_yeni <- log(veri_temiz$y)

# 4. Yeni Modeli Kur
sonveri_yeni <- lm(lny_yeni ~ x1 + x2 + x3 + x4, data = veri_temiz)
summary(sonveri_yeni)
# A) Normallik Testi
shapiro.test(residuals(sonveri_yeni))
ad.test(residuals(sonveri_yeni))
# p-değeri > 0.05 ise hatalar artık normal dağılıyor demektir.
# Tekrar Aykırı Değer Kontrolü (Grafiksel)
par(mfrow=c(2,2))
plot(sonveri_yeni)

#Tekrar Aykırı Değer Kontrolü 
std_yeni <- rstandard(sonveri_yeni)
yeni_aykiri_indeks <- which(abs(std_yeni) > 2)
print(yeni_aykiri_indeks) 
#güven aralığı
confint(sonveri_yeni, level=.99)

#  Değişen Varyanslılık (9. Soru)
library(lmtest)
bptest(sonveri_yeni)
summary(lm(abs(residuals(sonveri_yeni)) ~ fitted(sonveri_yeni)))
# Yorum: p > 0.05 ise "Değişen varyans sorunu yoktur" yazılır.

plot(sonveri_yeni, which = 1)  # Tam olarak Residuals vs Fitted plotu
#  Öz İlişki sorunu (10. Soru)
dwtest(sonveri_yeni)
#VIF değerlerini kontrol et (10 dan büyük var mı)
#  Çoklu Bağlantı (11. Soru)
install.packages("olsrr")
library(olsrr)
ols_vif_tol(sonveri_yeni)
install.packages("corrplot")
library(corrplot)
corrplot(cor(veri_temiz[, c("x1", "x2", "x3")]))
pairs(veri_temiz[, c("x1", "x2", "x3")])
# Yorum: VIF değerleri 10'dan küçükse çoklu bağlantı yoktur.
# 12. Verideki 15. gözlem için uyum kestirimi
predict(sonveri_yeni, veri_temiz[15,], interval = "confidence", level = 0.95)
# 15. satırdaki tüm değişkenleri getirir
veri_temiz[15, ]
# 13. Veride olmayan yeni bir değer için ön kestirim
yeni_satir <- data.frame(x1=8.5, x2=2.1, x3=3.5, x4=factor(2))
predict(sonveri_yeni, yeni_satir, interval = "prediction", level = 0.95)

# Tüm değişkenler içinden en anlamlılarını seçer
adimli_model <- step(sonveri_yeni, direction = "both")
summary(adimli_model)
#İleriye doğru seçim (forward selection)
library(stats)
lm.null<-lm(lny~1)
forward<-step(lm.null, scope = ~ x1 + x2 + x3 + x4, direction="forward")
forward
summary(forward)

#Geriye doğru seçim(Backward elimination)
backward<-step(sonveri_yeni,direction = "backward")
summary(backward)

#Adımsal seçim yöntemi(steowise elimination)
step.model<-stepAIC(sonveri_yeni,direction="both",trace=FALSE)
summary(step.model)

#ridge
ridge <- lm.ridge(lny ~ x1 + x2 + x3 + x4, lambda=seq(0,1,0.05))
matplot(ridge$lambda,t(ridge$coef),type = "l",xlab=expression(lambda),ylab = expression(hat(beta)))
abline(h=0,lwd=2)
ridge$coef
select(ridge)
ridge$coef[ridge$lambda==0.4]