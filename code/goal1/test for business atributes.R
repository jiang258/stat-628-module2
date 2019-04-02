## 1.read processed business attribute data
dat = read.csv("attribute.csv")

## 2.Do nonparametric test to test if 10 important business attributes are statitically
## related to stars. Do Kruscal-Wallis test to test if there exists one sample stochastically 
## dominate one ohter, then do pairwise Wilcoxon rank sum test with Bonferroni correction.

# 2.1 RestaurantsTableService
c0 = dat$stars[dat$RestaurantsTableService == 0]   #False
c2 = dat$stars[dat$RestaurantsTableService == 2]   #True
data = data.frame(x = c(c0,c2),g = factor(rep(1:2,c(465,1163))))
wilcox.test(c0,c2,paired = F)    #p-value = 6.306e-11 < 0.05, reject null hypothesis
(m = c(round(mean(c0),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c2)),names.arg = c('False','True'),xlab = 'class',
        ylab = 'average star',main = 'RestaurantsTableService',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c2)+0.7),labels = m,cex=1.5,pos=1)
# Restaurants with no table service have higher star.

# 2.2 OutdoorSeating
c0 = dat$stars[dat$OutdoorSeating == 0]   #False
c2 = dat$stars[dat$OutdoorSeating == 2]   #True
data = data.frame(x = c(c0,c2),g = factor(rep(1:2,c(1880,1886))))
wilcox.test(c0,c2,paired = F)    #p-value < 2.2e-16 < 0.05, reject null hypothesis
(m = c(round(mean(c0),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c2)),names.arg = c('False','True'),xlab = 'class',
        ylab = 'average star',main = 'OutdoorSeating',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c2)+0.7),labels = m,cex=1.5,pos=1)
# Restaurants with outdoor seating have higher star.


# 2.3 BusinessParking
c0 = dat$stars[dat$BusinessParking == 0]   #False
c2 = dat$stars[dat$BusinessParking == 2]   #True
data = data.frame(x = c(c0,c2),g = factor(rep(1:2,c(904,2722))))
wilcox.test(c0,c2,paired = F)      #p-value = 1.344e-09 < 0.05, reject null hypothesis
(m = c(round(mean(c0),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c2)),names.arg = c('False','True'),xlab = 'class',
            ylab = 'average star',main = 'BusinessParking',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c2)+0.7),labels = m,cex=1.5,pos=1)
# Restaurants with business parking place have higher star.


# 2.4 NoiseLevel
c1 = dat$stars[dat$NoiseLevel == 1]   #average
c2 = dat$stars[dat$NoiseLevel == 2]   #loud
c3 = dat$stars[dat$NoiseLevel == 3]   #quiet
c4 = dat$stars[dat$NoiseLevel == 4]   #very_loud
data = data.frame(x = c(c1,c2,c3,c4),g = factor(rep(1:4,c(2463,193,595,56))))
kruskal.test(x~g,data = data)    #p-value = 6.303e-09, p < 0.05
# entire alpha = 0.05, use Bonferroni correction for each test alpha = 0.05/6 = 0.00833333
wilcox.test(c1,c2,paired = F)    #p-value = 0.0001156 < 0.00833333, reject null hypothesis
wilcox.test(c1,c3,paired = F)    #p-value = 0.0002615 < 0.00833333, reject null hypothesis
wilcox.test(c1,c4,paired = F)    #p-value = 0.001555 < 0.00833333, reject null hypothesis
wilcox.test(c2,c3,paired = F)    #p-value = 3.588e-07 < 0.00833333, reject null hypothesis
wilcox.test(c2,c4,paired = F)    #p-value = 0.3148 > 0.00833333, retain null hypothesis
wilcox.test(c3,c4,paired = F)    #p-value = 0.0001016 < 0.00833333, reject null hypothesis
(m = c(round(mean(c1),2),round(mean(c2),2),round(mean(c3),2),round(mean(c4),2)))
x = barplot(c(mean(c1),mean(c2),mean(c3),mean(c4)),names.arg = c('average','loud','quiet','very_loud'),xlab = 'class',
            ylab = 'average star',main = 'NoiseLevel',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c1)+0.7,mean(c2)+0.7,mean(c3)+0.7,mean(c4)+0.7),labels = m,cex=1.5,pos=1)
 # Restaurants with loud and very loud have same performance on stars.
# Quiet restaurants have higher star.


# 2.5 Caters
c0 = dat$stars[dat$Caters == 0]   #False
c2 = dat$stars[dat$Caters == 2]   #True
data = data.frame(x = c(c0,c2),g = factor(rep(1:2,c(1461,1544))))
wilcox.test(c0,c2,paired = F)   #p-value < 2.2e-16 < 0.05, reject null hypothesis
(m = c(round(mean(c0),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c2)),names.arg = c('False','True'),xlab = 'class',
            ylab = 'average star',main = 'Caters',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c2)+0.7),labels = m,cex=1.5,pos=1)
# Restaurants with caters have higher star.


# 2.6 RestaurantsGoodForGroups
c0 = dat$stars[dat$RestaurantsGoodForGroups == 0]   #False
c2 = dat$stars[dat$RestaurantsGoodForGroups == 2]   #True
data = data.frame(x = c(c0,c2),g = factor(rep(1:2,c(439,3424))))
wilcox.test(c0,c2,paired = F)     #p-value = 1.145e-05 < 0.05, reject null hypothesis
(m = c(round(mean(c0),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c2)),names.arg = c('False','True'),xlab = 'class',
            ylab = 'average star',main = 'RestaurantsGoodForGroups',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c2)+0.7),labels = m,cex=1.5,pos=1)
# Restaurants that are not good for groups have higher star.


# 2.7 trendy
c0 = dat$stars[dat$trendy == 0]   #False
c2 = dat$stars[dat$trendy == 2]   #True
data = data.frame(x = c(c0,c2),g = factor(rep(1:2,c(3291,288))))
wilcox.test(c0,c2,paired = F)     #p-value < 2.2e-16 < 0.05, reject null hypothesis
(m = c(round(mean(c0),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c2)),names.arg = c('False','True'),xlab = 'class',
            ylab = 'average star',main = 'trendy',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c2)+0.7),labels = m,cex=1.5,pos=1)
# Trendy restaruants have higher star.


# 2.8 RestaurantsPriceRange2
c0 = dat$stars[dat$RestaurantsPriceRange2 == 0]   #1
c1 = dat$stars[dat$RestaurantsPriceRange2 == 1]   #2
c2 = dat$stars[dat$RestaurantsPriceRange2 == 2]   #3
c3 = dat$stars[dat$RestaurantsPriceRange2 == 3]   #4
data = data.frame(x = c(c0,c1,c2,c3),g = factor(rep(1:4,c(1682,2139,149,11))))
kruskal.test(x~g,data = data)    #p-value = 0.01863, p < 0.05
# entire alpha = 0.05, use Bonferroni correction for each test alpha = 0.05/6 = 0.00833333
wilcox.test(c0,c1,paired = F)    #p-value = 0.184 > 0.00833333 ,retain null hypothesis
wilcox.test(c0,c2,paired = F)    #p-value = 0.003681 < 0.00833333, reject null hypothesis
wilcox.test(c0,c3,paired = F)    #p-value = 0.6117 > 0.00833333, retain null hypothesis
wilcox.test(c1,c2,paired = F)    #p-value = 0.00668 < 0.00833333, reject null hypothesis
wilcox.test(c1,c3,paired = F)    #p-value = 0.6786 > 0.00833333, retain null hypothesis
wilcox.test(c2,c3,paired = F)    #p-value = 0.6611 > 0.00833333, retain null hypothesis
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2),round(mean(c3),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2),mean(c3)),names.arg = c('1','2','3','4'),xlab = 'class',
            ylab = 'average star',main = 'RestaurantsPriceRange2',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c1)+0.7,mean(c2)+0.7,mean(c3)+0.7),labels = m,cex=1.5,pos=1)
# Intuitively, Restautants with high price provide better environment, food and service.


# 2.9 RestaurantsDelivery
c0 = dat$stars[dat$RestaurantsDelivery == 0]   #False
c2 = dat$stars[dat$RestaurantsDelivery == 2]   #True
data = data.frame(x = c(c0,c2),g = factor(rep(1:2,c(3306,532))))
wilcox.test(c0,c2,paired = F)     #p-value = 2.3e-09 < 0.05, reject null hypothesis
(m = c(round(mean(c0),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c2)),names.arg = c('False','True'),xlab = 'class',
            ylab = 'average star',main = 'RestaurantsDelivery',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0)+0.7,mean(c2)+0.7),labels = m,cex=1.5,pos=1)
# Restaurants with delivery have higher star.


# 2.10 WiFi
c1 = dat$stars[dat$WiFi == 1]   #free
c2 = dat$stars[dat$WiFi == 2]   #no
c3 = dat$stars[dat$WiFi == 3]   #paid
data = data.frame(x = c(c1,c2,c3),g = factor(rep(1:3,c(2376,1191,23))))
kruskal.test(x~g,data = data)    #p-value = 1.419e-06, p < 0.05
# entire alpha = 0.05, use Bonferroni correction for each test alpha = 0.05/3 = 0.01666667
wilcox.test(c1,c2,paired = F)    #p-value = 1.74e-06 < 0.01666667, reject null hypothesis
wilcox.test(c1,c3,paired = F)    #p-value = 0.02476 > 0.01666667, retain null hypothesis
wilcox.test(c2,c3,paired = F)    #p-value = 0.1129 > 0.0166666， retain null hypothesis
(m = c(round(mean(c1),2),round(mean(c2),2),round(mean(c3),2)))
x = barplot(c(mean(c1),mean(c2),mean(c3)),names.arg = c('free','no','paid'),xlab = 'class',
            ylab = 'average star',main = 'WiFi',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c1)+0.7,mean(c2)+0.7,mean(c3)+0.7),labels = m,cex=1.5,pos=1)
# Restaurants with no wifi can provide free wifi to increase star.
