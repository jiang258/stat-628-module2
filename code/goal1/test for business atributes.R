library(ggplot2)
##read processed business attribute
dat = read.csv("attribute.csv")

##Kruskal-Wallis test and

#RestaurantsTableService
c0 = dat$stars[dat$RestaurantsTableService == 0]   #False
c1 = dat$stars[dat$RestaurantsTableService == 1]   #None
c2 = dat$stars[dat$RestaurantsTableService == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(465,2644,1163))))
kruskal.test(x~g,data = data)    #p-value < 2.2e-16, p < 0.05
wilcox.test(c0,c1,paired = F)
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x=barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
        ylab = 'average star',main = 'RestaurantsTableService',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#OutdoorSeating
c0 = dat$stars[dat$OutdoorSeating == 0]   #False
c1 = dat$stars[dat$OutdoorSeating == 1]   #None
c2 = dat$stars[dat$OutdoorSeating == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(1880,506,1886))))
kruskal.test(x~g,data = data)    #p-value < 2.2e-16, p < 0.05
wilcox.test(c0,c1,paired = F)
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)   #p-value = 0.5574 >0.05
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
        ylab = 'average star',main = 'OutdoorSeating',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#BusinessParking
c0 = dat$stars[dat$BusinessParking == 0]   #False
c1 = dat$stars[dat$BusinessParking == 1]   #None
c2 = dat$stars[dat$BusinessParking == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(904,646,2722))))
kruskal.test(x~g,data = data)    #p-value = 3.42e-09, p < 0.05
wilcox.test(c0,c1,paired = F)    #p-value = 0.2477 > 0.05
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)  
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
            ylab = 'average star',main = 'BusinessParking',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#NoiseLevel
c0 = dat$stars[dat$NoiseLevel == 0]   #None
c1 = dat$stars[dat$NoiseLevel == 1]   #average
c2 = dat$stars[dat$NoiseLevel == 2]   #loud
c3 = dat$stars[dat$NoiseLevel == 3]   #quiet
c4 = dat$stars[dat$NoiseLevel == 4]   #very_loud
data = data.frame(x = c(c0,c1,c2,c3,c4),g = factor(rep(1:5,c(965,2463,193,595,56))))
kruskal.test(x~g,data = data)    #p-value = 1.066e-09, p < 0.05
wilcox.test(c0,c1,paired = F)    #p-value = 0.0009962
wilcox.test(c0,c2,paired = F)
wilcox.test(c0,c3,paired = F)    #p-value = 0.8371
wilcox.test(c0,c4,paired = F)
wilcox.test(c1,c2,paired = F)
wilcox.test(c1,c3,paired = F)
wilcox.test(c1,c4,paired = F)
wilcox.test(c2,c3,paired = F)
wilcox.test(c2,c4,paired = F)    #p-value = 0.3148
wilcox.test(c3,c4,paired = F)
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2),round(mean(c3),2),round(mean(c4),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2),mean(c3),mean(c4)),names.arg = c('None','average','loud','quiet','very_loud'),xlab = 'class',
            ylab = 'average star',main = 'NoiseLevel',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2),mean(c3),mean(c4)),labels = m,cex=1.5,pos=1)

#Caters
c0 = dat$stars[dat$Caters == 0]   #False
c1 = dat$stars[dat$Caters == 1]   #None
c2 = dat$stars[dat$Caters == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(1461,1267,1544))))
kruskal.test(x~g,data = data)    #p-value = 2.101e-15, p < 0.05
wilcox.test(c0,c1,paired = F)    
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)  
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
            ylab = 'average star',main = 'Caters',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#RestaurantsGoodForGroups
c0 = dat$stars[dat$RestaurantsGoodForGroups == 0]   #False
c1 = dat$stars[dat$RestaurantsGoodForGroups == 1]   #None
c2 = dat$stars[dat$RestaurantsGoodForGroups == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(439,409,3424))))
kruskal.test(x~g,data = data)    #p-value, p < 0.05
wilcox.test(c0,c1,paired = F)    
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)  
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
            ylab = 'average star',main = 'RestaurantsGoodForGroups',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#trendy
c0 = dat$stars[dat$trendy == 0]   #False
c1 = dat$stars[dat$trendy == 1]   #None
c2 = dat$stars[dat$trendy == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(3291,693,288))))
kruskal.test(x~g,data = data)    #p-value, p < 0.05
wilcox.test(c0,c1,paired = F)    #p-value = 0.05764 > 0.05
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)  
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
            ylab = 'average star',main = 'trendy',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#RestaurantsPriceRange2
c0 = dat$stars[dat$RestaurantsPriceRange2 == 0]   #1
c1 = dat$stars[dat$RestaurantsPriceRange2 == 1]   #2
c2 = dat$stars[dat$RestaurantsPriceRange2 == 2]   #3
c3 = dat$stars[dat$RestaurantsPriceRange2 == 3]   #4
c4 = dat$stars[dat$RestaurantsPriceRange2 == 4]   #None
data = data.frame(x = c(c0,c1,c2,c3,c4),g = factor(rep(1:5,c(1682,2139,149,11,291))))
kruskal.test(x~g,data = data)    #p-value = 7.129e-07, p < 0.05
wilcox.test(c0,c1,paired = F)    #p-value = 0.184
wilcox.test(c0,c2,paired = F)
wilcox.test(c0,c3,paired = F)    #p-value = 0.6117
wilcox.test(c0,c4,paired = F)
wilcox.test(c1,c2,paired = F)
wilcox.test(c1,c3,paired = F)    #p-value = 0.6786
wilcox.test(c1,c4,paired = F)
wilcox.test(c2,c3,paired = F)    #p-value = 0.6611
wilcox.test(c2,c4,paired = F)    #p-value = 0.2041
wilcox.test(c3,c4,paired = F)    #p-value = 0.4564
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2),round(mean(c3),2),round(mean(c4),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2),mean(c3),mean(c4)),names.arg = c('1','2','3','4','None'),xlab = 'class',
            ylab = 'average star',main = 'RestaurantsPriceRange2',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2),mean(c3),mean(c4)),labels = m,cex=1.5,pos=1)

#RestaurantsDelivery
c0 = dat$stars[dat$RestaurantsDelivery == 0]   #False
c1 = dat$stars[dat$RestaurantsDelivery == 1]   #None
c2 = dat$stars[dat$RestaurantsDelivery == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(3306,434,532))))
kruskal.test(x~g,data = data)    #p-value, p < 0.05
wilcox.test(c0,c1,paired = F)  
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)    #p-value = 0.5261
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
            ylab = 'average star',main = 'RestaurantsDelivery',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#WiFi
c0 = dat$stars[dat$WiFi == 0]   #None
c1 = dat$stars[dat$WiFi == 1]   #free
c2 = dat$stars[dat$WiFi == 2]   #no
c3 = dat$stars[dat$WiFi == 3]   #paid
data = data.frame(x = c(c0,c1,c2,c3),g = factor(rep(1:4,c(682,2376,1191,23))))
kruskal.test(x~g,data = data)    #p-value, p < 0.05
wilcox.test(c0,c1,paired = F)  
wilcox.test(c0,c2,paired = F)    #p-value = 0.1527 > 0.05
wilcox.test(c0,c3,paired = F)    #p-value = 0.2725 > 0,05
wilcox.test(c1,c2,paired = F) 
wilcox.test(c1,c3,paired = F)
wilcox.test(c2,c3,paired = F)    #p-value = 0.1129 > 0.05
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2),round(mean(c3),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2),mean(c3)),names.arg = c('None','free','no','paid'),xlab = 'class',
            ylab = 'average star',main = 'WiFi',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0),mean(c1),mean(c2),mean(c3)),labels = m,cex=1.5,pos=1)

# BusinessAcceptsCreditCards
c0 = dat$stars[dat$BusinessAcceptsCreditCards == 0]   #False
c1 = dat$stars[dat$BusinessAcceptsCreditCards == 1]   #None
c2 = dat$stars[dat$BusinessAcceptsCreditCards == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(70,1576,2626))))
kruskal.test(x~g,data = data)    #p-value < 2.2e-16, p < 0.05
wilcox.test(c0,c1,paired = F)
wilcox.test(c0,c2,paired = F)
wilcox.test(c1,c2,paired = F)  #p-value = 0.4354
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x=barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
          ylab = 'average star',main = 'BusinessAcceptsCreditCards',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#HasTV
c0 = dat$stars[dat$HasTV == 0]   #False
c1 = dat$stars[dat$HasTV == 1]   #None
c2 = dat$stars[dat$HasTV == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(2003,656,1613))))
kruskal.test(x~g,data = data)    #p-value = 0.2687
wilcox.test(c0,c1,paired = F)    #p-value = 0.08621
wilcox.test(c0,c2,paired = F)    #p-value = 0.6831
wilcox.test(c1,c2,paired = F)    #p-value = 0.2614
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x=barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
          ylab = 'average star',main = 'HasTV',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#RestaurantsReservations
c0 = dat$stars[dat$RestaurantsReservations == 0]   #False
c1 = dat$stars[dat$RestaurantsReservations == 1]   #None
c2 = dat$stars[dat$RestaurantsReservations == 2]   #True
data = data.frame(x = c(c0,c1,c2),g = factor(rep(1:3,c(2612,408,1252))))
kruskal.test(x~g,data = data)    #p-value < 0.05
wilcox.test(c0,c1,paired = F)    #p-value < 0.05
wilcox.test(c0,c2,paired = F)    #p-value < 0.05
wilcox.test(c1,c2,paired = F)    #p-value = 0.09157
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2)))
x=barplot(c(mean(c0),mean(c1),mean(c2)),names.arg = c('False','None','True'),xlab = 'class',
          ylab = 'average star',main = 'RestaurantsReservations',ylim = c(0,5))
text(x,c(mean(c0),mean(c1),mean(c2)),labels = m,cex=1.5,pos=1)

#Alcohol
c0 = dat$stars[dat$Alcohol == 0]   #None
c1 = dat$stars[dat$Alcohol == 1]   #free
c2 = dat$stars[dat$Alcohol == 2]   #no
c3 = dat$stars[dat$Alcohol == 3]   #paid
data = data.frame(x = c(c0,c1,c2,c3),g = factor(rep(1:4,c(808,1943,351,1170))))
kruskal.test(x~g,data = data)    #p-value, p < 0.05
wilcox.test(c0,c1,paired = F)    #p-value = 0.1146 > 0.05
wilcox.test(c0,c2,paired = F)    
wilcox.test(c0,c3,paired = F)    #p-value = 0.6373 > 0,05
wilcox.test(c1,c2,paired = F) 
wilcox.test(c1,c3,paired = F)
wilcox.test(c2,c3,paired = F)    #p-value = 0.1129 > 0.05
(m = c(round(mean(c0),2),round(mean(c1),2),round(mean(c2),2),round(mean(c3),2)))
x = barplot(c(mean(c0),mean(c1),mean(c2),mean(c3)),names.arg = c('None','free','no','paid'),xlab = 'class',
            ylab = 'average star',main = 'Alcohol',ylim = c(0,5),col = 'light blue')
text(x,c(mean(c0),mean(c1),mean(c2),mean(c3)),labels = m,cex=1.5,pos=1)
