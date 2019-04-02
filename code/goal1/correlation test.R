## cor.test(x,y,method=c("pearson","kendall","spearman")), p-value小于0.05则相关

bread=c(0.02457, 0.03506, 0.03696, 0.0396, 0.03193)
soup=c(0.01836, 0.02688, 0.02985, 0.03124, 0.024)
cor.test(bread,soup,method="pearson")
cor.test(bread,soup,method="kendall")
cor.test(bread,soup,method="spearman")

bread=c(0.02457, 0.03506, 0.03696, 0.0396, 0.03193)
scramble_egg=c(0.00557, 0.00708, 0.00701, 0.00722, 0.00523)
cor.test(bread,scramble_egg,method="pearson")  # p-value = 0.106
cor.test(bread,scramble_egg,method="kendall")  # p-value = 0.2333
cor.test(bread,scramble_egg,method="spearman")  # p-value = 0.1333

bacon=c(0.03468, 0.04667, 0.05561, 0.06337, 0.05079)
dessert=c(0.01947, 0.03348, 0.0485, 0.05582, 0.04688)
cor.test(bacon,dessert,method="pearson")
cor.test(bacon,dessert,method="kendall")
cor.test(bacon,dessert,method="spearman")

tea=c(0.00814, 0.01107, 0.01167, 0.01423, 0.01293)
dessert=c(0.01947, 0.03348, 0.0485, 0.05582, 0.04688)
cor.test(tea,dessert,method="pearson")
cor.test(tea,dessert,method="kendall")  #p-value = 0.08333
cor.test(tea,dessert,method="spearman")  #p-value = 0.08333
