library(ggplot2)
library(data.table)
library(vcd)
library(Ridit)

x <- c(89,37,30,28,2)
p <- c(40,20,20,15,5)
chisq.test(x, p = p, rescale.p = TRUE)

star=c(1,2,3,4,5)
total_count = c(49755, 44397, 64618, 132081, 214834)

coffee_good_location=c(960.9926, 1328.2843, 3496.2372, 8850.9833, 10998.8112)
not_coffee=total_count-coffee_good_location
chisq.test(coffee_good_location, p=not_coffee, rescale.p = TRUE)
chisq.test(coffee_good_location, p=not_coffee, rescale.p = TRUE,correct=FALSE)
star1=c(960.9926,48794.0074)
star2=c(1328.2843,43068.7157)
star3=c(3496.2372,61121.7628)
star4=c(8850.9833,123230.0167)
star5=c(10998.8112,203835.1888)
(firsttable=data.frame(star1,star2,star3,star4,star5,row.names = c("coffee&good location","not coffee")))


wait_time_service = c(1689.5719, 1284.7484, 1662.8552, 2283.4284, 2997.6589)
not_wait = total_count-wait_time_service
chisq.test(wait_time_service, p=not_wait, rescale.p = TRUE)
star1=c(1689.5719,48065.43)
star2=c(1284.7484,43112.25)
star3=c(1662.8552,43112.25)
star4=c(2283.4284,43112.25)
star5=c(2997.6589,211836.34)
(secondtable=data.frame(star1,star2,star3,star4,star5,row.names = c("wait time service","not wait")))


nice_service_amazing_atmosphere=c(918.469, 1295.7542, 4310.386, 22691.7687, 65116.1658)
not_nice_service = total_count-nice_service_amazing_atmosphere
chisq.test(nice_service_amazing_atmosphere, p=not_nice_service, rescale.p = TRUE)
star1=c(918.469,48836.53)
star2=c(1295.7542,43101.25)
star3=c(4310.386,60307.61)
star4=c(22691.7687,109389.23)
star5=c(65116.1658,149717.83)
(thirdtable=data.frame(star1,star2,star3,star4,star5,row.names = c("nice service&amazing atmosphere","not nice service")))


Mexican_food=c(852.9652, 952.7788, 1637.9559, 4285.9031, 7956.2303)
not_Mexican = total_count-Mexican_food
chisq.test(Mexican_food, p=not_Mexican, rescale.p = TRUE)
star1=c(852.9652,48902.03)
star2=c(952.7788,43444.22)
star3=c(1637.9559,62980.04)
star4=c(4285.9031,127795.10)
star5=c(7956.2303,206877.77)
(fourthtable=data.frame(star1,star2,star3,star4,star5,row.names = c("Mexican food","not Mexican")))


wait_long_time=c(26858.2525, 18794.0274, 16856.2699, 14360.2652, 12782.2937)
not_wait_long = total_count-wait_long_time
chisq.test(wait_long_time, p=not_wait_long, rescale.p = TRUE)
star1=c(26858.2525,22896.75)
star2=c(18794.0274,25602.97)
star3=c(16856.2699,47761.73)
star4=c(14360.2652,117720.73)
star5=c(12782.2937,202051.71)
(fifthtable=data.frame(star1,star2,star3,star4,star5,row.names = c("wait long time","not wait long")))


open_at_night_alcohol=c(707.8647, 821.4149, 1499.0914, 4243.4561, 8652.448)
not_open_at_night = total_count-open_at_night_alcohol
chisq.test(open_at_night_alcohol, p=not_open_at_night, rescale.p = TRUE)
star1=c(707.8647,49047.14)
star2=c(821.4149,43575.59)
star3=c(1499.0914,63118.91)
star4=c(4243.4561,127837.54)
star5=c(8652.448,206181.55)
(sixthtable=data.frame(star1,star2,star3,star4,star5,row.names = c("open at night&alcohol","not open at night&alcohol")))


buffet_affordable_price=c(2597.2784, 2916.1684, 4632.6847, 6838.5166, 7960.2587)
not_affordable = total_count - buffet_affordable_price
chisq.test(buffet_affordable_price, p=not_affordable, rescale.p = TRUE)
star1=c(2597.2784,47157.72)
star2=c(2916.1684,41480.83)
star3=c(4632.6847,59985.32)
star4=c(6838.5166,125242.48)
star5=c(7960.2587,206873.74)
(seventhtable=data.frame(star1,star2,star3,star4,star5,row.names = c("buffet&affordable price","not buffet&affordable price")))


main_courses=c(3416.6152, 5849.6538, 10787.5383, 20898.4015, 22782.1599)
not_main_courses = total_count - main_courses
chisq.test(main_courses, p=not_main_courses, rescale.p = TRUE)

coffee_tea_donut_pastry=c(1230.9865, 1440.4776, 3193.8427, 9736.2134, 18129.7841)
not_coffee_tea = total_count - coffee_tea_donut_pastry
chisq.test(coffee_tea_donut_pastry, p=not_coffee_tea, rescale.p = TRUE)

southern_food_seafood=c(586.7675, 477.4901, 732.8325, 1326.4049, 1746.6851)
not_seafood = total_count - southern_food_seafood
chisq.test(southern_food_seafood, p=not_seafood, rescale.p = TRUE)

bruschetta_bar_happy_hour=c(503.0687, 554.3443, 1257.667, 3440.1115, 5552.4856)
not_happy_hour = total_count - bruschetta_bar_happy_hour
chisq.test(bruschetta_bar_happy_hour, p=not_happy_hour, rescale.p = TRUE)

pancake_waffle_egg_toast_omelet=c(1807.0657, 2974.1705, 7308.9436, 20345.9748, 29058.1151)
not_pancake = total_count- pancake_waffle_egg_toast_omelet
chisq.test(pancake_waffle_egg_toast_omelet, p=not_pancake, rescale.p = TRUE)

bagel_cream_cheese=c(924.6483, 703.0918, 884.2743, 1735.5246, 2924.6957)
not_bagel = total_count - bagel_cream_cheese
chisq.test(bagel_cream_cheese, p=not_bagel, rescale.p = TRUE)

new=data.frame(star,coffee_good_location,not_coffee,wait_time_service,not_wait,
               nice_service_amazing_atmosphere,not_nice_service,Mexican_food,not_Mexican,
               wait_long_time,not_wait_long,open_at_night_alcohol,not_open_at_night,
               buffet_affordable_price,not_affordable,main_courses,not_main_courses,
               coffee_tea_donut_pastry,not_coffee_tea,southern_food_seafood,not_seafood,
               bruschetta_bar_happy_hour,not_happy_hour,pancake_waffle_egg_toast_omelet,not_pancake,
               bagel_cream_cheese,not_bagel)

improved=c(rep("coffee_good_location",5),rep("not_coffee",5))
stars=rep(star,2)
values=c(coffee_good_location,not_coffee)
first=data.frame(stars,improved,values)



#with(first,table(stars,improved))
#xtabs(~improved+stars,data=first)



#### mosaic plot ####
##read processed business attribute
dat = read.csv("attribute.csv")
attach(dat)

mosaic(Titanic,shade=TRUE,legend=TRUE)
w = data.table(RestaurantsTableService=1:5,OutdoorSeating=1:5,BusinessParking=1:5,NoiseLevel=1:5)

new=ftable(dat[c("stars","RestaurantsTableService","OutdoorSeating"),c(1:100)])
mosaic(new)


