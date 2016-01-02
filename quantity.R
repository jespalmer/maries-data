##quantity of products sold for specified date
# quantity <- lapply(productlist, function(prodname) {
#   subset1 <- monthsales[monthsales$product_name == prodname, ]
#   return(nrow(subset1))
# })
#
# framelen <- length(quantity)
#
# nodata <- data.frame(product=character(framelen), quantity=integer(framelen))
# nodata$quantity <- as.numeric(quantity)
# nodata$product <- as.character(productlist)
#
# quantityprod <- nodata[order(-nodata$quantity, nodata$product), ]

quantreport <- function (file, date = 1:12) {

  #read the file
  orders <- read.csv(file, header=TRUE, sep ="\t")

  #change factor dates into date class
  library(lubridate)
  orders$newdates <- ymd(as.character(orders$placed_date))

  #get the right columns we need
  cleandata <- orders[, c(1, 4, 5, 7)]

  #convert the dates to the right format
  for (d in date < 10) {
  gooddates <- lapply(date, function(n) sprintf("%02d", n))
  }

  monthsales <- subset(cleandata, format.Date(newdates, "%m") %in% gooddates)
  productlist <- levels(factor(monthsales$product_name))

  #data we need to run linear regression by date purchased:
  library(plyr)
  productlists <- tapply(monthsales$product_name, as.Date(monthsales$newdates), count)

  productlist1 <- as.character()
  quantitylist <- as.character()
  datelist <- as.character()

  dates <- as.character(names(productlists))

  for (i in 1:length(productlists)) {
    date <- dates[i]
    list <- productlists[i][[date]]
    unique(unlist(list[1])) -> tmp
    as.character(unlist(list[2])) -> tmp1
    rep(date, length(tmp)) -> tmp2

    productlist1 <- c(productlist1, tmp)
    quantitylist <- c(quantitylist, tmp1)
    datelist <- c(datelist, tmp2)
  }

  for (i in 1:length(productlist1)) {
    prodindex <- as.numeric(productlist1[i])
    productlist1[i] <- productlist[prodindex]
  }

  framelenq <- length(quantitylist)
  framelenp <- length(productlist1)
  framelend <- length(datelist)

  nodata <- data.frame(product=character(framelenp), quantity=integer(framelenq), date=(framelend))
  nodata$quantity <- as.numeric(quantitylist)
  nodata$product <- as.character(productlist1)
  nodata$date <- as.Date(datelist)

  final <- nodata[order(nodata$date, nodata$product), ]
  print(final)
}

#regression
reg <- subset(final, final$product == "Cherry Pie Baked 9-inch")
print(reg)
plot(reg$date, reg$quantity, pch=19, xlab="Date Sold", ylab="Quantity Sold", main="Unit Cherry Pie Baked 9-inch Sales")
lm.out <- lm(reg$quantity ~ reg$date)
print(summary(lm.out))
abline(lm(reg$quantity~reg$date), col="red")
#
#forcasting
len <- length(reg$quantity)
library(forecast)
require(lubridate)
y = ts(reg$quantity, start=c(2015, yday("2015-01-01")), frequency=len)
plot(forecast(ets(y), 100), xaxt="n", shadecols="oldstyle")
a =seq(as.Date("2015-01-01"), by="months", length=len)
axis(1, at=decimal_date(a), labels=format(a, "%Y %b"), cex.axis=0.6)

  #
  # #data we need to run linear regression by day of the month:
  # tapply(monthsales$product_name, day(ymd(monthsales$newdates)), count)
  #
  # #data we need to run linear regression by month:
  # tapply(monthsales$product_name, month(ymd(monthsales$newdates)), count)
  #
  # #data we need to run linear regression by year:
  # tapply(monthsales$product_name, year(ymd(monthsales$newdates)), count)
