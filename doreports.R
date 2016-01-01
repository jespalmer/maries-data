source("salesreport.R")

header <- function(title) {
  cat("\n", "--------------------------------------------------------------------------------------", "\n")
  cat(title)
  cat("\n", "--------------------------------------------------------------------------------------", "\n")
}

#main report title and date
header("2015 Gross Wholesale Summary ran on Dec 31, 2015")

#year summary
header("2015 Gross Wholesale Totals | All Quarters")
report("fixed_orders.csv")
#year top products as percentage of YTD
header("2015 Wholesale Totals | All Quarters | Bestselling Products")
report("fixed_orders.csv", product="best", store="skip")
#year top stores as percentage of YTD
header("2015 Wholesale Totals | All Quarters | Bestselling Stores")
report("fixed_orders.csv", product="skip", store="best")
#year bottom products as percentage of YTD
header("2015 Wholesale Totals | All Quarters | Worstselling Products")
report("fixed_orders.csv", product="worst", store="skip")
#year bottom stores as percentage of YTD
header("2015 Wholesale Totals | All Quarters | Worstselling Stores")
report("fixed_orders.csv", product="skip", store="worst")

#Q1 summary
header("2015 Wholesale Totals | Q1 (Jan, Feb, Mar)")
report("fixed_orders.csv", date=1:3)
#Q1 top products as percentage of YTD
header("2015 Wholesale Totals | Q1 (Jan, Feb, Mar) | Bestselling Products")
report("fixed_orders.csv", date=1:3, product="best", store="skip")
#Q1 top stores as percentage of YTD
header("2015 Wholesale Totals | Q1 (Jan, Feb, Mar) | Bestselling Stores")
report("fixed_orders.csv", date=1:3, product="skip", store="best")
#Q1 bottom products as percentage of YTD
header("2015 Wholesale Totals | Q1 (Jan, Feb, Mar) | Worstselling Products")
report("fixed_orders.csv", date=1:3, product="worst", store="skip")
#Q1 bottom stores as percentage of YTD
header("2015 Wholesale Totals | Q1 (Jan, Feb, Mar) | Worstselling Stores")
report("fixed_orders.csv", date=1:3, product="skip", store="worst")

#Q2 summary
header("2015 Wholesale Totals | Q2 (Apr, May, Jun)")
report("fixed_orders.csv", date=4:6)
#Q2 top products as percentage of YTD
header("2015 Wholesale Totals | Q2 (Apr, May, Jun) | Bestselling Products")
report("fixed_orders.csv", date=4:6, product="best", store="skip")
#Q2 top stores as percentage of YTD
header("2015 Wholesale Totals | Q2 (Apr, May, Jun) | Bestselling Stores")
report("fixed_orders.csv", date=4:6, product="skip", store="best")
#Q2 bottom products as percentage of YTD
header("2015 Wholesale Totals | Q2 (Apr, May, Jun) | Worstselling Products")
report("fixed_orders.csv", date=4:6, product="worst", store="skip")
#Q2 bottom stores as percentage of YTD
header("2015 Wholesale Totals | Q2 (Apr, May, Jun) | Worstselling Stores")
report("fixed_orders.csv", date=4:6, product="skip", store="worst")

#Q3 summary
header("2015 Wholesale Sales | Q3 (Jul, Aug, Sept)")
report("fixed_orders.csv", date=7:9)
#Q3 top products as percentage of YTD
header("2015 Wholesale Totals | Q3 (Jul, Aug, Sept) | Bestselling Products")
report("fixed_orders.csv", date=7:9, product="best", store="skip")
#Q3 top stores as percentage of YTD
header("2015 Wholesale Totals | Q3 (Jul, Aug, Sept) | Bestselling Stores")
report("fixed_orders.csv", date=7:9, product="skip", store="best")
#Q3 bottom products as percentage of YTD
header("2015 Wholesale Totals | Q3 (Jul, Aug, Sept) | Worstselling Products")
report("fixed_orders.csv", date=7:9, product="worst", store="skip")
#Q3 bottom stores as percentage of YTD
header("2015 Wholesale Totals | Q3 (Jul, Aug, Sept) | Worstselling Stores")
report("fixed_orders.csv", date=7:9, product="skip", store="worst")

#Q4 summary
header("2015 Q4 Wholesale Sales | Q4 (Oct, Nov, Dec)")
report("fixed_orders.csv", date=10:12)
#Q4 top products as percentage of YTD
header("2015 Wholesale Totals | Q4 (Oct, Nov, Dec) | Bestselling Products")
report("fixed_orders.csv", date=10:12, product="best", store="skip")
#Q4 top stores as percentage of YTD
header("2015 Wholesale Totals | Q4 (Oct, Nov, Dec) | Bestselling Stores")
report("fixed_orders.csv", date=10:12, product="skip", store="best")
#Q4 bottom products as percentage of YTD
header("2015 Wholesale Totals | Q4 (Oct, Nov, Dec) | Worstselling Products")
report("fixed_orders.csv", date=10:12, product="worst", store="skip")
#Q4 bottom stores as percentage of YTD
header("2015 Wholesale Totals | Q4 (Oct, Nov, Dec) | Worstselling Stores")
report("fixed_orders.csv", date=10:12, product="skip", store="worst")
