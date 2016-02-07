source("monthreport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Monthly Wholesale Summary ran on Feb 6, 2016")

#Monthly summary
header("2016 Wholesale Totals | January")
report("fixed_orders.csv", month1= "01", month2= "01", year1="16", year2="15")
