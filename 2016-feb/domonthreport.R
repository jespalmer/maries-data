source("monthreport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Monthly Wholesale Summary ran on Mar 1, 2016")

#Monthly summary
header("2016 Wholesale Totals | February")
report("fixed_orders.csv", month1= "02", month2= "01", year1="16", year2="16")
