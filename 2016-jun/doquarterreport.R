source("monthreport.R")

header <- function(title){
  cat("\n", "---------------------------------------------------------------",
  "\n")
  cat(title)
  cat("\n", "---------------------------------------------------------------",
  "\n")
}

#main report title and date
header("2016 Q1 Wholesale Summary ran on August 7, 2016")

#Monthly summary
header("2016 Q1 Wholesale Totals | Q2 2015 compared to Q2 2016")
report("fixed_orders.csv", month1= "06", month2= "06", year1="16", year2="15")
