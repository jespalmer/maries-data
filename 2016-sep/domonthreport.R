source("monthreport.R")

header <- function(title) {
    cat("\n", "---------------------------------------------------------------",
        "\n")
    cat(title)
    cat("\n", "---------------------------------------------------------------",
        "\n")
}

# main report title and date
header("2016 Monthly Wholesale Summary ran on October 31, 2016")

# Monthly summary
header("2016 Wholesale Totals | 2016 Quarter 3 Report | July, Aug, Sept 2016 compared to Q3 2015")
report("fixed_orders.csv", month1 = "09", month2 = "09", year1 = "16", year2 = "15")
