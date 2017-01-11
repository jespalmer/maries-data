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
header("2016 Wholesale Totals | August 2016 compared to July 2016 2016")
report("fixed_orders.csv", month1 = "08", month2 = "07", year1 = "16", year2 = "16")
