source("../tools.r")
source("../common.r")

histyears <- 1
maturityindex <- c(0.25, 1, 2, 3, 4, 5, 7, 10, 15, 20, 30)
maturities <- c("C", as.character(maturityindex[-1]))

g <- function() source("graphics3.r")


sofrbasis <- function(years = histyears) {
# LIBOR 3m vs SOFR basis
    tickers <- paste("USSRVL", maturities, " Curncy", sep = "")
    data <- bbdh(tickers, years)
    data <- na.locf(data)
    colnames(data) <- c(0.25, maturities)
    return(data)
}

liborimplied <- function(years = histyears) {
# evolution of current LIBOR via eurodollars
    tickers <- paste("ED", 1:20, " Comdty", sep = "")
    data <- bbdh(tickers, years)
    data <- 100 - data
    libor = bbdh("US0003M Index", years)
    data <- cbind(libor, data)
    data <- na.locf(data)
    colnames(data) <- c(0, 1:20) / 4
    return(data)
}

sofrirs <- function(years = histyears) {
    tickers <- paste("USOSFR", maturities, " Curncy", sep = "")
    data <- bbdh(tickers, years)
    data <- na.locf(data)
    colnames(data) <- maturityindex
    return(data)
}

annualirs <- function(years = histyears) {
    tickers <- paste("USSA", maturities, " Curncy", sep = "")
    data <- bbdh(tickers, years)
    data <- na.locf(data)
    colnames(data) <- maturityindex
    return(data)
}


















