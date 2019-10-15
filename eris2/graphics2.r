gg <- function() source("graphics2.r") 

cashflows_chart <- function(topng = F) {
    chartsize <- c(9, 5)
    if(topng) {
        png("cashflows.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 200, pointsize = 12)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    data <- read.csv("cashflows.csv")
    data[, "Payment.Date"] <- as.Date(data[, "Payment.Date"])
    fixed <- data[data[, "Is.Fixed"] == "True", ]
    floating <- data[data[, "Is.Fixed"] == "False", ]
    plot(floating[, 2], -floating[, "Amount"], ylim = c(-15000, 15000), col = "white", 
         xlab = "", ylab = "Cashflow amount")
    arrows(floating[, 2], -floating[, "Amount"], y1 = 0, code = 1, col = "red")
    points(fixed[, 2], fixed[, "Amount"], col = "white")
    arrows(fixed[, 2], fixed[, "Amount"], y1 = 0, code = 1, col = "dodgerblue")
    abline(h = 0, lty = "dashed")
    legend("bottomright", fill = c("red", "dodgerblue"), legend = c("Floating", "Fixed"))
    if(topng) dev.off()
}

cashflows2_chart <- function(topng = F) {
    chartsize <- c(9, 5)
    if(topng) {
        png("cashflows2.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 200, pointsize = 12)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    data <- read.csv("cashflows2.csv")
    data[, "Payment.Date"] <- as.Date(data[, "Payment.Date"])
    fixed <- data[data[, "Is.Fixed"] == "True", ]
    floating <- data[data[, "Is.Fixed"] == "False", ]
    plot(floating[, 2], -floating[, "Amount"], ylim = c(-15000, 15000), col = "white", 
         xlab = "", ylab = "Cashflow amount")
    arrows(floating[, 2], -floating[, "Amount"], y1 = 0, code = 1, col = "red")
    points(fixed[, 2], fixed[, "Amount"], col = "white")
    arrows(fixed[, 2], fixed[, "Amount"], y1 = 0, code = 1, col = "dodgerblue")
    abline(h = 0, lty = "dashed")
    legend("bottomright", fill = c("red", "dodgerblue"), legend = c("Floating", "Fixed"))
    if(topng) dev.off()
}


rr <- function(topng = F) {
    cashflows_chart(topng = topng)
    cashflows2_chart(topng = topng)
}
    





