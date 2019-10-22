gg <- function() source("graphics2.r") 
if(Sys.info()[["sysname"]] == "Linux") windows <- function(w, h) x11(width = w, height = h)
source("../../rtools/tools.r")
source("../../rtools/common.r")

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

zero_charts <- function(topng = F) {
    chartsize <- c(9, 5)
    if(topng) {
        png("zeros.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 200, pointsize = 12)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    data <- read.csv("cashflows2.csv")
    discnov <- as.numeric(read.csv("discnov3018.txt", header = F)[, 1])
    discoct <- as.numeric(read.csv("discoct1019.txt", header = F)[, 1])
    zeronov <- as.numeric(substr(read.csv("zeronov3018.txt", header = F)[, 1], 1, 8))
    zerooct <- as.numeric(substr(read.csv("zerooct1019.txt", header = F)[, 1], 1, 8))
    data["discnov"] <- discnov
    data["discoct"] <- discoct
    data["zeronov"] <- zeronov
    data["zerooct"] <- zerooct
    data[, "Payment.Date"] <- as.Date(data[, "Payment.Date"])
    fixed <- data[data[, "Is.Fixed"] == "True", ]
    floating <- data[data[, "Is.Fixed"] == "False", ]
    plot(floating[, 2], floating[, "zeronov"], col = "green3", type = "b", xlab = "", ylab = "", ylim = c(1.4, 3), lwd = 2)
    points(floating[, 2], floating[, "zerooct"], col = "darkgoldenrod2", type = "b", xlab = "", ylab = "", lwd = 2)
    legend("bottomleft", fill = c("green3", "darkgoldenrod2"), legend = c("November 2018", "October 2019"))
    if(topng) dev.off()
    if(topng) {
        png("discounts.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 200, pointsize = 12)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    barplot(as.matrix(floating[, c("discnov", "discoct")]), beside = T, ylim = c(0, 1), 
         names.arg = unlist(rep(floating[, "Payment.Date"], 2)), las = 2, cex.names = 0.9, 
         border = c(rep("green3", 20), rep("darkgoldenrod2", 20)), col = "grey90")
    abline(h = 1, lty = "dashed")
    if(topng) dev.off()

}

cf_tableNov <- function(topng = F) {
    data <- read.csv("cashflows.csv")
    discnov <- as.numeric(read.csv("discnov3018.txt", header = F)[, 1])
    discoct <- as.numeric(read.csv("discoct1019.txt", header = F)[, 1])
    zeronov <- as.numeric(substr(read.csv("zeronov3018.txt", header = F)[, 1], 1, 8))
    zerooct <- as.numeric(substr(read.csv("zerooct1019.txt", header = F)[, 1], 1, 8))
    data["discnov"] <- discnov
    data["discoct"] <- discoct
    data["zeronov"] <- zeronov
    data["zerooct"] <- zerooct
    data[, "Payment.Date"] <- as.Date(data[, "Payment.Date"])
    fixed <- data[data[, "Is.Fixed"] == "True", ]
    floating <- data[data[, "Is.Fixed"] == "False", ]
    fixed["npv"] <- fixed[, "Amount"] * fixed[, "discnov"]
    floating["npv"] <- floating[, "Amount"] * floating[, "discnov"]
    write.csv(fixed, "fixednov.csv")
    write.csv(floating, "floatingnov.csv")
}

cf_tableOct <- function(topng = F) {
    data <- read.csv("cashflows2.csv")
    discnov <- as.numeric(read.csv("discnov3018.txt", header = F)[, 1])
    discoct <- as.numeric(read.csv("discoct1019.txt", header = F)[, 1])
    zeronov <- as.numeric(substr(read.csv("zeronov3018.txt", header = F)[, 1], 1, 8))
    zerooct <- as.numeric(substr(read.csv("zerooct1019.txt", header = F)[, 1], 1, 8))
    data["discnov"] <- discnov
    data["discoct"] <- discoct
    data["zeronov"] <- zeronov
    data["zerooct"] <- zerooct
    data[, "Payment.Date"] <- as.Date(data[, "Payment.Date"])
    fixed <- data[data[, "Is.Fixed"] == "True", ]
    floating <- data[data[, "Is.Fixed"] == "False", ]
    fixed["npv"] <- fixed[, "Amount"] * fixed[, "discoct"]
    floating["npv"] <- floating[, "Amount"] * floating[, "discoct"]
    write.csv(fixed, "fixedoct.csv")
    write.csv(floating, "floatingoct.csv")
}

reg_charts <- function(topng = F) {
    data = load("cb2.data")

    regress(cb2[, 1], cb2[, 2], ylab = "Eris 5y Sep19 LIWU19", xlab = "Calculated TRI from IRS market", 
            main = "")

    dev.new()
    colnames(cb2) <- c("Eris September 2019 LIWU19", "NPV of equivalent IRS from first principles")
    plot(cb2, main = "Eris Futures vs equivalent IRS NPV", cex.main = 10, legend.loc = "bottomright")
}


rr <- function(topng = F) {
    cashflows_chart(topng = topng)
    cashflows2_chart(topng = topng)
    zero_charts(topng = topng)
    cf_tableNov(topng = topng)
    cf_tableOct(topng = topng)
    reg_charts(topng = topng)
}
    




