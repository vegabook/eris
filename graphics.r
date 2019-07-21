#convexity charts

source("tools.r")
source("common.r")

library(ggplot2)
library(colorspace)
library(gridExtra)

# ------------------- utilities ----------------------

if(Sys.info()[["sysname"]] == "Linux") windows <- function(w, h) x11(width = w, height = h)

viewColours <- function() {
    windows(20, 10)
    hcl_palettes(plot = TRUE)
}

cc <- function() source("graphics.r")

colourway = qualitative_hcl(4, palette = "Cold")

# ------------------- data ----------------------
chainData <- function(leader = "EDZ5 Comdty") {
    bds(leader, "fut chain")[, 1]
}

yieldData <- function(futures) {
    # futures come from chainData
    data1 <- bdp(futures, c("mifid_maturity_date", "convexity_bias_basis_points", "convexity_adjusted_rate", "yld_ytm_mid"))
    data1 <- na.omit(data1)
    colnames(data1) <- c("maturity", "convexity_bps", "convexity_adjusted", "linear_yield")
    data1["contract"] = rownames(data1)
    data1["labels"] = apply(data1, 1, function(x) paste(strsplit(x[5], " ")[[1]][1], format(as.Date(x[1]), "%b-%y")))
    data1
}


saveData <- function(structures) {
    save(structures, file = "structures.dat")
}

loadData <- function() {
    load("structures.dat")
    for(s in names(structures)) {
        assign(s, structures[[s]], envir = .GlobalEnv)
    }
    structures <<- structures
}

loadData()

initData <- function() {
    futureChain <<- chainData()
    data1 <<- yieldData(futureChain)
    structures <<- list("futureChain" = futureChain, 
                      "data1" = data1)
}


# --------------------graphics code convexity vs linear -------------------

convexity_chart <- function(yields, chartnum) {
    # data come from yieldData
    data <- melt(yields[, c("maturity", "convexity_adjusted", "linear_yield")], 
                 id = c("maturity"))
    plt1 <- ggplot(data, aes(x = maturity, y = value, col = variable))
    plt1 <- plt1 + geom_line(lwd = 1)
    #plt1 <- plt1 + geom_point(col = colourway[1], fill = "white", pch = 21, size = 2)
    plt1 <- plt1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    plt1 <- plt1 + theme(legend.position = "bottom")
    plt1 <- plt1 + labs(title = "Convexity-adjusted vs linear EuroDollar rates",
                        subtitle = paste("Chart", chartnum))
    plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue"))
    plt1 <- plt1 + scale_colour_discrete_qualitative(palette = "Cold")
    return(plt1)
}


convexity_spread <- function(yields, chartnum) {
    # data come from yieldData
    data <- melt(yields[, c("maturity", "convexity_bps")], 
                 id = c("maturity"))
    plt1 <- ggplot(data, aes(x = maturity, y = value, col = variable))
    plt1 <- plt1 + geom_line(lwd = 1)
    #plt1 <- plt1 + geom_point(col = colourway[1], fill = "white", pch = 21, size = 2)
    plt1 <- plt1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    plt1 <- plt1 + theme(legend.position = "bottom")
    plt1 <- plt1 + labs(title = "Value of convexity in bps",
                        subtitle = paste("Chart", chartnum))
    plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue"))
    return(plt1)
}

# --------------------graphics code convexity formula -------------------


convex_price_from_rate <- function(start_price, rates, years) {
    start_price / (1 + rates) ** years
}

linear_price_from_range <- function(start_price, rates, years) {
    start_price - (rates * 100 * years)
}


data_for_chart <- function(start_price, years, start_rate, end_rate, entry_rate) {
    yield_change <- seq(start_rate, end_rate, by = 0.0005)
    convprices <- convex_price_from_rate(start_price, rates = yield_change, years = years)
    linear <- linear_price_from_range(start_price, rates = yield_change, years)
    data.frame(yield_change, "true_price" = convprices, linear)
}

linconv_chart <- function(data, chartnum, titl) {
    datamelt <- melt(data, id = c("yield_change"))

    plt1 <- ggplot(datamelt, aes(x = yield_change, y = value, col = variable))
    plt1 <- plt1 + geom_line(lwd = 1)
    plt1 <- plt1 + theme(legend.position = "bottom")
    plt1 <- plt1 + labs(title = titl,
                        subtitle = paste("Chart", chartnum))
    plt1 <- plt1 + xlab("yield change")
    plt1 <- plt1 + ylab("price change")
    plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue"))
    plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue"))
    plt1 <- plt1 + scale_colour_discrete_qualitative(palette = "Cold")
    plt1 <- plt1 + geom_vline(xintercept = 0, colour = "grey65", lty = "dashed")


    return(plt1)

}


# ------------------- do it all -----------------------------------

dodo <- function(topng = FALSE) {
    

    # chart 3 and 4
    chartsize <- c(9, 5)
    cc1 <- convexity_chart(data1, 3)
    cc2 <- convexity_spread(data1, 4)
    if(topng) {
        png("cc1.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 600)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(cc1, cc2, nrow = 1)
    if(topng) dev.off()

    # chart 3 and 4
    data1 <- data_for_chart(start_price = 102.5, 
                            years = 2, 
                            start_rate = -0.05,
                            end_rate = 0.05, 
                            entry_rate = 0.025)

    data2 <- data_for_chart(start_price = 102.5, 
                            years = 10, 
                            start_rate = -0.05,
                            end_rate = 0.05, 
                            entry_rate = 0.025)

    chartsize <- c(9, 5)
    cc1 <- linconv_chart(data1, 1, "Convexity effect - 2y instrument")
    cc2 <- linconv_chart(data2, 2, "Convexity effect - 10y instrument")
    if(topng) {
        png("cc2.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 600)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(cc1, cc2, nrow = 1)
    if(topng) dev.off()

}

dodo(T)





