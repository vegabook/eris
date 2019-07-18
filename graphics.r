#convexity charts

source("u:/code/rtools/tools.r")
source("u:/code/rtools/common.r")

library(ggplot2)
library(colorspace)
library(gridExtra)

# ------------------- utilities ----------------------

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

initData <- function() {
    futureChain <<- chainData()
    data1 <<- yieldData(futureChain)
    structures <<- list("futureChain" = futureChain, 
                      "data1" = data1)
}


# --------------------graphics code -------------------

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

dodo <- function(topng = FALSE) {
    cc1 <- convexity_chart(data1, 1)
    cc2 <- convexity_spread(data1, 2)
    chartsize <- c(9, 5)
    if(topng) {
        png("cc1.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 600)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(cc1, cc2, nrow = 1)
    if(topng) dev.off()
}





