#convexity charts

source("tools.r")
source("common.r")

library(ggplot2)
library(ggpmisc)
library(colorspace)
library(gridExtra)
library(lme4)
library(parallel)
library(leaps)

# ------------------- utilities ----------------------

if(Sys.info()[["sysname"]] == "Linux") windows <- function(w, h) x11(width = w, height = h)

viewColours <- function() {
    windows(20, 10)
    hcl_palettes(plot = TRUE)
}

cc <- function() source("graphics.r")

colourway = qualitative_hcl(4, palette = "Cold")

smooth_series <- function(ser, sds = 3) {
    browser()
    dser <- diff(ser)
    bad <- abs(dser) > (sd(na.omit(dser)) * sds)
    bad[is.na(bad)] <- FALSE
    ser[bad] <- NA
    na.locf(ser)
}

# ------------------- data ----------------------
chainData <- function(leader = "EDZ5 Comdty") {
    # targets futureChain
    bds(leader, "fut chain")[, 1]
}

OldChainData <- function(start = 14, end = 26) {
    apply(expand.grid(c("H", "M", "U", "Z"), start:end), 1, function(x) {
          paste("ED", x[1], x[2], " Comdty", sep = "")
    })
}

historicFutures <- function(futures) {
    # futures come from chainData
    # targets edfutures
    na.locf(bbdh(futures, 5, "last price", asDateNotPosix = TRUE))
}

yieldData <- function(futures) {
    # futures come from chainData
    # targets data1
    data <- bdp(futures, c("mifid_maturity_date", "convexity_bias_basis_points", 
                            "convexity_adjusted_rate", "yld_ytm_mid"))
    colnames(data) <- c("maturity", "convexity_bps", "convexity_adjusted", "linear_yield")
    data["contract"] = rownames(data)
    data["labels"] = apply(data, 1, function(x) paste(strsplit(x[5], " ")[[1]][1], 
                                                        format(as.Date(x[1]), "%b-%y")))
    return(data)
}


saveData <- function(strucs) {
    save(strucs, file = "structures.dat")
}

loadData <- function() {
    load("structures.dat", envir = .GlobalEnv)
    for(s in names(strucs)) {
        print(s)
    }
}

initData <- function() {
    futureChain <<- chainData()
    oldFutureChain <<- OldChainData()
    data1 <<- na.omit(yieldData(futureChain))
    dataOld <<- yieldData(oldFutureChain)
    edfutures <<- historicFutures(futureChain)
    edhistoric <<- historicFutures(oldFutureChain)
    edtris <<- ed_tris(edhistoric, dataOld)
    eristris <<- eris_sheet_tris()
    irstris <<- irs_tris(sheetname = "./usd_irs_pnl/usdpnl.csv") 
    irstrisnoroll <<- irs_tris(sheetname = "./usd_irs_pnl/usdpnl_noroll.csv") 
    irstriscoupon <<- irs_tris_coupon(dirname = "./usd_irs_pnl/")
    swapdata <<- swap_data()
    strucs <<- list("futureChain" = futureChain, 
                      "oldFutureChain" = oldFutureChain,
                      "data1" = data1,
                      "dataOld" = dataOld,
                      "edfutures" = edfutures,
                      "edhistoric" = edhistoric,
                      "edtris" = edtris,
                      "irstris" = irstris,
                      "irstrisnoroll" = irstrisnoroll,
                      "irstriscoupon" = irstriscoupon,
                      "swapdata" = swapdata,
                      "eristris" = eristris)
}


# --------------- regressions of eris versus irs to show good fit -----------------

filter_irs <- function(irstris, matperiod, fwdperiod = "3m") {
    # use this function to filter the first input to test_regressions for speed
    qual <- paste(fwdperiod, tolower(matperiod), sep = "")  
    filterer1 <- sapply(strsplit(colnames(irstris), " "), function(x) x[2])
    filterer2 <- sapply(strsplit(filterer1, "_"), function(x) x[1])
    ep <- irstris[, filterer2 == qual]
    return(ep)
}


filter_eris <- function(eristris, period) {
    # use this function to filter the second input to test_regressions for speed
    ep <- eristris[, sapply(strsplit(colnames(eristris), " "), function(x) tolower(x[1]) == tolower(period))]
    months <- match(sapply(strsplit(colnames(ep), " "), function(x) x[2]), month.abb)
    years <- sapply(colnames(ep), function(x) strsplit(strsplit(x, "-")[[1]][2], ":")[[1]][1])
    returnorder <- order(paste(years, months))
    return(ep[, returnorder])
}

test_regressions <- function(xts1, xts2, samplesize = 260, sort = F, k = 10) {
    # tests which regress best from all series in xts1 vs all series in xts2
    allcolsx2 <- expand.grid(colnames(xts1), colnames(xts2))
    flushprint(paste("will be testing", nrow(allcolsx2), "combinations"))
    splits <- split(as.matrix(allcolsx2), seq(nrow(allcolsx2)))
    tests <- lapply(splits, function(x) {
              x2 <- na.omit(na.locf(cbind(xts1[, x[1]], xts2[, x[2]])))
              if(nrow(x2) > samplesize) {
                  rollregs <- rollapply(x2, samplesize, function(rollx2) {
                      rollx2r <- diffret(rollx2)
                      lmrsq <- round(summary(lm(rollx2[, 1] ~ rollx2[, 2]))$r.squared, 7)
                      lmrrsq <- round(summary(lm(rollx2r[, 1] ~ rollx2r[, 2]))$r.squared, 7)
                      c(lmrsq, lmrrsq, range(index(rollx2)), x)
                  }, by.column = F, by = k)
                  best <- order(as.numeric(rollregs[, 2]), decreasing = T)[1]
                  bestrange <- as.numeric(rollregs[best, ][, 3:4])
                  data <- x2[(index(x2) >= as.Date(bestrange[1])) & (index(x2) <= as.Date(bestrange[2])), ]
                  return(list("rsq" = as.numeric(rollregs[best, 1]),
                              "rsqr" = as.numeric(rollregs[best, 2]),
                              "data" = data))
             } else {
                 return(NULL)
             }
    })
    return(tests)
}

regressions_chart <- function(regdata, colournum = 1) {
    regcomplete <- regdata[sapply(regdata, function(x) !is.null(x))]
    graphs <- lapply(regcomplete, function(rr) {
        rrdat <- data.frame(rr$data)
        colnames(rrdat) <- gsub("\\.", "_", colnames(rrdat))
        n1 <- colnames(rrdat)[1]
        n2 <- colnames(rrdat)[2]
        plt1 <- ggplot(rrdat, aes_string(x = n1, y = n2))
        plt1 <- plt1 + geom_point(size = 3, col = "white", pch = 21, fill = colourway[colournum])
        # add regression line
        myformula <- n2 ~ n1
        plt1 <- plt1 + geom_smooth(method = "lm", lty = "dashed", col = "grey")
        plt1 <- plt1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
        plt1 <- plt1 + theme(legend.position = "bottom")
        titl <- gsub("X", "", (gsub("_", " ", n2)))
        plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue"))
        plt1 <- plt1 + labs(title = titl)
        plt1 <- plt1 + theme(plot.title = element_text(colour = "dodgerblue"))
        plt1 <- plt1 + scale_colour_discrete_qualitative(palette = "Cold")

        # add regression equation
        lm_eqn <- function(df) {
            m <- lm(y ~ x, data = df)
            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(m)[1]), digits = 3),
                      b = format(unname(coef(m)[2]), digits = 3),
                     r2 = format(summary(m)$r.squared, digits = 3)))
            as.character(as.expression(eq))
        }
        xydf <- rrdat
        colnames(xydf) <- c("x", "y")
        eqtext <- lm_eqn(xydf)
        eqxpos <- range(xydf$x)[1] + diff(range(xydf$x)) * 1 / 3
        eqypos <- range(xydf$y)[1] + diff(range(xydf$y)) * 3 / 4
        plt1 <- plt1 + geom_text(x = eqxpos, y = eqypos, label = eqtext, parse = T, col = "grey40")

        return(plt1)
    })
    return(graphs)
}



# --------------------graphics code convexity vs linear -------------------

convexity_chart <- function(yields, chartnum) {
    # data come from yieldData
    data <- melt(yields[, c("maturity", "convexity_adjusted", "linear_yield")], 
                 id = c("maturity"))
    plt1 <- ggplot(data, aes(x = maturity, y = value, col = variable))
    plt1 <- plt1 + geom_line(lwd = 1)
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
    plt1 <- plt1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    plt1 <- plt1 + theme(legend.position = "bottom")
    plt1 <- plt1 + labs(title = "Value of convexity in bps",
                        subtitle = paste("Chart", chartnum))
    plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue"))
    return(plt1)
}

# -------------------- graphics code convexity formula -------------------


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

# -------------------- ed futures indicies -------------------

ed_tris <- function(futureHistory, maturityData, maturityYears = c(1, 2, 3, 5, 7)) {
    # parameters would be edhistoric and dataOld respectively
    fh <- futureHistory[, !(apply(futureHistory, 2, function(x) all(is.na(x))))]
    fh <- fh[, paste(colnames(fh), "Comdty") %in% maturityData[, "contract"]]
    datedata <- as.data.frame(t(replicate(nrow(fh), 
                 maturityData[paste(colnames(fh), "Comdty"), "maturity"])))
    dtm <- xts(apply(datedata, 2, function(x) x - as.numeric(index(fh))), 
               order.by = index(fh)) # days to maturity
    fhr <- diffret(fh)

    # returns of *relevant* contracts for each maturity
    idxs <- lapply(maturityYears, function(x) { 
                       xx <- fhr
                       xx[dtm[-1, ] < (x * 365)] <- NA
                       return(xx)
                    })
    names(idxs) <- as.character(maturityYears)
    tris <- lapply(idxs, function(x) {
               thisret <- apply(x, 1, function(y) na.omit(y)[1])
               thistri <- genseries(thisret, logrets = F)
               thisxts <- xts(thistri, order.by = index(futureHistory))
               return(thisxts)
    })
    tris <- do.call(cbind.xts, tris)
    colnames(tris) <- paste("ED", maturityYears, "y", sep = "")
    return(tris)
}

# -------------------- eris tris from sheet -------------------

eris_sheet_tris <- function(sheetname = "Eris_Historical_Prices_For_Standards.csv") {
    # parses Geoff Sharp's csv. Assumes:
    #   * dates in column 1
    #   * series start in row 6
    #   * word "SettlementPrice" is at the top of each TRI
    #   * rows 2 and 3 contain short name and zcode respectively
    #   If any of the above change, the routine below will not work

    csv <- read.csv(sheetname, stringsAsFactors = F, header = F)
    tricols <- sapply(csv[5, ], function(x) grepl("SettlementPrice", x))
    names <- csv[2, -1]
    names <- names[names != ""]
    codes <- csv[3, -1]
    codes <- codes[codes != ""]
    dates <- csv[-(1:5), 1]
    dates <- as.Date(sapply(strsplit(dates, "/"), function(x) paste(x[3], x[1], x[2], sep = "-")))
    series <- csv[-(1:5), tricols]
    series[series == ""] <- NA
    trix <- xts(series, order.by = dates)
    storage.mode(trix) <- "numeric"
    colnames(trix) <- paste(names, codes, sep = ":")
    return(trix)
}

tri_data <- function(ed, eris, edselect = "EDM23", erisselect = "5Y Jun 2018-2023:LIWM18") {
    # parameters would be edhistoric and eristris
    bound <- na.omit(na.locf(cbind(ed[, edselect], eris[, erisselect])))
    return(bound)
}

# -------------------- USD curve TRIs --------------------------------------

irs_tris <- function(sheetname = "./usd_irs_pnl/usdpnl.csv") {
    csv <- read.csv(sheetname, stringsAsFactors = F, header = T)
    csvx <- xts(csv[, -which(colnames(csv) %in% c("X", "dates"))], order.by = as.Date(csv[, "dates"]))
    cumx <- apply(csvx / 1e4, 2, cumsum) + 100 # move from base zero, notional 100, to base 100 notional 100
    colnames(cumx) <- paste("IRS", gsub("\\.", "", gsub("X", "", colnames(cumx))) -> colnames(cumx))
    cumx <- xts(cumx, order.by = index(csvx))
    return(cumx)
}

irs_tris_coupon <- function(dirname = "./usd_irs_pnl/") {
    contents <- dir(dirname)
    couponfiles <- paste(dirname, contents[grepl("coupon", contents)], sep = "")
    couponed <- lapply(couponfiles, function(cf) {
        thiscoupon <- read.csv(cf, stringsAsFactors = F)
        thisdata <- read.csv(gsub("coupon", "pnl", cf))
        thisdatand <- thisdata[, -c(1, ncol(thisdata))]
        thiscouponnd <- thiscoupon[, -c(1, ncol(thiscoupon))]
        colnames(thisdatand) <- paste(colnames(thisdatand), round(thiscouponnd[2, ], 3), sep = "_")
        thisx <- xts(thisdatand, order.by = as.Date(thisdata[, 1]))
        return(thisx)
    })
    xtsall <- do.call(cbind.xts, couponed)
    colnames(xtsall) <- paste("IRS", gsub("\\.", "", gsub("X", "", colnames(xtsall))))
    return(xtsall)
}



# -------------------- graphics code beta chart and regression -------------------

tri_line <- function(data, chartnum, titl) {
    # data come from tri_data
    dfdata <- as.data.frame(data)
    dfdata[, 1] <- dfdata[, 1] / as.numeric(dfdata[1, 1])
    dfdata[, 2] <- dfdata[, 2] / as.numeric(dfdata[1, 2])
    dfdata["date"] <- index(data)
    melted <- melt(dfdata, id = "date")
    plt1 <- ggplot(melted, aes(x = date, y = value, col = variable))
    plt1 <- plt1 + geom_line(lwd = 1)
    plt1 <- plt1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    plt1 <- plt1 + theme(legend.position = "bottom")
    plt1 <- plt1 + labs(title = titl,
                        subtitle = paste("Chart", chartnum))
    plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue"))
    plt1 <- plt1 + scale_colour_discrete_qualitative(palette = "Cold")
    return(plt1)
}


tri_regress <- function(data, chartnum, titl) {
    # data come from tri_data
    x12m <- last(data, "12 months")
    x12m[, 1] <- x12m[, 1] / as.numeric(x12m[1, 1])
    x12m[, 2] <- x12m[, 2] / as.numeric(x12m[1, 2])
    x1w <- last(x12m, "1 week")
    x3m <- last(x12m, "3 months")
    x1d <- last(x12m, "1 day")
    x1w <- as.data.frame(x1w)
    x12m <- as.data.frame(x12m)
    x3m <- as.data.frame(x3m)
    x1d <- as.data.frame(x1d)
    x1w["period"] <- rep("1w", nrow(x1w))
    x12m["period"] <- rep("1y", nrow(x12m))
    x3m["period"] <- rep("3m", nrow(x3m))
    x1d["period"] <- rep("1d", nrow(x1d))
    xall <- rbind(x12m, x3m, x1w, x1d)
    xall$period <- factor(xall$period, levels = c("1y", "3m", "1w", "1d"))
    n1 <- colnames(xall)[1]
    n2 <- colnames(xall)[2]
    plt1 <- ggplot(xall, aes_string(x = n1, y = n2, fill = "period"))
    plt1 <- plt1 + geom_point(size = 3, col = "white", pch = 21)
    # add regression line
    myformula <- n2 ~ n1
    plt1 <- plt1 + geom_smooth(method = "lm", lty = "dashed", col = "grey")
    plt1 <- plt1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    plt1 <- plt1 + theme(legend.position = "bottom")
    plt1 <- plt1 + labs(title = titl,
                        subtitle = paste("Chart", chartnum))
    plt1 <- plt1 + theme(plot.subtitle = element_text(colour = "dodgerblue", size = 4))
    plt1 <- plt1 + scale_colour_discrete_qualitative(palette = "Cold")

    # add regression equation
	lm_eqn <- function(df) {
		m <- lm(y ~ x, data = df)
		eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
			 list(a = format(unname(coef(m)[1]), digits = 3),
				  b = format(unname(coef(m)[2]), digits = 3),
				 r2 = format(summary(m)$r.squared, digits = 3)))
		as.character(as.expression(eq))
	}
	xydf <- xall
    colnames(xydf) <- c("x", "y", "period")
    eqtext <- lm_eqn(xydf)
    eqxpos <- range(xydf$x)[1] + diff(range(xydf$x)) * 1 / 3
    eqypos <- range(xydf$y)[1] + diff(range(xydf$y)) * 3 / 4

	plt1 <- plt1 + geom_text(x = eqxpos, y = eqypos, label = eqtext, parse = T, col = "grey40")
    return(plt1)
}
	
# -------------------- IRS PCA ---------------------------

swap_data <- function(mats = c(2, 3, 5, 7, 10, 15, 20, 30), years = 10) {
    na.locf(bbdh(paste("USSW", mats, " Curncy", sep = ""), years, asDateNotPosix == TRUE))
}

tri_select <- function(matx = irstris, mats = c(2, 3, 5, 7, 10, 15, 20, 30), years = 10) {
    filtered <- lapply(mats, function(x) filter_irs(matx, paste(x, "y", sep = "")))
    tris <- do.call(cbind, filtered)
    colnames(tris) <- paste("tri", mats, sep = "")
    return(tris)
}

all_reg_regressions <- function(inmat, alienmat, nvmx = 4, nbst = 1, rollperiod = NULL) {
    # takes inmat, does regsubsets on each column for all other columns 
    # takes alienmat and takes inmat weights and applies those too
    # shows how tri data mining via inmat tris is better then IRS datamining via alienmat
    if(ncol(inmat) != ncol(alienmat)) {
        print("inmat and alienmat must have the same number of columns")
        return(-1)
    }
    cols <- colnames(inmat)
    regs <- lapply(cols, function(cl) {
                       cli <- which(colnames(inmat) == cl)
                       leftovermat <- inmat[, -cli]
                       leftoveralien <- alienmat[, -cli]
                       f <- as.formula(paste(cl, "~", paste(cols[-which(cols == cl)], collapse = " + ")))
                       weights <- summary(regsubsets(f, data = inmat))$which[, -1]
                       lapply(1:nrow(weights), function(i) {
                            w <- weights[i, ]
                            linmod <- lm(inmat[, cl] ~ leftovermat[, w])
                            coeffs <- linmod$coefficients[-1]
                            rsq <- summary(linmod)$r.squared
                            resids <- xts(as.numeric(linmod$residuals), order.by = index(inmat))
                            # now do the alienmat with the same 
                            linmodalien <- lm(alienmat[, cli] ~ leftoveralien[, w])
                            coeffsalien <- linmodalien$coefficients[-1]
                            rsqalien <- summary(linmodalien)$r.squared
                            residsalien <- xts(as.numeric(linmodalien$residuals), order.by = index(alienmat))
                            inusingalien <- leftovermat[, w] %*% coeffsalien
                            return(list(resids = resids,
                                        residsalien = residsalien,
                                        dependent = cl,
                                        dependentalien = colnames(alienmat)[cli],
                                        independent = colnames(leftovermat)[w],
                                        independentalien = colnames(leftoveralien)[w],
                                        coeffs = coeffs,
                                        coeffsalien = coeffsalien,
                                        rsq = rsq,
                                        rsqalien = rsqalien,
                                        inusingalien = inusingalien))
                       })

    })
}


dodo <- function(topng = FALSE) {
    # do all the graphics. 


    # chart x and y for 


    tri4regs <- last(tri_select(), "9 years")
    irs4regs <- last(swapdata[index(tri4regs), ], "9 years")
    regs <- all_reg_regressions(tri4regs, irs4regs, 5, 1)

    browser()


    # chart ? and ? # accuracy of eris versus tris
    data5y <- test_regressions(filter_irs(irstris, "5y", "3m"), 
                               filter_eris(eristris, "5y"), 
                               samplesize = 260)
    charts5y <- regressions_chart(data5y, 3)
    l6 <- last(charts5y, 6)
    chartsize <- c(8, 12)
    if(topng) {
        png("ccr5y.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 400)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(l6[[1]], l6[[2]], l6[[3]], l6[[4]], l6[[5]], l6[[6]],
                 ncol = 2, nrow = 3, 
                 top = textGrob("Regressions versus IRS 10y",
                                 gp = gpar(fontsize = 16)))
    if(topng) dev.off()

    data10y <- test_regressions(filter_irs(irstris, "10y", "3m"), 
                               filter_eris(eristris, "10y"), 
                               samplesize = 260)
    charts10y <- regressions_chart(data10y, 4)
    l5 <- last(charts10y, 6)
    chartsize <- c(8, 12)
    if(topng) {
        png("ccr10y.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 400)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(l5[[1]], l5[[2]], l5[[3]], l5[[4]], l5[[5]], l5[[6]],
                 ncol = 2, nrow = 3, 
                 top = textGrob("Regressions versus IRS 10y",
                                 gp = gpar(fontsize = 16)))
    if(topng) dev.off()


    chartsize <- c(9, 5)
    if(topng) {
        png("ccrsingle.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 400)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(l5[[3]], l6[[3]],
                 ncol = 2, nrow = 1,
                 top = textGrob("Sample of Eris vs IRS total return indices",
                                 gp = gpar(fontsize = 13)))
    if(topng) dev.off()




    # chart 1 and 2
    chartsize <- c(9, 5)
    edselect = "EDM23"
    erisselect = "5Y Jun 2018-2023:LIWM18"
    trid <- tri_data(edhistoric, eristris, edselect, erisselect)
    trid <- first(last(trid, "13 months"), "12 months")
    cc1 <- tri_line(trid, 1, paste("TRI performance", edselect, "vs Eris", erisselect))
    cc2 <- tri_regress(trid, 2, paste("Eris 5y high beta against EDM23"))
    if(topng) {
        png("cc1.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 600)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(cc1, cc2, nrow = 1)
    if(topng) dev.off()

    # chart 3 and 4
    data3 <- data_for_chart(start_price = 102.5, 
                            years = 2, 
                            start_rate = -0.05,
                            end_rate = 0.05, 
                            entry_rate = 0.025)

    data4 <- data_for_chart(start_price = 102.5, 
                            years = 10,
                            start_rate = -0.05,
                            end_rate = 0.05, 
                            entry_rate = 0.025)

    chartsize <- c(9, 5)
    cc1 <- linconv_chart(data3, 3, "Convexity effect - 2y instrument")
    cc2 <- linconv_chart(data4, 4, "Convexity effect - 10y instrument")
    if(topng) {
        png("cc3.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 600)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(cc1, cc2, nrow = 1)
    if(topng) dev.off()

    # chart 5 and 6
    chartsize <- c(9, 5)
    cc1 <- convexity_chart(data1, 5)
    cc2 <- convexity_spread(data1, 6)
    if(topng) {
        png("cc2.png", width = chartsize[1], height = chartsize[2], 5, units = "in", res = 600)
    } else {
        windows(chartsize[1], chartsize[2])
    }
    grid.arrange(cc1, cc2, nrow = 1)
    if(topng) dev.off()


}






