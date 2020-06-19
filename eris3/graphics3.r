source("../../rtools/tools.r")
source("../../rtools/common.r")
library(data.table)
library(ggplot2)
library(cowplot)

histyears <- 2
maturityindex <- c(0.25, 1, 2, 3, 4, 5, 7, 10, 15, 20, 30)
maturities <- c("C", as.character(maturityindex[-1]))

g <- function() source("graphics3.r")


sofrbasis <- function(years = histyears) {
# LIBOR 3m vs SOFR basis
    tickers <- paste("USSRVL", maturities, " Curncy", sep = "")
    data <- bbdh(tickers, years)
    data <- na.locf(data)
    colnames(data) <- c(0.25, maturities[-1])
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

sofrrate <- function(years = histyears) {
	tickers <- "SOFRRATE Index"
	data <- bbdh(tickers, years)
	data <- na.locf(data)
	return(data)
}

libor3m <- function(years = histyears) {
	tickers <- "US0003M Index"
	data <- bbdh(tickers, years)
	data <- na.locf(data)
	return(data)
}

randomfixings <- function(view = F) {

	wdaylist <- function() {
	# creates a list of weekdays starting at startDate and ending at endDate
		endDate <- Sys.Date()
		startDate <- Sys.Date() - 1000
		alldays <- as.Date(startDate:endDate) # create the list of all days
		wdays <- alldays[!(weekdays(alldays) %in% c("Saturday", "Sunday"))]
		wdays <- last(wdays, 520)
		return(wdays)
	}

	years <- 2
	set.seed(50)
	xx <- sapply(1:30, function(x) rnorm(8))
	diffs <- sapply(1:30, function(x) sapply(1:30, function(y) mean(xx[, x] - xx[, y])))
	if(view) {
		sapply(1:30, function(x) sapply(1:30, function(y) {
			if((diffs[x, y] > 0.8) & (diffs[x, y] < 0.9)) {
				dev.new()
				barplot(xx[, x] - xx[, y])
				title(paste(x, y))
			}
		}))
	}
	x8 <- x8 <- cbind(xx[, 6], xx[, 26])
	x520 <- cbind(rnorm(520), rnorm(520))
	x520 <- xts(x520, order.by = wdaylist())
	c1 <- do.call(c, lapply(x8[, 1], function(x) rep(x, 65)))
	c2 <- do.call(c, lapply(x8[, 2], function(x) rep(x, 65)))
	data <- cbind(x520, c1, c2)
	return(data)
}

getall <- function(years = histyears) {
	sb <- sofrbasis(years)
	li <- liborimplied(years)
	si <- sofrirs(years)
	ai <- annualirs(years)
	sr <- sofrrate(years)
	l3 <- libor3m(years)
	return(list(sb = sb, li = li, si = si, ai = ai, sr = sr, l3 = l3))
}



# --------------- PLOT -----------------

SofrVsLiborVol <- function(sofrxts, liborxts) {
	sdates <- as.Date(as.character(index(sofrxts)))
	ldates <- as.Date(as.character(index(liborxts)))
	if(!all(sdates == ldates)) {
		flushprint("dates do not matcht")
		return(-1)
	} else {
		savg <- na.omit(rollapply(sofrxts, 65, mean)) # first 3m rolling mean
		srsd <- na.omit(rollapply(savg, 65, function(x) sd(diffret(x))))
		lrsd <- rollapply(liborxts, 65, function(x) sd(diffret(x)))
		numrows <- min(nrow(srsd), nrow(lrsd))
		df <- data.frame(date = last(ldates, numrows), 
						 sofr = as.numeric(last(sofrxts, numrows)),
						 sofr3m = as.numeric(last(savg, numrows)),
						 libor = as.numeric(last(liborxts, numrows)),
						 sofrsd = as.numeric(last(srsd, numrows)),
						 liborsd = as.numeric(last(lrsd, numrows)))
	}
	# now start plotting
	melt1 <- melt(df[, c("date", "sofr", "libor", "sofr3m")], id.vars = "date")
	colnames(melt1)[which(colnames(melt1) == "value")] <- "rate"
	colnames(melt1)[which(colnames(melt1) == "variable")] <- "index"
	p1 <- ggplot(melt1, aes(x = date, y = rate, color = index)) + geom_line(size = 1)
	p1 <- p1 + ggtitle("Outright rates")

	df2 <- df[, c("date", "sofrsd", "liborsd")]
	colnames(df2) <- c("date", "sofr3m", "libor")

	melt2 <- melt(df2, id.vars = "date")
	colnames(melt2)[which(colnames(melt2) == "value")] <- "standard_deviation"
	colnames(melt2)[which(colnames(melt2) == "variable")] <- "index"
	p2 <- ggplot(melt2, aes(x = date, y = standard_deviation, color = index)) + geom_line(size = 1)
	p2 <- p2 + ggtitle("Vol (rolling daily standard deviation of changes)")

	plot_grid(p1, p2, labels = "AUTO")
}

SofrVsIRSBasis <- function(basis) {
	sdates <- as.Date(as.character(index(na.omit(basis))))
	data <- as.numeric(na.omit(basis)[, 1])
	df <- data.frame(date = sdates, basis3m = data)
	p1 <- ggplot(df, aes(x = date, y = basis3m)) + geom_line(size = 1, color = "#9999DD")
	p1 <- p1 + ggtitle("3-month LIBOR vs SOFR forward basis")
	plot_grid(p1, labels = c("C"))
}

BasisCurve <- function(basis) {
	basis <- na.omit(na.locf(basis))
	hair(as.data.frame(basis), 
		 title = "History of SOFR vs LIBOR curve", ylab = "bps",
		 withlegend = TRUE, 
		 meancol = "green4")
}

prandomfixings <- function(data = randomfixings()) {
	frame <- cbind(as.Date(index(data)), data)
	browser()
}



plotall <- function(update_data = FALSE) {
	if (!exists("aa") | update_data) aa <<- getall()
	dev.new()
	xx <- (aa$ai - aa$si)[, -which(colnames(aa$ai) == "15")]
	xx <- (aa$ai - aa$si)[, -which(colnames(aa$ai) %in% c("4", "15"))]
	xx[xx < 0.1] <- NA
	BasisCurve(xx)
	dev.new()
	SofrVsLiborVol(aa$sr, aa$l3)
	dev.new()
	SofrVsIRSBasis(aa$sb)
	dev.new()
	xx <- randomfixings()
	prandomfixings(xx)
}








