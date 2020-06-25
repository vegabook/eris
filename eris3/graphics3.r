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
		wdays <- last(wdays, 552)
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
	s1 <- c(rep(NA, 32), rnorm(520))
	s2 <- c(rnorm(520), rep(NA, 32))
	sdiff <- s1 - s2
	s1[!is.na(sdiff)] <- s2[!is.na(sdiff)]
	sdiff <- s1 - s2
	smean = rep(mean(na.omit(sdiff)), length(sdiff))
	x8 <- x8 <- cbind(xx[, 6], xx[, 26])
	
	l1 <- c(rep(NA, 32), do.call(c, lapply(x8[, 1], function(x) rep(x, 65))))
	l2 <- c(do.call(c, lapply(x8[, 2], function(x) rep(x, 65))), rep(NA, 32))
	libdiff <- l1 - l2
	libmean <- rep(mean(na.omit(libdiff)), length(libdiff))
	frame <- data.frame(dates = wdaylist(), Libor1 = l1, Libor2 = l2, 
					   Libor_diff = libdiff, Libor_mean = libmean,
					   SOFR1 = s1, SOFR2 = s2, SOFR_diff = sdiff, SOFR_mean = smean)
	return(frame)
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

prandomfixings <- function(frame = randomfixings()) {
	m1 <- melt(frame[, c("dates", "Libor1", "Libor2", "Libor_mean", "Libor_diff")], id.vars = "dates")
	m2 <- melt(frame[, c("dates", "SOFR1", "SOFR2", "SOFR_mean", "SOFR_diff")], id.vars = "dates")
	colorway <- c("#FF00FF", "#00FFFF", "#FFFF00", "#44FF44")
	plt1 <- ggplot(m1, aes(x = dates, y = value, colour = variable)) + geom_line(lwd = 2, alpha = 0.5)
	plt1 = plt1 + scale_colour_manual(values = colorway)
	plt1 <- plt1 + ggtitle("Two LIBOR swaps, 8 fixings, 45 days offset")
	plt2 <- ggplot(m2, aes(x = dates, y = value, colour = variable)) + geom_line(lwd = 2, alpha = 0.5) 
	plt2 = plt2 + scale_colour_manual(values = colorway) 
	plt2 <- plt2 + ggtitle("Two SOFR swaps, 520 fixings, 45 days offset")
	dev.new()
	plot(plt1, labels = "D")
	dev.new()
	plot(plt2, labels = "D")

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








