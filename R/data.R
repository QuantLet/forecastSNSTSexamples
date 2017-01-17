################################################################################
#' London Housing Price Index, 1995--2015
#'
#' Contains 252 monthly index values for the years 1995--2015 of the UK Housing
#' price index for London.
#'
#' The data was downloaded from the 'UK House Price Index' website.
#' 
#' To retrieve the data: go to the `customise your search' part of the
#' `search the UK house price index' form. There select the `English region'
#' London, the period from 01-1995 to 12-2015, and obtained the `average price'
#' for `all property types'.
#'
#' @format A univariate time series with 252 observations; a \code{ts} object
#'
#' @name data-LondonHPI
#' @aliases LondonHPI
#' @docType data
#'
#' @references UK House Price Index
#' \url{http://landregistry.data.gov.uk/app/ukhpi}
#' @keywords data
#'
#' @examples
#' ## compute monthly changes of average prices
#' L <- length(LondonHPI)
#' LondonHPI_change <- LondonHPI[2:L]/LondonHPI[1:(L-1)] - 1
#' ## demean
#' LondonHPI_mean <- mean(LondonHPI_change)
#' LondonHPI_adj <- ts(LondonHPI_change - LondonHPI_mean, f = 12, start=c(1995,2))
#' op <- par(mfcol=c(1,2))
#' plot(LondonHPI, main = "", ylab = "average prices", xlab = "year")
#' plot(LondonHPI_adj, main = "", ylab = "monthly changes", xlab = "year")
#' par(op)
################################################################################
NULL

################################################################################
#' Financial Times Stock Exchange 100 Index, 01.01.2015--30.12.2016
#'
#' Share index of 100 companies listed on the London Stock Exchange with the
#' highest market capitalisation. The first series contains the opening prices,
#' the second series contains the closing prices.
#'
#' The data was downloaded from Finanzen.net on 4 November 2016.
#'
#' @format A bivariate time series with 506 observations; a \code{zoo} object
#'
#' @name data-FTSE100
#' @aliases FTSE100
#' @docType data
#'
#' @references Obtained from Finanzen.net on 12 January 2017
#' \url{http://www.finanzen.net/index/FTSE_100/Historisch}.
#' Further, the data for 18 November 2016 to 30 November 2016 was faulty and
#' therefore replaced by data obtained from Google Finance
#' \url{http://www.google.co.uk/finance/historical?q=INDEX%20FTSE}.
#' 
#' Further information on the index
#' \url{https://en.wikipedia.org/wiki/FTSE_100_Index}
#' 
#' @keywords data
#'
#' @examples
#' FTSE100_open <- FTSE100[,1]
#' FTSE100_close <- FTSE100[,2]
#' FTSE100_returns <- (FTSE100_open - FTSE100_close) / FTSE100_close
#' FTSE100_volatility <- FTSE100_returns^2 - mean(FTSE100_returns^2)
#' op <- par(mfcol=c(1,2))
#' plot(FTSE100_close, main = "", ylab="FTSE 100", xlab="year")
#' plot(FTSE100_volatility, main = "", ylab="returns (sq., ctrd.)", xlab="year")
#' par(op)
################################################################################
NULL

################################################################################
#' Daily air temperatures, collected in Hohenpeissenberg, 1985--2015
#'
#' The station of Hohenpeissenberg is located in Germany, Brandenburg.
#' Its location is 977m (elevation), 47.8009 (latitude) and 11.0109 (longitude).
#' DWD lists it with station-id 2290. Data is avaiable from 1781 onwards.
#' 
#' The observations for any 29 February were removed to have 365 observations
#' per year. 
#'
#' @format A univariate time series with 11315 observations; a \code{ts} object
#'
#' @name data-Hohenpeissenberg
#' @aliases Hohenpeissenberg
#' @docType data
#' 
#' @importFrom forecast fourier
#' @import zoo
#' 
#' @references Obtained from Deutscher Wetterdienst
#' \url{http://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html}
#' @keywords data
#'
#' @examples
#' ## Perform Fourier regression to remove trend and seasonality
#' library(forecast)
#' Y <- Hohenpeissenberg
#' tt = 1:length(Y)
#' year.lm = lm(Y ~ (tt + fourier(Y, 4)))
#' Y.mean <- ts(fitted(year.lm), f = 365, start=c(1985,1,1))
#' Y.norm = ts(Y - Y.mean, f = 365, start=c(1985,1,1))
#' op <- par(mfcol=c(1,2))
#' plot(Y, main = "", ylab="temperatures ", xlab="date")
#' lines(Y.mean, col="red")
#' plot(Y.norm, type="l", main = "", ylab="adjusted temperatures", xlab="date")
#' par(op)
################################################################################
NULL
