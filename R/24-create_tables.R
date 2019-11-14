################################################################################
#
#' Function to create indicator results tables
#'
#' This function performs an exact binomial test on the data for the indicator
#' of interest and provides an estimate of the indicator proportion and the
#' corresponding 95\% confidence interval. This assumes that the probability
#' weights of selection of an respondent are equal (given that sample has
#' been taken using probability proportional to population size, this
#' assumption holds true unless otherwise stated).
#'
#' @param df A recoded data.frame of indicators
#' @param var Indicator to create a results table for
#'
#' @return A data.frame of indicator results with 95\% confidence limits for
#'   each state in total and disaggregated by urban, rural and hard-to-reach
#'   areas.
#'
#' @examples
#' ## Recode anthro indicators
#' temp <- recode_anthro(df = create_canthro(df = childAnthro, x = anthroDF))
#'
#' create_table(df = temp, var = "global.haz")
#'
#' @export
#'
#'
#
################################################################################

create_table <- function(df, var) {
  temp <- df[ , c("geo_rural", "geo_state", var)]

  tab <- with(temp, table(geo_rural, geo_state, get(var)))

  rural.02 <- binom.test(x = tab[1, 1, 2], n = sum(tab[1, 1, 1], tab[1, 2, 1]))
  rural.03 <- binom.test(x = tab[1, 2, 2], n = sum(tab[1, 2, 1], tab[1, 2, 2]))

  urban.02 <- binom.test(x = tab[2, 1, 2], n = sum(tab[2, 1, 1], tab[2, 1, 2]))
  urban.03 <- binom.test(x = tab[2, 2, 2], n = sum(tab[2, 2, 1], tab[2, 2, 2]))

  eho.02 <- binom.test(x = tab[3, 1, 2], n = sum(tab[3, 1, 1], tab[3, 1, 2]))
  eho.03 <- binom.test(x = tab[3, 2, 2], n = sum(tab[3, 2, 1], tab[3, 2, 2]))

  tabTotal <- with(temp, table(get(var), geo_state))

  total.02 <- binom.test(x = tabTotal[2, 1], n = colSums(tabTotal)[1])
  total.03 <- binom.test(x = tabTotal[2, 2], n = colSums(tabTotal)[2])

  resultsDF <- data.frame(matrix(ncol = 7, nrow = 5))

  resultsDF[ , 1] <- c("", "Rural", "Urban", "Hard-to-reach", "Total")
  resultsDF[1, 2:7] <- c(rep("State 1", 3), rep("State 2", 3))
  resultsDF[2, 2:7] <- c(rural.02$estimate, rural.02$conf.int, rural.03$estimate, rural.03$conf.int)
  resultsDF[3, 2:7] <- c(urban.02$estimate, urban.02$conf.int, urban.03$estimate, urban.03$conf.int)
  resultsDF[4, 2:7] <- c(eho.02$estimate, eho.02$conf.int, eho.03$estimate, eho.03$conf.int)
  resultsDF[5, 2:7] <- c(total.02$estimate, total.02$conf.int, total.03$estimate, total.03$conf.int)

  names(resultsDF) <- c("", "Estimate", "95% LCL", "95% UCL", "Estimate", "95% LCL", "95% UCL")

  return(resultsDF)
}


################################################################################
#
#' Function to create indicator results tables
#'
#' @param df A recoded data.frame of indicators
#' @param ppiDF A data.frame produced when \code{recode_ppi} is applied to
#'   the \code{hh} dataset of the Myanmar MCCT Programme Evaluation
#' @param vars Indicator/s to create a results table for
#' @param digits Integer value for number of digits to round off values.
#'   Default is NULL for no rounding off.
#'
#' @return A data.frame of indicator results for each state in total and
#'   disaggregated by urban, rural and hard-to-reach areas and by wealth
#'   quintile.
#'
#' @examples
#' ## Recode wash indicators
#' washDF  <- recode_wash(df = hh)
#'
#' create_unweighted_table(df = washDF,
#'                         vars = c("summer1", "summer6", "summer5",
#'                                  "rain1", "rain6", "rain5",
#'                                  "winter1", "winter6", "winter5"))
#'
#' @export
#'
#
################################################################################

create_unweighted_table <- function(df,
                                    ppiDF = recode_ppi(df = hh),
                                    vars,
                                    digits = NULL) {
  ppiDF <- ppiDF[ , c("KEY", "ppi")]
  ## Merge df with hh to get ppi results
  x <- merge(df, ppiDF, by = "KEY")
  x <- x[x$sample_component == 1, ]
  ## Get quintiles of samples in x by state
  x <- split_to_quintiles(df = x, by = "geo_state")
  ## Aggregate variables by state and geographic strata
  y <- aggregate(x = x[ , vars],
                 by = list(x$geo_state, x$geo_rural),
                 FUN = function(x) {xx <- mean(x, na.rm = TRUE); if(is.na(xx)) xx <- 0; return(xx)})
  ## Rename variables
  names(y) <- c("state", "strata", vars)
  ##
  y$state <- ifelse(y$state == "MMR002", "Kayah", "Kayin")
  y$strata <- ifelse(y$strata == 0, "Rural",
                ifelse(y$strata == 1, "Urban", "Hard-to-reach"))
  ## Aggregate variables by state and wealth strata
  z <- aggregate(x = x[ , vars],
                 by = list(x$geo_state, x$wealthQuintile),
                 FUN = function(x) {xx <- mean(x, na.rm = TRUE); if(is.na(xx)) xx <- 0; return(xx)})
  ## Rename variables
  names(z) <- c("state", "strata", vars)
  ##
  z$state <- ifelse(z$state == "MMR002", "Kayah", "Kayin")
  #z$strata <- paste("Wealth Quintile ", z$strata, sep = "")
  z$strata[z$strata == 1] <- "Wealthiest"
  z$strata[z$strata == 2] <- "Wealthy"
  z$strata[z$strata == 3] <- "Medium"
  z$strata[z$strata == 4] <- "Poor"
  z$strata[z$strata == 5] <- "Poorest"
  ## Concatenate results
  tab <- data.frame(rbind(y, z))
  ## Round off?
  if(!is.null(digits)) {
    tab[ , vars] <- round(tab[ , vars], digits = digits)
  }
  ## Order tab
  tab <- tab[order(tab$state), ]
  ## Return
  return(tab)
}


################################################################################
#
#' Function to create indicator results tables
#'
#' @param df A recoded data.frame of indicators
#' @param vars Indicator/s to create a results table for
#' @param digits Integer value for number of digits to round off values.
#'   Default is NULL for no rounding off.
#'
#' @return A data.frame of indicator results for each state in total and
#'   disaggregated by urban, rural and hard-to-reach areas and by wealth
#'   quintile.
#'
#' @examples
#' ## Recode wash indicators
#' canthro  <- recode_anthro(df = create_canthro(df = childAnthro, x = anthroDF))
#'
#' create_anthro_table(df = canthro,
#'                     vars = c("haz", "global.haz", "moderate.haz", "severe.haz"))
#'
#' @export
#'
#
################################################################################

create_anthro_table <- function(df,
                                vars,
                                digits = NULL) {
  x <- df[df$sample_component == 1, ]
  ## Aggregate variables by state and geographic strata
  y <- aggregate(x = x[ , vars],
                 by = list(x$geo_state, x$geo_rural),
                 FUN = mean, na.rm = TRUE)
  ## Rename variables
  names(y) <- c("state", "strata", vars)
  ##
  y$state <- ifelse(y$state == "MMR002", "Kayah", "Kayin")
  y$strata <- ifelse(y$strata == 0, "Rural",
                     ifelse(y$strata == 1, "Urban", "Hard-to-reach"))
  ## Concatenate results
  tab <- y
  ## Round off?
  if(!is.null(digits)) {
    tab[ , vars] <- round(tab[ , vars], digits = digits)
  }
  ## Order tab
  tab <- tab[order(tab$state), ]
  ## Return
  return(tab)
}
