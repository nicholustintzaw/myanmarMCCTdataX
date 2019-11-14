################################################################################
#
#' @keywords internal
#' @docType package
#' @name myanmarMCCTdata
#' @importFrom utils read.csv
#' @importFrom bbw recode
#' @importFrom nutricheckr flag_who find_child_stunting find_child_wasting
#'   find_child_underweight
#' @importFrom lubridate ymd_hms mdy ymd date %within% interval
#' @importFrom stats binom.test aggregate quantile
#' @importFrom stringr str_detect
#'
#
################################################################################
"_PACKAGE"

## quiets concerns of R CMD check re: the string that appear in recode_others
if(getRversion() >= "2.15.1")  utils::globalVariables(c("hhMembers", "hh"))
