#' format_for_regression
#'
#' \code{format_for_regression} takes data and prepares it for PRM regression
#' @param df RAM data
#' @param DependentVariable the left hand variable
#' @param CatchVariables vector of catch related variables
#' @param CatchLags number of years of catch lags to create
#' @param IsLog is the dependent variable in log space
#'
#' @return formatted data frame
#' @export

format_for_regression <- function(df) {

## Create primary predictor variables
df <- df %>%
  filter(is.na(TCbest) == F) %>%
  group_by(stockid) %>%
  mutate(b_div_bmsy = log(BdivBmsypref), # adjust to take isLog argument using ifelse
         max_catch = max(TCbest, na.rm = T),
         years_back     = rev(1:length(TCbest)),
         scaled_catch = TCbest / max(TCbest, na.rm = T),
         scaled_catch_1 = lag(scaled_catch, n = 1), # adjust to use a function to mutate the number of catch lags
         scaled_catch_2 = lag(scaled_catch, n = 2),
         scaled_catch_3 = lag(scaled_catch, n = 3),
         scaled_catch_4 = lag(scaled_catch, n = 4),
         mean_scaled_catch = mean(scaled_catch, na.rm = T),
         rolling_catch_ratio = TCbest / cummax(TCbest),
         yrs_to_max_catch    = year[match(max_catch, TCbest)] - min(year)) %>%
  ungroup()


## Calculate catch slope
slope_window <- 1:6 # scaled catch window to include in regression adjust to set slope window as parameter
catch_slope <- df %>%
  split(.$stockid) %>%
  map_df(~ lm(scaled_catch[1:6] ~  slope_window, na.action='na.omit', data = . )$coefficients['slope_window']) %>%
  gather() %>%
  rename(stockid = key,
         initial_scaled_catch_slope = value)

## join with RAM
df <- df %>%
  left_join(catch_slope)

return(df)
}
