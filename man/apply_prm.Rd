% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_prm.R
\name{apply_prm}
\alias{apply_prm}
\title{apply_prm}
\usage{
apply_prm(dat, reg, CatchLags = 4, LifeHistoryVars = c("MaxLength",
  "AgeMat", "VonBertK", "Temp", "SpeciesCat", "SpeciesCatName", "b_to_k_ratio"),
  IdVar = "IdOrig", CatchVariables = c("YearsBack", "ScaledCatch",
  paste("ScaledCatch", 1:CatchLags, "Back", sep = ""), "MaxCatch",
  "TimeToMaxCatch", "InitialScaledCatchSlope", "MeanScaledCatch",
  "CatchToRollingMax"), min_catch_years = 10)
}
\arguments{
\item{dat}{data frame}

\item{reg}{the regression model being used}

\item{CatchLags}{number of years of catch lags}

\item{LifeHistoryVars}{life history variables needed}

\item{IdVar}{the variable marking id}

\item{CatchVariables}{catch related variables}

\item{min_catch_years}{minimum number of catch years}
}
\value{
data with PRM predicted log BvBmsy
}
\description{
\code{apply_prm} applied panel regression model to new data
}

