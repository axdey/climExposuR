% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4.2-create-lt-cutoff-vars-harmonic.R
\name{seasonal_cycle}
\alias{seasonal_cycle}
\title{Calculate Seasonal Cycles from daily weather data}
\usage{
seasonal_cycle(df, .cols, method = "harmonic_quantile_regression", ...)
}
\arguments{
\item{df}{a data.frame() of daily weather observations, with the first column
containing DATE or POSIXct objects and the remaining columns containing the
weather obs (e.g. maximum temperature, precipitation).

Remove all rows with NAs before calculating. I will fix bad behaviour on my
end}

\item{.cols}{a character vector listing variables for which we want to calculate
the seasonal cycle}

\item{method}{string of the particular method used to calculate seasonal cycles;
currently, "harmonic_quantile_regression" and "harmonic_ols_regression" are
supported}

\item{...}{extra arguments passed to the chosen method; type
?harmonic_quantile_regression() or ?harmonic_ols_regression() for details}
}
\value{
a data.frame() with the date, the chosen variables, and their annual
cycles
}
\description{
Calculate Seasonal Cycles from daily weather data
}
\examples{
test_df <- data.frame(date = seq.Date(from = lubridate::ymd("1999-01-01"),
to = lubridate::ymd("2000-12-31"), by = 1)) \%>\%
  dplyr::mutate(tmin = 12 + 8 * sin(-pi/2 + 2*pi*(lubridate::yday(date) + 20)/365.25) +
  stats::rnorm(n = dplyr::n(),
  mean = 0,
  sd = 1.5 * (1 - 0.6*sin(-pi/2 + 2*pi*lubridate::yday(date)/365.25))),
   tmax = pmax(tmin + 2, 22 + 6 * sin(-pi/2 + 2*pi*(lubridate::yday(date) + 20)/365.25) +
   stats::rnorm(n = dplyr::n(), mean = 0,
   sd = 1 * (1 - 0.6*sin(-pi/2 + 2*pi*lubridate::yday(date)/365.25))))) \%>\%
   stats::na.omit()

temp_out <- seasonal_cycle(df = test_df,
.cols = c("tmax", "tmin"),
method = "harmonic_ols_regression",
n_harmonics = 4)

ggplot2::ggplot(temp_out \%>\%
#dplyr::select(date, tidyselect::starts_with("tmax")) \%>\%
tidyr::pivot_longer(-date, values_to = "measurement", names_to = "var"),
ggplot2::aes(x = date, y = measurement, group = var, linetype = var, color = var)) +
ggplot2::geom_path() +
ggplot2::scale_color_manual(values = c("red", "red", "blue", "blue")) +
ggplot2::scale_shape_manual(values = c(1, 1, 1, 1)) +
ggplot2::scale_linetype_manual(values = c(3, 1, 3, 1))

temp_out <- seasonal_cycle(df = test_df,
.cols = c("tmax", "tmin"),
method = "harmonic_quantile_regression",
quantile = c(0.05,0.5,0.95), n_harmonics = 4)

}
