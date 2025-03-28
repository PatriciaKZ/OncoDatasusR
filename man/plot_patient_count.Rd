% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_patient_count.r
\name{plot_patient_count}
\alias{plot_patient_count}
\title{Plot Patient Count}
\usage{
plot_patient_count(
  data,
  method = c("APAC", "AIH"),
  y_type = "number",
  group = NULL,
  first_year = NULL,
  last_year = NULL,
  legend_title = NULL,
  legend_labels = NULL,
  include_label = TRUE,
  age_group = "all",
  age_type = "age_diagnosis",
  ylim = NULL,
  y_break = NULL,
  ylim_adjust = 0,
  ny = NULL,
  nx = NULL,
  line_size = 1,
  point_size = 1.5,
  text_color = "black"
)
}
\arguments{
\item{data}{DataFrame. A data frame containing the APAC data.}

\item{method}{Character vector. The method, either 'APAC' or 'AIH'.}

\item{y_type}{Character string. The type of y-axis, either 'number' or 'percentage'. Default is 'number'.}

\item{group}{Character string (optional). The grouping variable for the data.}

\item{first_year}{Integer (optional). The first year to include in the plot.}

\item{last_year}{Integer (optional). The last year to include in the plot.}

\item{legend_title}{Character string (optional). The title of the legend.}

\item{legend_labels}{Character vector (optional). The labels for the legend.}

\item{include_label}{Logical. Whether to include labels on the plot. Default is \code{TRUE}.}

\item{age_group}{Character string. Specifies the age group, options are 'all', 'adult', 'young_adult', or 'pediatric'. Default is 'all'.}

\item{age_type}{Character string. The type of age, either 'age_diagnosis' or 'age_attendance'. Default is 'age_diagnosis'.}

\item{ylim}{Numeric vector (optional). The y-axis limits.}

\item{y_break}{Numeric (optional). The interval between y-axis breaks.}

\item{ylim_adjust}{Numeric. Adjustment for the y-axis limits. Default is \code{0}.}

\item{ny}{Numeric vector (optional). Positions for y-axis labels.}

\item{nx}{Numeric vector (optional). Positions for x-axis labels.}

\item{line_size}{Numeric. The size of the line. Default is \code{1}.}

\item{point_size}{Numeric. The size of the points. Default is \code{1.5}.}

\item{text_color}{Character string. The color of the text labels. Default is 'black'.}

\item{line_color}{Character string (optional). The color of the line.}

\item{point_color}{Character string (optional). The color of the points.}
}
\value{
A \code{ggplot} object representing the plot of patient counts.
}
\description{
This function generates a plot of unique patient counts from APAC data based on the specified parameters.
}
\details{
\itemize{
\item The plot is created using \code{ggplot2} and includes customizable options for age groups, years, grouping variables, and label positions.
\item The y-axis can represent either the count (\code{number}) or percentage (\code{percentage}) of patients.
\item Labels can be positioned using \code{nx} and \code{ny} for finer control over their placement.
\item The method (\code{APAC} or \code{AIH}) determines specific settings for columns and grouping.
}
}
\examples{
\dontrun{
# Example of plotting patient count
plot <- plot_patient_count(
  data = patient_data, 
  method = "APAC", 
  y_type = "number", 
  group = "AQ_ESTADI", 
  first_year = 2008, 
  last_year = 2018, 
  include_label = TRUE, 
  ny = c(1, 0, 1, 1, 1, 1),
  ylim = c(0, 2500), 
  y_break = 500
)
print(plot)
}
}
