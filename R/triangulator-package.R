#' Derive a consensus estimate from multiple, possibly contradictory, empirical estimates.
#'
#' @description The Triangulator is a Shiny user interface designed to help derive consensus estimates of a population quantity (e.g. a population size, a proportion, a mean, etc.) from multiple empirical estimates.
#'
#' @docType package
#' @name triangulator-package
#' @aliases triangulator
#' @useDynLib triangulator, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom stats var
#' @importFrom RcppParallel CxxFlags
#' @importFrom ggplot2 ggplot
#' @importFrom patchwork area
#' @importFrom rhandsontable hot_col
#' @importFrom rstantools init_cpp
#' @importFrom shinyhelper helper
#'
#'
NULL

# roxygen2::roxygenise()

# Silence R CMD CHECK because it doesn't see the shiny app
RcppParallel::CxxFlags
ggplot2::ggplot
patchwork::area
rhandsontable::hot_col
rstantools::init_cpp
shinyhelper::helper
