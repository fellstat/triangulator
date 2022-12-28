
# Triangulator

The Triangulator is a Shiny user interface designed to help derive consensus estimates
of a population quantity (e.g. a population size, a proportion, a mean, etc.) from
multiple empirical estimates. Underlying the application is a Bayesian multi-level model,
and the steps of the consensus process proceed as follows:

1. Gather experts and stakeholders.
2. Obtain all available empirical estimates of the population quantity.
3. Elicit from them their beliefs about the plausible values of the population quantity (i.e. the prior).
4. With experts, evaluate the methodologies used to generate each estimate and assign it a "Study Confidence" from 0 to 100%.
5. Enter the above information into the Shiny application and run the analysis to get a consensus estimate.
6. Report the consensus estimate and its credible interval. Additionally, report information in the "Data and Input Parameters" field so that the estimate is reproducible and the stakeholder decisions are fully inspectable.

Check out the manual at https://fellstat.github.io/triangulator/

## Installation

You can install the development version of triangulator from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fellstat/triangulator")
```

## Example

Launch the Shiny application with:

``` r
library(triangulator)
launch_triangulator()
```

