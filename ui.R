library(rhandsontable)
library(shiny)
library(shinycssloaders)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Consensus Estimate Calculator"),
    
    tabsetPanel(
        tabPanel(
            "Enter Estimates",
            br(),
            rHandsontableOutput("hot"),
            p("Estimate : An estimate from a study"),
            p("Standard Error : The standard error of the estimate. Standard error can be calculated from a 95% confidence interval as (upper - lower) / (2 * 1.96)."),
            p("Design Confidence : Expert confidence in the design / implementation of the study. This scales the standard error such that a value of 50 will double the standard error.")
        ),
        tabPanel(
            "Define Prior Beliefs",
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        "prior_dist",
                        "Distribution",
                        c("Normal","Log-Normal"),
                        selected="Log-Normal"
                    ),
                    numericInput("prior_median","Median",NA),
                    numericInput("prior_spread","Standard Deviation", NA),
                    numericInput("prior_lower","Lower Bound",-Inf),
                    numericInput("prior_upper","Upper Bound",Inf)
                ),
                mainPanel(
                    plotOutput("prior"),
                    p("Summaries"),
                    tableOutput("prior_summaries"),
                    p("Quantiles:"),
                    tableOutput("prior_quant")
                )
            )
        ),
        tabPanel(
            "Synthesis",
            sidebarLayout(
                sidebarPanel(
                    actionButton("run","Run")
                ),
                mainPanel(
                    p("Consensus Estimate:"),
                    tableOutput("post_summaries"),
                    p("Posterior:"),
                    plotOutput("post_plot"),
                    p("Posterior Quantiles:"),
                    tableOutput("post_quant"),
                    hr(),
                    p("Data and Input Parameters:"),
                    verbatimTextOutput("input")
                )
            )
        )
    )
))
