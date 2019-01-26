library(rhandsontable)
library(ggplot2)
library(shiny)
library(rstan)

source("combine_estimates.R")

valid_numeric <- function(x) !is.null(x) && is.finite(x)


shinyServer(function(input, output) {
    
    values <- reactiveValues()
    null_df <- data.frame(
        Estimate = as.numeric(rep(NA,10)), 
        `Standard Error` = as.numeric(rep(NA,10)), 
        `Design Confidence` = as.numeric(rep(100,10)),
        check.names = FALSE)
    values[["DF"]] <- null_df
    ## Handsontable
    observe({
        if (!is.null(input$hot)) {
            values[["previous"]] <- isolate(values[["DF"]])
            DF = hot_to_r(input$hot)
        } else {
            if (is.null(values[["DF"]]))
                DF <- DF
            else
                DF <- values[["DF"]]
        }
        values[["DF"]] <- DF
    })
    
    output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF)){
            rhandsontable(DF, useTypes = FALSE)
        }
    })
    
    prior_samp <- reactiveVal()
    output$prior <- renderPlot({
        if(!valid_numeric(input$prior_median) || !valid_numeric(input$prior_spread))
            return(NULL)
        if(input$prior_dist == "Normal"){
            samp <- rnorm(50000, input$prior_median, input$prior_spread)
        }else{
            samp <- exp(rnorm(50000, log(input$prior_median), input$prior_spread))
        }
        if(valid_numeric(input$prior_lower))
            samp <- samp[samp >= input$prior_lower]
        if(valid_numeric(input$prior_upper))
            samp <- samp[samp <= input$prior_upper]
        #if(length(samp) < 2)
        #    return(NULL)
        prior_samp(samp)
        ggplot() + geom_histogram(aes(x=samp), bins=100)
    })
    
    output$prior_quant <- renderPrint({
        if(is.null(prior_samp()))
            return(NULL)
        print(quantile(prior_samp(), probs = (1:19)/20))
    })
    
    observeEvent(input$run,{
        if(!valid_numeric(input$prior_median) || !valid_numeric(input$prior_spread))
            return(NULL)
        med <- input$prior_median
        log_normal <- input$prior_dist == "Log-Normal"
        if(log_normal){
            med <- log(med)
        }
        df <- hot_to_r(input$hot)
        df <- df[!is.na(df[[1]]),]
        low <- -Inf
        high <- Inf
        if(valid_numeric(input$prior_lower))
            low <- input$prior_lower
        if(valid_numeric(input$prior_upper))
            high <- input$prior_upper
        theta <- combine_estimates_stan(
            df[,1],
            df[,2],
            df[,3] / 100, 
            med, 
            input$prior_spread, 
            low, 
            high,
            log_normal)
        output$post_plot <- renderPlot({
            hist(theta)
        })
        output$post_quant <- renderTable({
            q <- t(as.matrix(quantile(theta, probs = (1:19)/20)))
            
            as.data.frame(q)
        })
    })
    
    
})
