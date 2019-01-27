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
            p <- log_normal_transform_params(input$prior_median, input$prior_spread)
            samp <- exp(rnorm(50000, p$mu, p$sigma))
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
    
    output$prior_quant <- renderTable({
        if(is.null(prior_samp()))
            return(NULL)
        q <- t(as.matrix(quantile(prior_samp(), probs = c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95))))
        as.data.frame(q)
    })
    
    output$prior_summaries <- renderTable({
        if(is.null(prior_samp()))
            return(NULL)
        s <- prior_samp()
        data.frame(Median=median(s), Mean=mean(s), `Standard Deviation`=sd(s))
    })
    
    observeEvent(input$run,{
        if(!valid_numeric(input$prior_median) || !valid_numeric(input$prior_spread))
            return(NULL)
        id <- showNotification("Running...", duration = NULL)
        med <- input$prior_median
        sigma <- input$prior_spread
        log_normal <- input$prior_dist == "Log-Normal"
        if(log_normal){
            p <- log_normal_transform_params(input$prior_median, input$prior_spread)
            med <- p$mu
            sigma <- p$sigma
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
            sigma, 
            low, 
            high,
            log_normal)
        
        output$post_plot <- renderPlot({
            df <- rbind(data.frame(`Population Value`=theta, Distribution="Posterior"), data.frame(`Population Value`=isolate(prior_samp()), Distribution="Prior"))
            qplot(x= Population.Value, color=Distribution, data=df, geom="density",trim=TRUE) + 
                ylab("Density") + 
                scale_y_continuous(breaks=c()) + 
                theme_bw()
        })
        output$post_quant <- renderTable({
            q <- t(as.matrix(quantile(theta, probs = c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95))))
            
            as.data.frame(q)
        })
        
        output$post_summaries <- renderTable({
            if(is.null(prior_samp()))
                return(NULL)
            s <- theta
            ci <- paste0("(", format(quantile(theta, .025), digits=2), ", ",format(quantile(theta, .975), digits=2),")")
            ci2 <- paste0("(", format(quantile(theta, .05), digits=2), ", ",format(quantile(theta, .95), digits=2),")")
            data.frame(Median=median(s), Mean=mean(s), `Standard Deviation`=sd(s), `95% CI`=ci, `90% CI`=ci2, check.names = FALSE)
        })
        
        output$input <- renderPrint(isolate({
            med <- input$prior_median
            sigma <- input$prior_spread
            log_normal <- input$prior_dist == "Log-Normal"
            if(log_normal){
                p <- log_normal_transform_params(input$prior_median, input$prior_spread)
                med <- p$mu
                sigma <- p$sigma
            }
            cat("Prior: ",input$prior_dist, " : mu = ", med, " : sigma = ", sigma,"\n")
            cat("Prior Bounds: lower = ", input$prior_lower, " : upper = ", input$prior_upper,"\n")
            cat("Data:\n")
            df <- hot_to_r(input$hot)
            df <- df[!is.na(df[[1]]),]
            print(df)
            
        }))
        
        removeNotification(id)
    })

    
})
