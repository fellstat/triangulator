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
        `Lower` = as.numeric(rep(NA,10)), 
        `Upper` = as.numeric(rep(NA,10)), 
        #`Standard Error` = as.numeric(rep(NA,10)), 
        `Design Confidence` = as.numeric(rep(100,10)),
        check.names = FALSE)
    values[["DF"]] <- null_df
    
    get_transform <- function(){
        if(input$transform == "None")
            tf <- function(x) x
        else if(input$transform == "Log")
            tf <- log
        else 
            tf <-  function(x) log(x/(1-x))
        tf
    }
    get_inv_transform <- function(){
        if(input$transform == "None")
            tf <- function(x) x
        else if(input$transform == "Log")
            tf <- exp
        else 
            tf <-  function(x) exp(x) / (1 + exp(x))
        tf
    }
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
        if(!valid_numeric(input$prior_median) || !valid_numeric(input$prior_q75))
            return(NULL)
        transform <- get_transform()
        inv_transform <- get_inv_transform()
        
        med <- transform(input$prior_median)
        q75 <- transform(input$prior_q75)
        prior_sd <- (q75 - med) / .674
        
        samp <- rnorm(50000, med, prior_sd)
        if(valid_numeric(input$prior_lower))
            samp <- samp[samp >= transform(input$prior_lower)]
        if(valid_numeric(input$prior_upper))
            samp <- samp[samp <= transform(input$prior_upper)]
        #if(length(samp) < 2)
        #    return(NULL)
        samp <- inv_transform(samp)
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
        if(!valid_numeric(input$prior_median) || !valid_numeric(input$prior_q75))
            return(NULL)
        id <- showNotification("Running...", duration = NULL)
        
        transform <- get_transform()
        inv_transform <- get_inv_transform()
        
        prior_med <- transform(input$prior_median)
        prior_q75 <- transform(input$prior_q75)
        prior_sd <- (prior_q75 -prior_med) / .674
        df <- hot_to_r(input$hot)
        df <- df[!is.na(df[[1]]),]
        yhat <- transform(df[,1])
        yhat_sd <- (transform(df[,3]) - transform(df[,2])) / (2 * 1.96)
        conf <- df[,4] / 100
        low <- -Inf
        high <- Inf
        if(valid_numeric(input$prior_lower))
            low <- transform(input$prior_lower)
        if(valid_numeric(input$prior_upper))
            high <- transform(input$prior_upper)
        theta <- combine_estimates_stan(
            yhat,
            yhat_sd,
            conf, 
            prior_med, 
            prior_sd, 
            low, 
            high)
        theta <- inv_transform(theta)
        
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
            cat("Transform:", input$transform,"\n")
            cat("Prior: "," : mu = ", med, " : sigma = ", sigma,"\n")
            cat("Prior Bounds: lower = ", input$prior_lower, " : upper = ", input$prior_upper,"\n")
            cat("Data:\n")
            df <- hot_to_r(input$hot)
            df <- df[!is.na(df[[1]]),]
            print(df)
            
        }))
        
        removeNotification(id)
    })

    
})
