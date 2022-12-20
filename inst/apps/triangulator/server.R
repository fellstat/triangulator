library(rhandsontable)
library(ggplot2)
library(shiny)
library(rstan)
library(shinyhelper)
library(patchwork)
library(triangulator)

valid_numeric <- function(x) !is.null(x) && is.finite(x)


shinyServer(function(input, output) {

    observe_helpers() #help files

    values <- reactiveValues()
    empty_input_table <- function(nr){
        data.frame(
            Name = paste0("Source ",1:nr),
            Estimate = as.numeric(rep(NA,nr)),
            `Lower` = as.numeric(rep(NA,nr)),
            `Upper` = as.numeric(rep(NA,nr)),
            `Design Confidence` = as.numeric(rep(100,nr)),
            check.names = FALSE)
    }
    input_table_rows <- 5
    values[["DF"]] <- empty_input_table(input_table_rows)

    observeEvent(input$more,{
        print("more")
        input_table_rows <<- input_table_rows + 5
        values[["DF"]] <- empty_input_table(input_table_rows)
    })


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
        df <- df[!is.na(df$Estimate),]
        yhat <- transform(df$Estimate)
        yhat_sd <- (transform(df$Upper) - transform(df$Lower)) / (2 * 1.96)
        conf <- df$`Design Confidence` / 100
        low <- -Inf
        high <- Inf
        if(valid_numeric(input$prior_lower))
            low <- transform(input$prior_lower)
        if(valid_numeric(input$prior_upper))
            high <- transform(input$prior_upper)
        multi <- input$multi
        post <- combine_estimates_stan(
            yhat,
            yhat_sd,
            conf,
            prior_med,
            prior_sd,
            low,
            high,
            multi,
            iter=10000,
            thin=5)
        theta <- post$theta
        theta <- inv_transform(theta)
        Rsq <- rsq_lambda(yhat,yhat_sd/conf, post$theta, post$tau)$Rsq

        output$pooling <- renderText({
            paste0(
                "Percent of Estimate Variability Attributable to Unaccounted-for Study Bias: ",
                round(max(100*Rsq,0)),
                "%"
                )
        })

        output$post_plot <- renderPlot({
            df2 <- rbind(
                data.frame(`Population Value`=theta, Distribution="Posterior"),
                data.frame(`Population Value`=isolate(prior_samp()), Distribution="Prior")
            )
            p1 <- qplot(x= Population.Value, color=Distribution, data=df2, geom="density",trim=TRUE) +
                ylab("Density") +
                theme_bw() +
                theme(legend.position = "bottom")
            prior <- isolate(prior_samp())
            conf <- df$`Design Confidence`/100
            lsc <- transform(df$Estimate) - (transform(df$Estimate) - transform(df$Lower)) / conf
            usc <- transform(df$Estimate) + (transform(df$Upper) - transform(df$Estimate)) / conf

            forest_df <- data.frame(Name=factor(c('Prior',df$Name,'Consensus'),levels=rev(c('Prior',df$Name,'Consensus'))),
                                    Estimate = c(median(prior),df$Estimate,median(theta)),
                                    Lower = c(quantile(prior,c(0.025,0.975))[['2.5%']],df$Lower,quantile(theta,c(0.025,0.975))[['2.5%']]),
                                    Upper= c(quantile(prior,c(0.025,0.975))[['97.5%']],df$Upper,quantile(theta,c(0.025,0.975))[['97.5%']]),
                                    Type=c('#00BFC4',rep("#000000",nrow(df)),'#F8766D'),
                                    Lower_scaled = c(NA, inv_transform(lsc), NA),
                                    Upper_scaled = c(NA, inv_transform(usc), NA))
            p <- ggplot(data=forest_df,aes(x=Estimate,y=Name,color=Type)) +
              geom_pointrange(aes(xmin=Lower,xmax=Upper),shape=15,size=1) +
              geom_linerange(aes(xmin=Lower_scaled,xmax=Upper_scaled,linetype='dotted'),linewidth=1) +
              scale_color_identity() +
              scale_linetype_identity(labels="CI Extended by Study Confidence", guide=guide_legend(title="")) +
              labs(x='Population',y='',size=12) +
              theme_bw() +
              theme(legend.position="bottom", text = element_text(size = 12))

            if(input$transform == "Log"){
              p1 <- p1 + scale_x_log10(labels = scales::label_comma())
              p <- p + scale_x_log10(labels = scales::label_comma())
            }else if(input$transform == "Logit"){
              p1 <- p1 + scale_x_continuous(trans="logit", labels = scales::label_comma())
              p <- p + scale_x_continuous(trans="logit", labels = scales::label_comma())
            }
            (p / p1) + plot_layout(heights = c(2,1))
        },
          height = 450
        )
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
            cat("Transform:", input$transform,"\n")
            cat("Multi:", input$multi, "\n")
            cat("Prior: "," : mu = ", med, " : q75 = ", input$prior_q75, "\n")
            cat("Prior Bounds: lower = ", input$prior_lower, " : upper = ", input$prior_upper,"\n")
            cat("Data:\n")
            df <- hot_to_r(input$hot)
            df <- df[!is.na(df$Estimate),]
            print(df)

        }))

        removeNotification(id)
    })


})
