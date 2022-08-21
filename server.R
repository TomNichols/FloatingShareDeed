library(shiny)
library(ggplot2)
library(rhandsontable)
library(shinyFeedback)

shinyServer(function(input, output, session){

  # Initiate house price change table
  output$price_inc_table <- renderRHandsontable({
    rhandsontable(data.frame(Year_1 = rep(0.03,1),
                             Year_2 = rep(0.03,1),
                             Year_3 = rep(0.03,1),
                             Year_4 = rep(0.03,1),
                             Year_5 = rep(0.03,1),
                             Year_6 = rep(0.03,1),
                             Year_7 = rep(0.03,1),
                             Year_8 = rep(0.03,1),
                             Year_9 = rep(0.03,1),
                             Year_10 = rep(0.03,1),
                             stringsAsFactors = FALSE))
  })

  # Set list of input elements to be reactive to
  toListen <- reactive({
    list(input$purchase_price,
         input$deposit_a,
         input$deposit_b,
         input$costs_a,
         input$costs_b,
         input$repayment_a,
         input$repayment_b,
         input$repairs_a,
         input$repairs_b,
         input$price_inc_table)
  })

  observeEvent(toListen(),{

    # Validation of deposit amounts
    if(!is.na(input$purchase_price) &
       !is.na(input$deposit_a) &
       !is.na(input$deposit_b) &
       input$deposit_a > input$purchase_price - input$deposit_b){
      showFeedbackWarning(inputId = "deposit_a", text = "Total deposit more than purchase price!")
    }else{
      hideFeedback(inputId = "deposit_a")
    }
    if(!is.na(input$purchase_price) &
       !is.na(input$deposit_a) &
       !is.na(input$deposit_b) &
       input$deposit_b > input$purchase_price - input$deposit_a){
      showFeedbackWarning(inputId = "deposit_b", text = "Total deposit more than purchase price!")
    }else{
      hideFeedback(inputId = "deposit_b")
    }

    # Get data from the price change table
    price_change_df <- hot_to_r(input$price_inc_table)
    price_change_list <- c(0,as.numeric(price_change_df[1,]))

    # Convert to cumulative change
    price_change_list <- cumprod(1+price_change_list)-1

    # Vector of years to calculate for
    year <- c(0:10)

    # Calculate total deposit & initial mortgage debt
    total_deposit <- input$deposit_a + input$deposit_b
    mortgage_debt <- input$purchase_price - total_deposit

    # Calculate the projected sale price
    sale_price <- input$purchase_price * (1 + price_change_list)

    # Calculate the total amount of mortgage repayments made to date
    total_repayment <- year * (input$repayment_a + input$repayment_b) * 12

    # Calculate the amount of funds returned on sale
    debt_redemption <- mortgage_debt - total_repayment
    returned_funds <- sale_price - debt_redemption

    # Get the total contributions
    total_contribution_a <- input$deposit_a + input$costs_a + input$repairs_a + input$repayment_a * 12 * year
    total_contribution_b <- input$deposit_b + input$costs_b + input$repairs_b + input$repayment_b * 12 * year

    # Calculate the shares
    share_a <- total_contribution_a/(total_contribution_a + total_contribution_b)
    share_b <- total_contribution_b/(total_contribution_a + total_contribution_b)

    # Get returned funds
    return_a <- share_a * returned_funds
    return_b <- share_b * returned_funds

    # Dataframe for stacked bar chart
    year <- c(0:10,0:10)
    sale_proceeds <- c(return_a,return_b)
    person <- c(rep("A",11),rep("B",11))
    df <- data.frame(year = year,
                     sale_proceeds = sale_proceeds,
                     person = person)

    # Calculating table of contributed funds
    total_contribution <- total_contribution_a + total_contribution_b
    contributions_df <- data.frame(Person = c("Total","A","B"),
                                   Year_0 = c(total_contribution[1],total_contribution_a[1],total_contribution_b[1]),
                                   Year_1 = c(total_contribution[2],total_contribution_a[2],total_contribution_b[2]),
                                   Year_2 = c(total_contribution[3],total_contribution_a[3],total_contribution_b[3]),
                                   Year_3 = c(total_contribution[4],total_contribution_a[4],total_contribution_b[4]),
                                   Year_4 = c(total_contribution[5],total_contribution_a[5],total_contribution_b[5]),
                                   Year_5 = c(total_contribution[6],total_contribution_a[6],total_contribution_b[6]),
                                   Year_6 = c(total_contribution[7],total_contribution_a[7],total_contribution_b[7]),
                                   Year_7 = c(total_contribution[8],total_contribution_a[8],total_contribution_b[8]),
                                   Year_8 = c(total_contribution[9],total_contribution_a[9],total_contribution_b[9]),
                                   Year_9 = c(total_contribution[10],total_contribution_a[10],total_contribution_b[10]),
                                   Year_10 = c(total_contribution[11],total_contribution_a[11],total_contribution_b[11]),
                                   stringsAsFactors = FALSE)

    # Calculating returned funds table
    returns_a <- df[df$person == "A",]$sale_proceeds
    returns_b <- df[df$person == "B",]$sale_proceeds
    returns_total <- returns_a + returns_b
    returns_df <- data.frame(Person = c("Total","A","B"),
                             Year_0 = c(returns_total[1],returns_a[1],returns_b[1]),
                             Year_1 = c(returns_total[2],returns_a[2],returns_b[2]),
                             Year_2 = c(returns_total[3],returns_a[3],returns_b[3]),
                             Year_3 = c(returns_total[4],returns_a[4],returns_b[4]),
                             Year_4 = c(returns_total[5],returns_a[5],returns_b[5]),
                             Year_5 = c(returns_total[6],returns_a[6],returns_b[6]),
                             Year_6 = c(returns_total[7],returns_a[7],returns_b[7]),
                             Year_7 = c(returns_total[8],returns_a[8],returns_b[8]),
                             Year_8 = c(returns_total[9],returns_a[9],returns_b[9]),
                             Year_9 = c(returns_total[10],returns_a[10],returns_b[10]),
                             Year_10 = c(returns_total[11],returns_a[11],returns_b[11]),
                             stringsAsFactors = FALSE)

    # Render outputs
    output$plot <- renderPlot({
      ggplot(df, aes(x = year, y = sale_proceeds, fill = person)) +
        geom_bar(stat = "identity")
    })

    output$returns_table <- renderTable({
      returns_df
    })

    output$contributions_table <- renderTable({
      contributions_df
    })

  })
})
