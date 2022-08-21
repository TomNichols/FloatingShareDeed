library(shiny)
library(ggplot2)
library(rhandsontable)
library(shinyFeedback)

shinyUI(fluidPage(
  useShinyFeedback(),
  titlePanel("Floating Share Deed of Trust"),
  br(),
  sidebarLayout(
    sidebarPanel(

      # Sale Price
      numericInput(
        "purchase_price",
        "Purchase Price",
        value=440000,
        min=0
      ),

      # Deposit Contributions
      h4("Deposits"),
      fluidRow(
        column(6,
               numericInput(
                 "deposit_a",
                 "Person A",
                 value=55000,
                 min=0
               )),
        column(6,
               numericInput(
                 "deposit_b",
                 "Person B",
                 value=15000,
                 min=0
               ))
      ),

      # Repairs
      h4("Contributions to Purchase Costs"),
      fluidRow(
        column(6,
               numericInput(
                 "costs_a",
                 "Person A",
                 value=1500,
                 min=0
               )),
        column(6,
               numericInput(
                 "costs_b",
                 "Person B",
                 value=1500,
                 min=0
               ))
      ),


      # Repairs
      h4("Contributions to Repairs"),
      fluidRow(
        column(6,
               numericInput(
                 "repairs_a",
                 "Person A",
                 value=7000,
                 min=0
               )),
        column(6,
               numericInput(
                 "repairs_b",
                 "Person B",
                 value=3000,
                 min=0
               ))
      ),

      # Monthly Mortgage Repayments (net of interest)
      h4("Monthly Mortgage Payment (Net of interest)"),
      fluidRow(
        column(6,
               numericInput(
                 "repayment_a",
                 "Person A",
                 value=700,
                 min=0
               )),
        column(6,
               numericInput(
                 "repayment_b",
                 "Person B",
                 value=500,
                 min=0
               ))
      )
    ),
    mainPanel(

      # House Price Change Assumption
      strong("House Price Changes"),
      rHandsontableOutput("price_inc_table"),
      br(),

      # Contributed Funds
      strong("Contributed Funds"),
      tableOutput("contributions_table"),
      br(),

      # Returned Funds
      strong("Returned Funds"),
      tableOutput("returns_table"),
      plotOutput("plot")

    )
  )
))
