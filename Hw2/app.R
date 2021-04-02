#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(tidymodels)
library(tidyverse)
#library(bslib)

model <- readRDS("lending_final_stack.rds")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lending Club Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("var",
                        "Variable for CP:",
                        choices = list(`Funded Amount`= "funded_amnt",
                                        `Interest Rate` = "int_rate",
                                       `Annual Income` = "annual_inc",
                                       `Deliquent after 2 years` = "delinq_2yrs",
                                       `Inquiries Last 6 Month` = "inq_last_6mnths",
                                       `Revolving Utilization Rate` = "revol_util",
                                       `Number of Accounts now Deliquent` = "acc_now_delinq",
                                       `Open Installments in Last 6 months` = "open_il_6m",
                                       `Open Installments in Last 12 months` = "open_il_12m",
                                       `Open Installments in Last 24 months` = "open_il_24m",
                                       `Total Balance of Installments` = "total_bal_il",
                                       `Balance to Credit Limit` = "all_util",
                                       `Personal Finance Inquires`= "inq_fi",
                                       `Credit Inquiries Last 12 Months` = "inq_last_12m",
                                       `Past Due Amount Owed Deliquent Accounts` = 'delinq_amnt',
                                       `Number of Installment Accounts` = "num_il_tl",
                                       `Total Installment Credit Limit` = "total_il_high_credit_limit")),
           
            sliderInput(inputId = "funded_amnt", 
                        label = "Funded Amount",
                        min = 1000, 
                        max = 40000, 
                        value = 40000),
            sliderInput(inputId = "int_rate", 
                        label = "Interest Rate",
                        min = 5.32, 
                        max = 28.99, 
                        value = 28.99),
            sliderInput(inputId = "annual_inc", 
                        label = "Annual Income",
                        min = 0, 
                        max = 960000, 
                        value = 960000),
        sliderInput(inputId = "delinq_2years", 
                    label = "Deliquent after 2 Years",
                    min = 0, 
                    max = 22, 
                    value = 22),
        sliderInput(inputId = "inq_last_6mnths", 
                    label = "Inquires in the last  6 months",
                    min = 0, 
                    max = 5, 
                    value = 5),
        sliderInput(inputId = "revol_util", 
                    label = "Revolving Uitilization Rate",
                    min = 0, 
                    max = 144.3, 
                    value = 144.3),
        sliderInput(inputId = "acc_now_delinq", 
                    label = "Number of Accounts Currently Deliquent",
                    min = 0, 
                    max = 2, 
                    value = 2),
        sliderInput(inputId = "open_til_6m", 
                    label = "Open Installments last 6 months",
                    min = 0, 
                    max = 28, 
                    value = 28),
        sliderInput(inputId = "open_til_12m", 
                    label = "Open Installments last 12 months",
                    min = 0, 
                    max = 20, 
                    value = 20),
        sliderInput(inputId = "open_til_24m", 
                    label = "Open Installments last 24 months",
                    min = 0, 
                    max = 30, 
                    value = 30),
        sliderInput(inputId = "total_bal_il", 
                    label = "Total Balance of Installments",
                    min = 0, 
                    max = 566851, 
                    value = 566851),
        sliderInput(inputId = "all_util", 
                    label = "Balance to Credit Limit",
                    min = 0, 
                    max = 198, 
                    value = 198),
        sliderInput(inputId = "inq_fi", 
                    label = "Personal Finance Inquiries",
                    min = 0, 
                    max = 15, 
                    value = 15),
        sliderInput(inputId = "inq_last_12m", 
                    label = "Credit Inquiries last 12 months",
                    min = 0, 
                    max = 32, 
                    value = 32),
        sliderInput(inputId = "delinq_amnt", 
                    label = "Past Due on Deliquent Accounts",
                    min = 0, 
                    max = 31405, 
                    value = 31405),
        sliderInput(inputId = "num_il_tl", 
                    label = "Number of Installment Accounts",
                    min = 0, 
                    max = 82, 
                    value = 82),
        sliderInput(inputId = "total_il_high_credit_limit", 
                    label = "Total Installment Credit Limit",
                    min = 0, 
                    max = 554119, 
                    value = 554119),
        ),
       
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({obs <- lending_training %>% slice(4)
    obs %>% 
        select(annual_inc) %>% 
        bind_cols(
            predict(model,
                    new_data = obs, type = "prob")
        ) %>% 
        ggplot(aes(x = annual_inc,
                   y = .pred_bad)) +
        geom_line() +
        labs(y = "Predicted Probability of Class Bad")})
}

# Run the application 
shinyApp(ui = ui, server = server)
