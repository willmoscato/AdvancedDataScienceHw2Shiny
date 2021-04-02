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
library(tidyverse)         # for graphing and data cleaning
library(tidymodels)        # for modeling
library(stacks)            # for stacking models
library(naniar)            # for examining missing values (NAs)
library(lubridate)         # for date manipulation
library(moderndive)        # for King County housing data
library(vip)               # for variable importance plots
library(DALEX)             # for model interpretation  
library(DALEXtra)          # for extension of DALEX
library(patchwork)         # for combining plots nicely

#library(bslib)
data("lending_club")
model <- readRDS("lending_final_stack.rds")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lending Club Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            sliderInput(inputId = "funded_amnt", 
                        label = "Funded Amount",
                        min = 1000, 
                        max = 40000, 
                        value = 40000),
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
        selectInput("term",
                    "Term",
                    choices = lending_club$term),
        selectInput("sub_grade",
                    "Sub Grade",
                    choices = lending_club$sub_grade),
        selectInput("addr_state",
                       "State",
                       choices = lending_club$addr_state),
        selectInput("verification_status",
                    "Verification Status",
                    choices = lending_club$verification_status),
        selectInput("emp_length",
                    "Employment Length",
                    choices = lending_club$emp_length),
        submitButton(text = "Create my plot!")
        ),
       
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({min <- min(lending_club$int_rate)
    max <- max(lending_club$int_rate)
    
    df <- data.frame(funded_amnt = input$"funded_amnt", 
                     annual_inc = input$"annual_inc", 
                     delinq_2yrs = input$"delinq_2years", 
                     inq_last_6mths = input$"inq_last_6mnths",
                     revol_util = input$"revol_util",
                     acc_now_delinq = input$"acc_now_delinq",
                     open_il_6m = input$"open_til_6m",
                     open_il_12m = input$"open_til_12m",
                     open_il_24m = input$"open_til_24m",
                     total_bal_il = input$"total_bal_il",
                     all_util = input$"all_util",
                     inq_fi = input$"inq_fi",
                     inq_last_12m = input$"inq_last_12m",
                     delinq_amnt = input$"delinq_amnt",
                     num_il_tl = input$"num_il_tl",
                     total_il_high_credit_limit = input$"total_il_high_credit_limit",
                     term = input$"term",
                     sub_grade = input$"sub_grade",
                     addr_state = input$"addr_state",
                     verification_status = input$"verification_status",
                     emp_length = input$"emp_length",
                     int_rate = "int_rate")
    
    obs10 <- lending_club %>% slice(12)
    
    obs4_many <- obs10 %>% 
        #there is probably a better way to do this
        sample_n(size = 50, replace = TRUE) %>% 
        select(-int_rate) %>% 
        mutate(int_rate = seq(min, max, length.out = 50))
    
    obs4_many %>% 
        select(int_rate) %>% 
        bind_cols(
            predict(model,
                    new_data = obs4_many, type = "prob")) %>% 
                ggplot(aes(x = annual_inc,
                           y = .pred_bad)) +
                geom_line() +
                labs(y = "Predicted Probability of Class Bad")})
}

# Run the application 
shinyApp(ui = ui, server = server)
