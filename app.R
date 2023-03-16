library(shiny)
library(shinydashboard)
library(dplyr) #for data manipulation
library(lubridate)
library(shinyWidgets)
library(stringr)
library(ggplot2)
library(scales)
library(rsconnect)
#library(plotly)

# 1. Import & Load data as a corpus 
#setwd("D:/Projects/Sales Dashboard")
data<-read.csv("sales_data_sample.csv")

# Select relevant data
datapr <- data %>% select(ORDERDATE, ORDERNUMBER, ORDERLINENUMBER, COUNTRY, SALES, PRODUCTLINE, STATUS, CUSTOMERNAME)

# Preprocessing
datapr <- datapr %>%mutate(ORDERDATE = mdy_hm(ORDERDATE),ORDERDATE = as_datetime(ORDERDATE))
datapr$MONTHYEAR <- format(as.Date(datapr$ORDERDATE), "%Y-%m")


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Sales Dashboard"),
                    dashboardSidebar(
                      
                      dateRangeInput("date", "Date",
                                     start = min(datapr$ORDERDATE),
                                     end   = max(datapr$ORDERDATE)),
                      
                      shinyWidgets::pickerInput(
                        inputId  = "country",
                        label    = h4("Country"),
                        choices  = sort(unique(datapr$COUNTRY)),
                        selected = unique(datapr$COUNTRY),
                        multiple = TRUE, # Allow multiple options
                        options = list(
                          `actions-box` = TRUE,  # Note back ticks
                          size = 10,
                          `selected-text-format` = "count > 3"
                        )
                      ),
                      
                      # Picker Input Widget: PRODUCTLINE
                      shinyWidgets::pickerInput(
                        inputId  = "product",
                        label    = h4("Product"),
                        choices  = sort(unique(datapr$PRODUCTLINE)),
                        selected = unique(datapr$PRODUCTLINE),
                        multiple = TRUE, # Allow multiple options
                        options  = list(
                          `actions-box` = TRUE,  # Note back ticks
                          size = 10,
                          `selected-text-format` = "count > 3"
                        )
                      ),
                      
                      shinyWidgets::pickerInput(
                        inputId  = "status",
                        label    = h4("Status"),
                        choices  = sort(unique(datapr$STATUS)),
                        selected = unique(datapr$STATUS),
                        multiple = TRUE, # Allow multiple options
                        options  = list(
                          `actions-box` = TRUE,  # Note back ticks
                          size = 10,
                          `selected-text-format` = "count > 3"
                        )
                      ),
                      
                      shinyWidgets::pickerInput(
                        inputId  = "customer",
                        label    = h4("Customer"),
                        choices  = unique(datapr$CUSTOMERNAME) %>% sort(),
                        selected = unique(datapr$CUSTOMERNAME) %>% sort(),
                        multiple = TRUE, # Allow multiple options
                        options  = list(
                          `actions-box` = TRUE,  # Note back ticks
                          size = 10,
                          `selected-text-format` = "count > 3"
                        )
                      ),
                      br(),
                      
                      fluidRow(
                        column(4, align="center", offset = 3,
                               actionButton("apply",label = "Apply"))
                      )
                      
                      
                    ),
                    
                    dashboardBody(
                      fluidRow(
                        valueBoxOutput("orders"),
                        valueBoxOutput("sales"),
                        valueBoxOutput("shipped")
                      ),
                      fluidRow(
                        box(
                          width = "8",plotOutput("sales_revenue"), title="Sales Revenue ($)"
                          
                        ),
                        box(
                          width = "4",plotOutput("top_products"), title="Top Products by Orders"
                          
                        )
                        
                      )
                    )
                    
)

server <- function(input, output) {
  # Reactive Event: waits until a button (Apply) is clicked to run reactive code 
  pr_data <- eventReactive(
    eventExpr = input$apply, 
    
    valueExpr = {
      
      datapr %>%
        
        # Date Range filter
        filter(ORDERDATE %>% between(left = as_datetime(input$date[1]),
                                     right = as_datetime(input$date[2]))) %>%
        
        # Picker filter: Country
        filter(COUNTRY %in% input$country) %>%
        
        # Picker filter: Product Type
        filter(PRODUCTLINE %in% input$product) %>%
        
        # Checkbox filter: Status
        filter(STATUS %in% input$status)%>%
        
        # Picker filter: Customer
        filter(CUSTOMERNAME %in% input$customer)
      
      
    },
    ignoreNULL = FALSE  # Don't pass data as default: run code when app loads
  )
  
  
  output$orders <- renderValueBox({
    valueBox(
      value = pr_data()%>%summarise(orders=unique(ORDERNUMBER))%>%count(),
      subtitle = "Orders"
    )
  })
  
  output$sales <- renderValueBox({
    valueBox(
      value = pr_data()%>%summarise(sales=paste("$",round(sum(SALES)/1e6,2),"M")),
      "Sales"
    )
  })
  
  #output$customers <- renderValueBox({
  # valueBox(
  #  value = pr_data()%>%summarise(customers=unique(CUSTOMERNAME))%>%count(),
  # "Unique users"
  #)
  #})
  
  output$shipped <- renderValueBox({
    valueBox(
      value = pr_data()%>%summarise(shipped = paste(round(sum(str_detect(STATUS, "Shipped")) / 
                                                            (length(STATUS))*100,1),"%")), # Pct shipped
      "Shipped"
    )
  })
  
  
  output$sales_revenue <- renderPlot({  
    
    pr_data() %>%mutate(MONTHYEAR<-ymd(MONTHYEAR))%>%
      group_by(MONTHYEAR) %>%                             
      summarise(sum = sum(SALES)) %>%
      ggplot(aes(x=MONTHYEAR, y=sum)) +                 # plot 
      geom_bar(stat="identity", fill="blue", alpha=.6, width=.4)+ labs(x="Date", y = "Sales (USD)")+
      theme( axis.title = element_text(size = 12),axis.text = element_text(size = 10,face="bold"), 
             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_y_continuous(labels = label_comma())
    
    
    #p <- ggplot(pr_data(), aes(x=ORDERDATE, y=SALES)) + 
    # geom_line(size=1, color = 'blue')+ labs(x="Date", y = "Sales (USD)")+
    #theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))
    
  })
  
  output$top_products <- renderPlot({  
    
    pr_data() %>%
      group_by(PRODUCTLINE) %>%                              # calculate the counts
      summarize(counts = n()) %>%
      arrange(counts) %>%                                # sort by counts
      mutate(PRODUCTLINE = factor(PRODUCTLINE, PRODUCTLINE)) %>%   # reset factor
      ggplot(aes(x=PRODUCTLINE, y=counts)) +                 # plot 
      geom_bar(stat="identity", fill="seagreen3", alpha=.6, width=.4)+
      coord_flip() +
      labs(x="",y="")+
      theme_bw()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),axis.text = element_text(size = 12),axis.title.y = element_text(colour = "blue"))
    
    
    
  })
  
}

shinyApp(ui, server)

rsconnect::setAccountInfo(name='shivangiprojects',
                          token='DD53A681E8808E282FD293F221C7DF6D',
                          secret='whJ6AYlYZ7w5IxfjVBNaN8IZZKQ14Kqgra+3RYZM')

deployApp()


