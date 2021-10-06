library(shiny)
library(dplyr)
ui <- fluidPage(
    selectInput(
        inputId = "stateName",
        label = "Select state: ",
        choices =
            c(
                "Andaman and Nicobar Islands" = "Andaman and Nicobar Islands",
                "Andhra Pradesh" = "Andhra Pradesh",
                "Arunachal Pradesh" = "Arunachal Pradesh",
                "Assam" = "Assam",
                "Bihar" = "Bihar",
                "Chandigarh" = "Chandigarh",
                "Chhattisgarh" = "Chhattisgarh",
                "Delhi" = "Delhi",
                "Goa" = "Goa",
                "Gujarat" = "Gujarat",
                "Haryana" = "Haryana",
                "Himachal Pradesh" = "Himachal Pradesh",
                "Jammu and Kashmir" = "Jammu and Kashmir",
                "Jharkhand" = "Jharkhand",
                "Karnataka" = "Karnataka",
                "Kerala" = "Kerala",
                "Ladakh" = "Ladakh",
                "Lakshadweep" = "Lakshadweep",
                "Madhya Pradesh" = "Madhya Pradesh",
                "Maharashtra" = "Maharashtra",
                "Manipur" = "Manipur",
                "Meghalaya" = "Meghalaya",
                "Mizoram" = "Mizoram",
                "Nagaland" = "Nagaland",
                "Odisha" = "Odisha",
                "Puducherry" = "Puducherry",
                "Punjab" = "Punjab",
                "Rajasthan" = "Rajasthan",
                "Sikkim" = "Sikkim",
                "Tamil Nadu" = "Tamil Nadu",
                "Telengana" = "Telengana",
                "Tripura" = "Tripura",
                "Uttar Pradesh" = "Uttar Pradesh",
                "Uttarakhand" = "Uttarakhand",
                "West Bengal" = "West Bengal"
                
            )
    ),
    selectInput(
        inputId = "monthName",
        label = "Select month: ",
        choices =
            c(
                "Jan" = "Jan",
                "Feb" = "Feb",
                "Mar" = "Mar",
                "Apr" = "Apr",
                "May" = "May",
                "Jun" = "Jun",
                "Jul" = "Jul",
                "Aug" = "Aug",
                "Sep" = "Sep",
                "Oct" = "Oct",
                "Nov" = "Nov",
                "Dec" = "Dec"
            )
    ),
    
    selectInput(
        inputId = "yearName",
        label = "Select month: ",
        choices =
            c("2020" = "2020",
              "2021" = "2021")
    ),
    
    plotOutput("confirmedActualPlot"),
    plotOutput("deathActualPlot"),
    plotOutput("cureActualPlot")
    
    
)

server <- function(input, output) {
    output$confirmedActualPlot <- renderPlot({
        df <-
            read.csv(
                "C:\\Users\\Biswadip Basu\\Documents\\R\\Covid-Dashboard\\data\\covid_19_india.csv"
            )
        filteredData <- filter(df, State == input$stateName & Month == input$monthName & Year == input$yearName)
        newFilteredData <- filteredData
        newFilteredData$Date <- as.Date(filteredData$Date, "%d-%m-%Y")
        tryCatch({
            plot(
                newFilteredData$Date,
                newFilteredData$ConfirmedActual,
                type = "l",
                xaxt = "n",
                xlab = "Date",
                ylab = "Confirmed Cases",
                main = "Month wise confirmed case plot",
                col = "#0026ff",
                lwd = 2
            )
            axis(1,
                 newFilteredData$Date,
                 format(newFilteredData$Date, "%d-%m-%Y"))
        }, warning = function(war){
            
        }, error = function(err){
            
        })
        
    })
    
    output$deathActualPlot <- renderPlot({
        df <-
            read.csv(
                "C:\\Users\\Biswadip Basu\\Documents\\R\\Covid-Dashboard\\data\\covid_19_india.csv"
            )
        filteredData <- filter(df, State == input$stateName & Month == input$monthName & Year == input$yearName)
        newFilteredData <- filteredData
        newFilteredData$Date <- as.Date(filteredData$Date, "%d-%m-%Y")
        tryCatch({
            plot(
                newFilteredData$Date,
                newFilteredData$DeathsActual,
                type = "l",
                xaxt = "n",
                xlab = "Date",
                ylab = "Confirmed Deaths",
                main = "Month wise death plot",
                col = "red",
                lwd = 2
            )
            axis(1,
                 newFilteredData$Date,
                 format(newFilteredData$Date, "%d-%m-%Y"))
        }, warning = function(war){
            
        }, error = function(err){
            
        })
        
    })
    
    output$cureActualPlot <- renderPlot({
        df <-
            read.csv(
                "C:\\Users\\Biswadip Basu\\Documents\\R\\Covid-Dashboard\\data\\covid_19_india.csv"
            )
        filteredData <- filter(df, State == input$stateName & Month == input$monthName & Year == input$yearName)
        newFilteredData <- filteredData
        newFilteredData$Date <- as.Date(filteredData$Date, "%d-%m-%Y")
        tryCatch({
            plot(
                newFilteredData$Date,
                newFilteredData$CuredActual,
                type = "l",
                xaxt = "n",
                xlab = "Date",
                ylab = "Confirmed recoveries",
                main = "Month wise recovery plot",
                col = "#00ff3c",
                lwd = 2
            )
            axis(1,
                 newFilteredData$Date,
                 format(newFilteredData$Date, "%d-%m-%Y"))
        }, warning = function(war){
            
        }, error = function(err){
            
        })
        
    })
    
    
}

shinyApp(ui = ui, server = server)