library(shiny)
library(tidyverse)
library(plotly)
library(maps)

# In order to make the shiny app runs faster, some data has been pre-modified and
# stored separately. The complete data handling process is included in the Mapping
# with more data.rmd in the GitHub Repo.

# Load in the data -- all data sets used here are stored in GitHub
funding_sum  <- read.csv("funding_sum.csv")
funding_year <- read.csv("funding_year.csv")
state <- map_data("state")
Mapping_table_total <- read.csv("Mapping_table_total.csv")
assistance <- read.csv("FEMA.csv")
# Create hover for plotly
Mapping_table_total$hover <- with(Mapping_table_total, paste( "year:",year,"<br>","totalAmount:",total))


# Prepare the map by plotly
funding_year$hover <- with(funding_year, paste("State:", state,"<br>","Project Number:",number,"<br>")) 
project_number <- plot_geo(funding_year, locationmode = 'USA-states') 
project_number <- project_number %>% add_trace(
    locations = ~stateCode,
    type = 'choropleth',
    z = ~number,
    text = ~hover,
    colorscale = "Blues"
)
# Add title
project_number <- project_number %>% layout(
    title = 'Number of Hurricane Project from 2009 - 2018'
)



# Set up the shiny app
ui <- fluidPage(
    # Add web page title
    titlePanel(h1("Extended Mapping", 
                  style = {'background-color:hsl(90, 85%,90%);color:green; 
                    border:4px double black'})),
    mainPanel(
        tabsetPanel(
            tabPanel("Interactive Map by plotly", 
                         plotlyOutput("plotly")),
            tabPanel("Mapping by ggplot", 
                        plotOutput("ggplot")),
            tabPanel("Total Project Amount for Each State", 
                     # Generate a row with a sidebar
                     sidebarLayout(      
                         
                         # Define the sidebar with one input
                         sidebarPanel(
                             selectInput("year", "Year:", 
                                         choices = Mapping_table_total$year)
                             
                         ),
                         mainPanel(
                             plotlyOutput("fig")  
                         ))),
            tabPanel("Data about Public Assistance Per Capita",  
                     fluidRow(
                       column(4,
                       selectInput("Year",
                                   "Public assistance per capita based on Year among different states:",
                                   c("Select",
                                     unique(assistance$Year))),
                       )),
            DT::dataTableOutput("table")
                         ) 

        )
    )
)

# Define server 
server <- function(input, output) {
    output$plotly <- renderPlotly({
        project_number
    }) 
    output$ggplot <- renderPlot({
        # Prepare the map by ggplot
        ggplot() + geom_polygon(data = funding_sum, aes(x = long, y = lat, group = group,
                                                                  fill = range), 
                                          color = "grey", size = 0.2, alpha = 1.6) + 
            geom_polygon(data = state, aes(x = long, y = lat, group = group),
                         color = "black", fill = "white", size = 0.2, alpha = 0.3) +
            scale_fill_brewer(palette = "Blues") +
            ggtitle("Total Project Amount from 2009 to 2018") +
            # Center the title
            theme(plot.title = element_text(hjust = 0.5))
        
    }) 
    output$fig <- renderPlotly({
        
        fig <- plot_ly(Mapping_table_total %>% subset(Mapping_table_total$year==input$year)
                    , locationmode = 'USA-states')%>% add_trace(
                        locations = ~stateCode,
                        type='choropleth',
                        z= ~total,
                        text = ~hover,
                        colorscale="Reds"
                    )%>% layout(
                        title = input$year,geo= list(
                            scope = 'usa',
                            projection = list(type = 'albers usa'),
                            showlakes = TRUE,
                            lakecolor = toRGB('white')
                        )
                        
                    )
    })
    output$table <- DT::renderDataTable(DT::datatable({
        viewdata <- assistance
        if (input$Year != "Select") {
            viewdata <- viewdata[viewdata$Year == input$Year, ]
        }
        viewdata
    }))
    
}     
    
shinyApp(ui = ui, server = server)



