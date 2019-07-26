#
# Shiny web app for PAKKA shopping-list data analsis. 
# Developed By: Naveen Prasad Dutta 
# Associate Product Manager (Pakka Mobile App)
# Delivery Date: 26th July 2019
#

library(shiny)
library(tidyverse)
library(stringr)
library(magrittr)

source("import.r")
source("transform.r")
# source("plot.r")

# Import and join to create raw dbs--------------------------------------------------

UserCountComparison_RawDb <- JoinTables_ToCreate_UserCountComparison_RawDb()
ListCountComparison_RawDb <- JoinTables_ToCreate_ListCountComparison_RawDb()
# MutateColumnNames <- c("U1vsU2", "D1vsD2", "Ownership_Count_Type", "Size_Count_Type")

# UI design (start)--------------------------------------------------------------

ui <- fluidPage(
   
   titlePanel("Shopping-List Analysis"),
   
   sidebarLayout(
     
## Input UI ==============================================================  

      sidebarPanel(
        
        img(src = "logo.png", height = 40, width = 125),
        
        dateRangeInput("current_duration", h3("Current Duration"), format = "dd-mm-yyyy"),
        
        dateRangeInput("previous_duration", h3("Previous Duration"),format = "dd-mm-yyyy"),
        
        selectInput("graph_type", h3("Graph Type"), 
                    choices = list("Simple bar-graph" = "a", 
                                   "Grouped bar-graph" = "b",
                                   "Segmented bar-graph" = "c"), selected = "a"),
        
        radioButtons("user_type", h3("User Type"), 
                    choices = list("All Users" = "d", 
                                   "New Users" = "e",
                                   "Old Users" = "f"), selected = "d")
      ),
## Output UI ==============================================================   

      mainPanel(
        tabsetPanel(
          tabPanel("Compare user count",
                   fluidRow(
                     h3("Dashboard to compare User Count-" ),
                     column(6,
                            wellPanel(
                              h4("By old vs new users:"),
                              plotOutput("user_comparison_plot_by_user_count")
                            )
                     ),
                     
                     column(6,
                            wellPanel(
                              h4("In old vs new built:"),
                              plotOutput("built_comparison_plot_by_user_count")
                            )
                     )
                    )
          ),
          tabPanel("Compare list count",
                   fluidRow(
                     h3("Dashboard to compare List Count-" ),
                     column(6,
                            wellPanel(
                              h4("By old vs new users:"),
                              plotOutput("user_comparison_plot_by_list_count")
                            )
                     ),
                     
                     column(6,
                            wellPanel(
                              h4("In old vs new built:"),
                              plotOutput("built_comparison_plot_by_list_count")
                            )
                     )
                   )
          )
        )
      )
# UI design (end) --------------------------------------------------------------

   )
)

# server logic ----------------------------------------------------------------------

# CAUTION: Use these column names while mutating:
# U1vsU2 means NewUser vs OldUser.
# D1vsD2 means CurrentDuration vs PreviousDuration.
# Ownership_Count_Type means Few/Some/Many. 
# Size_Count_Type means Small/Medium/Large.

server <- function(input, output) {
   
## Create the transformed Dbs ======================================================
  
  TransformedDb_ForUserCountComparison_ByUserType <- reactive ({
   
    Transform_UserCountComparisonRawDb_ByUserType(RawDb = UserCountComparison_RawDb, 
                                                  Duration_1 = input$current_duration)
    
  })
  
  TransformedDb_ForUserCountComparison_ByBuiltType <- reactive({
    
    Transform_UserCountComparisonRawDb_ByBuiltType(RawDb = UserCountComparison_RawDb,
                                                   Duration_1 = input$current_duration, 
                                                   Duration_2 = input$previous_duration,
                                                   UserSelected = input$user_type)
  })  
  
  TransformedDb_ForListCountComparison_ByUserType <- reactive({ 
    
    Transform_ListCountComparisonRawDb_ByUserType(RawDb = ListCountComparison_RawDb, 
                                                  Duration_1 = input$current_duration)
  })
  
  TransformedDb_ForListCountComparison_ByBuiltType <- reactive({
    
    Transform_ListCountComparisonRawDb_ByBuiltType(RawDb = ListCountComparison_RawDb,
                                                   Duration_1 = input$current_duration, 
                                                   Duration_2 = input$previous_duration,
                                                   UserSelected = input$user_type)
  })
  
## Plot the output Graphs ==============================================================
  
  output$user_comparison_plot_by_user_count <- renderPlot({
   
     # PlotShoppingListData(TransformedDb = TransformedDb_ForUserCountComparison_ByUserType(), 
     #                     x_axis = MutateColumnNames[1], 
     #                     GroupBy = MutateColumnNames[3],
     #                     PlotType = input$graph_type)
    
    if(input$graph_type == "a") {    
      
      ggplot(data = TransformedDb_ForUserCountComparison_ByUserType()) + 
        geom_bar(mapping = aes(x = U1vsU2))
      
    } else if (input$graph_type == "b") { 
      ggplot(data = TransformedDb_ForUserCountComparison_ByUserType()) + 
        geom_bar(mapping = aes(x = U1vsU2, fill = Ownership_Count_Type), 
                 position = "dodge")
      
    } else { 
      ggplot(data = TransformedDb_ForUserCountComparison_ByUserType()) + 
        geom_bar(mapping = aes(x = U1vsU2, fill = Ownership_Count_Type), 
                 position = "fill")
    }
           
   })
   
  output$built_comparison_plot_by_user_count <- renderPlot({
     
     # PlotShoppingListData(TransformedDb = TransformedDb_ForUserCountComparison_ByBuiltType(), 
     #                     x_axis = MutateColumnNames[2], 
     #                     GroupBy = MutateColumnNames[3],
     #                     PlotType = input$graph_type)
     if(input$graph_type == "a") {    
       
       ggplot(data = TransformedDb_ForUserCountComparison_ByBuiltType()) + 
         geom_bar(mapping = aes(x = D1vsD2))
       
     } else if (input$graph_type == "b") { 
       ggplot(data = TransformedDb_ForUserCountComparison_ByBuiltType()) + 
         geom_bar(mapping = aes(x = D1vsD2, fill = Ownership_Count_Type), 
                  position = "dodge")
       
     } else { 
       ggplot(data = TransformedDb_ForUserCountComparison_ByBuiltType()) + 
         geom_bar(mapping = aes(x = D1vsD2, fill = Ownership_Count_Type), 
                  position = "fill")
     }
   })
   
  output$user_comparison_plot_by_list_count <- renderPlot({
     
     # PlotShoppingListData(TransformedDb = TransformedDb_ForListCountComparison_ByUserType(), 
     #                     x_axis = MutateColumnNames[1], 
     #                     GroupBy = MutateColumnNames[4],
     #                     PlotType = input$graph_type)
     if(input$graph_type == "a") {    
       
       ggplot(data = TransformedDb_ForListCountComparison_ByUserType()) + 
         geom_bar(mapping = aes(x = U1vsU2))
       
     } else if (input$graph_type == "b") { 
       ggplot(data = TransformedDb_ForListCountComparison_ByUserType()) + 
         geom_bar(mapping = aes(x = U1vsU2, fill = Size_Count_Type), 
                  position = "dodge")
       
     } else { 
       ggplot(data = TransformedDb_ForListCountComparison_ByUserType()) + 
         geom_bar(mapping = aes(x = U1vsU2, fill = Size_Count_Type), 
                  position = "fill")
     }
   })
   
  output$built_comparison_plot_by_list_count <- renderPlot({
     
     # PlotShoppingListData(TransformedDb = TransformedDb_ForListCountComparison_ByBuiltType(), 
     #                     x_axis = MutateColumnNames[2], 
     #                     GroupBy = MutateColumnNames[4],
     #                     PlotType = input$graph_type)
     if(input$graph_type == "a") {    
       
       ggplot(data = TransformedDb_ForListCountComparison_ByBuiltType()) + 
         geom_bar(mapping = aes(x = D1vsD2))
       
     } else if (input$graph_type == "b") { 
       ggplot(data = TransformedDb_ForListCountComparison_ByBuiltType()) + 
         geom_bar(mapping = aes(x = D1vsD2, fill = Size_Count_Type), 
                  position = "dodge")
       
     } else { 
       ggplot(data = TransformedDb_ForListCountComparison_ByBuiltType()) + 
         geom_bar(mapping = aes(x = D1vsD2, fill = Size_Count_Type), 
                  position = "fill")
     }
   })
}

# Run the application --------------------------------------------------------------- 
shinyApp(ui = ui, server = server)

