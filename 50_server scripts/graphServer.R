#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server for graph tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 April 2024
#   LAST MODIFIED DATE    29 July 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Graph of data
# 
#--------------------------------------------------------------------------------------------------------


  
selectedDataX <- reactive({
  baseHAS_shiny_graph[,c(input$xcol1)]
})

selectedDataY <- reactive({
  baseHAS_shiny_graph[,c(input$ycol1)]
})

output$plot1 <- renderPlotly(
  plot1 <- plot_ly(baseHAS_shiny_graph, x = selectedDataX(), y = selectedDataY(), type ="scatter", mode = "markers")
)

df_plot2 <- baseHAS_shiny_graph %>%
    select(input$xcol2) %>%
    rename(categories=1) %>%
    group_by(categories) %>%
    summarise(n = n())

output$plot2 <- renderPlotly(
  plot2 <- plot_ly(data = df_plot2) %>% add_bars(x = ~categories, y = ~n)
)