#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               UI for graph tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  24 October 2024
#   LAST MODIFIED DATE    29 July 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Graph page
# 
#--------------------------------------------------------------------------------------------------------



graphTab <- tabPanel(
  div(div(class = "fa fa-chart-simple", role = "navigation"), "Graphics"),
  value = "graph",
  
  tags$head(tags$style(HTML("body { background-color: #E2E2F2; }"))),
  sidebarPanel(
    h3("Graphique 1"),
    selectInput('xcol1', 'X variable', var_numerique_base_HAS),
    selectInput('ycol1', 'Y variable', var_numerique_base_HAS),
    h3("Graphique 2"),
    selectInput('xcol2', 'X variable', var_cate_base_HAS),
                # selected = names(baseHAS_shiny_graph)[[1]]
  ),
  mainPanel(
    plotlyOutput("plot1"),
    br(),br(),
    plotlyOutput("plot2")
  )
  
)



## END
