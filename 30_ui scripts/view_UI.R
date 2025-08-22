#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               UI for view tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  01 August 2024
#   LAST MODIFIED DATE    12 June 2025
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Visualization of data
# 
#--------------------------------------------------------------------------------------------------------



# Definition of the user interface for the dashboard page
dashboard_ui <- dashboardPage(
  dashboardHeader(title = "Data visualisation", titleWidth = 300, disable = TRUE),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dictionary", tabName = "dictionary", icon = icon("book")),
      menuItem("User Guide", tabName = "userguide", icon = icon("clipboard")),
      menuItem("Efficiency opinions", tabName = "HASview", icon = icon("magnifying-glass"), selected = TRUE),
      menuItem("Efficiency opinions (options)", tabName = "HASview2", icon = icon("list"), startExpanded = TRUE,
               div(
                 div("Search:", style = "float:left; margin-left:10px; font-weight: bold; color: #2A034C;"), 
                 div(shinyTree("tree", checkbox = TRUE, search = TRUE, themeIcons = FALSE, wholerow = TRUE, stripes = TRUE, multiple = TRUE)),
                 br(),
                 actionButton("full_screen",
                              "Full screen",
                              icon("paper-plane"),
                              style = "margin-left:20px;color: #FFF; background-color: #451284; border-color: #451284;"),
                 downloadButton("downloadData",
                                "Download",
                                style = "margin-left:20px;color: #FFF; background-color: #451284; border-color: #451284;"),
                 actionButton("id_filter",
                              "Filters on the dataset",
                              icon("filter"),
                              style = "margin-left:20px;color: #FFF; background-color: #451284; border-color: #451284;")
               )
      ),
      menuItem("CEM reservations", tabName = "reservesCEM", icon = icon("table")),
      menuItem("BIM reservations", tabName = "reservesBIM", icon = icon("table")),
      menuItem("Combination products", tabName = "tableassoc", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    actionButton("showSidebar", "Show / Hide sidebar", icon("eye"),
                 style = "padding: 10px; color: black; background-color: #FAB600;"),
    br(), 
    br(),
    tabItems(
      tabItem(tabName = "dictionary",
              div(icon("book"), "Dictionary", style = "color: #451284; font-weight: bold; font-size: 30px;"),
              p(style = "padding: 5px; color: white; background-color: #FAB600; text-indent: 20px; border-radius: 5px;"),
              div(
                style = "border-top: 4px double #451284; text-indent: 20px;",
                h4("Dictionary of variables by database/section"),
                style = "float:left; margin-left:50px;",
                dataTableOutput("dico")
              )
      ),
      tabItem(tabName = "userguide",
              div(icon("clipboard"), "User Guide", style = "color: #451284; font-weight: bold; font-size: 30px;"),
              p(style = "padding: 5px; color: white; background-color: #FAB600; text-indent: 20px; border-radius: 5px;"),
              h4("Guide d'utilisateur : cliquer sur le logo ci-dessous !!!"),
              br(),
              # add PHS logo to navigation bar
              div(style = "position: relative;
                             top: -15px;
                             margin-left: 10px;
                             margin-top: 5px;",
                  tags$a(img(src = "UserGuide.png",
                             width = 120),
                         href = "http://172.17.46.13/CEESPlorer/UserGuide.pdf",
                         target = "_blank")
              ),
              br(),
              h4("Guide d'implémentation : cliquer sur le logo ci-dessous !!!"),
              # add PHS logo to navigation bar
              div(style = "position: relative;
                             top: -15px;
                             margin-left: 10px;
                             margin-top: 5px;",
                  tags$a(img(src = "UserImplementation.png",
                             width = 120),
                         href = "http://172.17.46.13/CEESPlorer/UserImplementation.pdf",
                         target = "_blank")
              ),
      ),
      tabItem(tabName = "HASview",
              div(icon("magnifying-glass"), "Data view", style = "padding: 5px; color: #451284; font-weight: bold; font-size: 30px;"),
              div(div(icon("info"), "INFORMATION:", style = "margin-left:10px; font-weight: bold; font-size: 14px;"),
                  verbatimTextOutput("info_text"),
                  p(style = "padding: 5px; color: white; background-color: #FAB600; text-indent: 20px; border-radius: 5px;"),
              ),
              div(
                conditionalPanel(condition = "output.show_filter",
                                 div(
                                   column(2, actionButton("resetAll", "Reset all filters", icon("refresh"),
                                              style = "margin-left:50px; color: #451284; background-color: #FFF; border-color: #451284;")),
                                   column(2, checkboxInput("HASfilter", "Apply filters in Efficiency opinions", FALSE)),
                                   column(2, checkboxInput("CEMfilter", "Apply filters in CEM reservations", FALSE)),
                                   column(2, checkboxInput("BIMfilter", "Apply filters in BIM reservations", FALSE))
                                 ),
                                 br(),
                                 br(),
                                 div(
                                   id = "id_filter_reset",
                                   br(),
                                   h4("Data overview with 6 filters (maximum)"),
                                   column(2, uiOutput('select_filter1'), uiOutput('filter1'), uiOutput('missing1')),
                                   column(2, uiOutput('select_filter2'), uiOutput('filter2'), uiOutput('missing2')),
                                   column(2, uiOutput('select_filter3'), uiOutput('filter3'), uiOutput('missing3')),
                                   column(2, uiOutput('select_filter4'), uiOutput('filter4'), uiOutput('missing4')),
                                   column(2, uiOutput('select_filter5'), uiOutput('filter5'), uiOutput('missing5')),
                                   column(2, uiOutput('select_filter6'), uiOutput('filter6'), uiOutput('missing6')),
                                 ),
                ),
                br(),
                br(),
                fullscreen_this(dataTableOutput("view"), click_id = "full_screen")
              )
      ),
      tabItem(tabName = "reservesCEM",
              div(icon("table"), "CEM reservations", style = "color: #451284; font-weight: bold; font-size: 30px;"),
              p(style = "padding: 5px; color: white; background-color: #FAB600; text-indent: 20px; border-radius: 5px;"),
              downloadButton("downloadCEM",
                             "Download",
                             style = "margin-left:20px;color: #FFF; background-color: #451284; border-color: #451284;"),
              dataTableOutput("view_CEM")
      ),
      tabItem(tabName = "reservesBIM",
              div(icon("table"), "BIM reservations", style = "color: #451284; font-weight: bold; font-size: 30px;"),
              p(style = "padding: 5px; color: white; background-color: #FAB600; text-indent: 20px; border-radius: 5px;"),
              downloadButton("downloadBIM",
                             "Download",
                             style = "margin-left:20px;color: #FFF; background-color: #451284; border-color: #451284;"),
              dataTableOutput("view_BIM")
      ),
      tabItem(tabName = "tableassoc",
              div(icon("table"), "Combination products", style = "color: #451284; font-weight: bold; font-size: 30px;"),
              p(style = "padding: 5px; color: white; background-color: #FAB600; text-indent: 20px; border-radius: 5px;"),
              dataTableOutput("view_assoc")
      )
    )
  )
)


# View tab
viewTab <-
  tabPanel(
    div(div(class = "fa fa-binoculars", role = "navigation"), "Data View"),
    value = "view",
    dashboard_ui # Inclusion of 'dashboard' in a 'navbarPage' page
  )



## END
