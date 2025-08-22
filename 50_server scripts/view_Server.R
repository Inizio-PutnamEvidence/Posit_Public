#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Server for view tab
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  25 July 2023
#   LAST MODIFIED DATE    12 June 2025
#   RSTUDIO VERSION       2024.44.04 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-40-34 ucrt) (MMO) / 4.3.4 (2023-06-46 ucrt) (CTO)
#   DESCRIPTION           Visualization of data
# 
#--------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------#
####      Parameters & Initialization                                                                            ####
#-------------------------------------------------------------------------------------------------------------------#
{ 
  
  # Information concerning the presence or not of ROW in the QC
  output$info_text <- renderText({
    
    nHAS = nrow(as.data.frame(read_excel(paste0(getwd(),"/",folder_data,"/toQC_HAS.xlsx"))))
    nCEM = nrow(as.data.frame(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesCEM.xlsx"))))
    nBIM = nrow(as.data.frame(read_excel(paste0(getwd(),"/",folder_data,"/toQC_ReservesBIM.xlsx"))))
    nAssoc = nrow(as.data.frame(read_excel(paste0(getwd(),"/",folder_data,"/toQC_TableAssoc.xlsx"))))
    
    n = nHAS + nCEM + nBIM + nAssoc
    last_update = read_excel(paste0(getwd(),"/",folder_import,"/10_save/newline_HAS.xlsx"))
    last_avis <- last_update %>% filter(Statut == "Nouvelle HAS") %>% mutate(date = as.Date(date)) %>% filter(date == max(date)) %>% select(date) %>% unique()  
    
    if (n==0) {
      paste0("Last efficiency opinion added on the ", last_avis$date, ". The databases are up to date (Efficiency opinions, CEM/BIM methodological reservations, and combination products).")
    } else {
      "QC in progress: at least one of the databases is in the QC phase (Efficiency opinions, and/or CEM/BIM methodological reservations, and/or combination products)."
    }

  })
  
  # Dictionary of variables displayed as a TREE for AvisEfficience (only)
  output$tree <- renderTree({
    list(
      `Informations générales` = structure(treelist$`Avis Efficience`$`Informations générales`, stselected = TRUE),
      `Evaluation HAS` = structure(treelist$`Avis Efficience`$`Evaluation HAS`, stselected = TRUE),
      `Population cible` = structure(treelist$`Avis Efficience`$`Population cible`, stselected = TRUE),
       Prix = treelist$`Avis Efficience`$Prix,
       Posologie = treelist$`Avis Efficience`$Posologie,
       Délais = treelist$`Avis Efficience`$Délais
    )
  })
  
  # Dynamically generate the variable names selected in the TREE
  observeEvent(input$tree, {
    var_selected_tree = names(as.data.frame(get_selected(input$tree, format = "slices")))
    updateSelectInput(session, "vartree1", choices = var_selected_tree)
  })
  
  # Hide/Unhide the sidebar
  sidebarVisible <- reactiveVal(TRUE)
  observeEvent(input$showSidebar, {
    if (sidebarVisible()) {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    }
    # Reverse sidebar status
    sidebarVisible(!sidebarVisible())
  })
  
  # Hide/Unhide the buttons
  output$show_filter <- reactive({ input$id_filter%%2 })
  
  # Show
  outputOptions(output, "show_filter", suspendWhenHidden = FALSE)
  
  # Reset the filters
  observeEvent(input$resetAll, {
    reset("id_filter_reset")
    reset("HASfilter")    
    reset("CEMfilter")
    reset("BIMfilter")
  })
  
}

#-------------------------------------------------------------------------------------------------------------------#
####      Functions                                                                                              ####
#-------------------------------------------------------------------------------------------------------------------#
{ 

  # Transformation of variables into numeric
  func_num = function(var){
    var = suppressWarnings(as.numeric(as.character(var)))
  }
  
  
  # Retrieving variable modalities
  func_list_char = function(df, name_var){
    if (name_var %in% c("EchangeTechNbQuestion",list_var_char_num_range)){
      if ("-" %in% as.character(df %>% select(name_var) %>% unique() %>% distinct() %>% pull())){
        list_char = c("Missing",df %>% mutate(tmp = suppressWarnings(ifelse(is.na(as.numeric(as.character(get(name_var)))), as.character(get(name_var)), "-"))) %>% select(tmp) %>% filter(tmp != "-") %>% unique() %>% pull())
      } 
      else {
        list_char = c(df %>% mutate(tmp = suppressWarnings(ifelse(is.na(as.numeric(as.character(get(name_var)))), as.character(get(name_var)), "-"))) %>% select(tmp) %>% filter(tmp != "-") %>% unique() %>% pull())
      }
    }
    return(list_char)
  }
  
  
  # Database restricted according to the modalities selected
  func_filter <- function(df, name_var, input_select_filter, input_filter, input_missing){
    
    name_var = dico$Variable[which(dico$labelShiny %in% input_select_filter)]
    
    if (length(name_var) > 0) {
      
      # GROUP of variables 1
      if (grepl("Date",name_var) | name_var %in% list_var_num_slider | name_var %in% list_var_num_range){
        if (input_missing == TRUE) {
          if (input$value) { 
            df <- df %>% filter((get(name_var) >= input_filter[1] & get(name_var) <= input_filter[2]) | is.na(get(name_var))) 
          }
          else 
            df <- df %>% filter(is.na(get(name_var))) 
        }
        else {
          if (input$value) { 
            df <- df %>% filter(get(name_var) >= input_filter[1] & get(name_var) <= input_filter[2])
          }
          else {
            df <- df
          }
        }
      }
      
      # GROUP of variables 2
      else if (name_var %in% c("EchangeTechNbQuestion",list_var_char_num_range)){
        if (length(input_missing) == 0) {
          if (input$value) {
            df = df %>% mutate({{name_var}} := func_num(get(name_var))) %>% filter(get(name_var) >= input_filter[1] & get(name_var) <= input_filter[2])
            df = df %>% mutate({{name_var}} := as.character(get(name_var)))
          }
          else {
            df = df %>% mutate({{name_var}} := as.character(get(name_var)))
          }
        }
        if (length(input_missing)>0) {
          if (input$value) {
            if ("Missing" %in% input_missing) {
              df <- rbind(
                df %>% mutate({{name_var}} := func_num(get(name_var))) %>% filter(get(name_var) >= input_filter[1] & get(name_var) <= input_filter[2]),
                df %>% filter(get(name_var) %in% c("-",input_missing))
              )
            } 
            else {
              df <- rbind(
                df %>% mutate({{name_var}} := func_num(get(name_var))) %>% filter(get(name_var) >= input_filter[1] & get(name_var) <= input_filter[2]),
                df %>% filter(get(name_var) %in% input_missing)
              )
            }
          }
          else {
            if ("Missing" %in% input_missing) {
              df <- df %>% filter(get(name_var) %in% c("-",input_missing))
            } 
            else {
              df <- df %>% filter(get(name_var) %in% input_missing)
            }
          }   
        }
      }
      
      # GROUP of variables 3
      else {
        df <- df %>% filter(get(name_var) %in% input_filter)
      }
      
      return(df)
    }
  }
  
  
  # Type of visualisation according to the modalities seleted
  func_typefilter <- function(df, name_var, name_input, name_missing, name_choices){
    if (length(name_var) > 0) {
      if (grepl("Date",name_var)){
                                                      div(
                                                        checkboxInput(inputId = name_missing, 
                                                                      label   = "Missing data?",
                                                                      value   = TRUE),
                                                        checkboxInput(inputId = "value", 
                                                                      label   = "Select a range of dates", 
                                                                      value   = TRUE),
                                                        conditionalPanel(condition = "input.value == true",
                                                                         dateRangeInput(inputId = name_input,
                                                                                        label   = NULL)
                                                        )
                                                      )
      } else if (name_var %in% list_var_num_slider){
                                                      div(
                                                        checkboxInput(inputId = name_missing, 
                                                                      label   = "Missing data?",
                                                                      value   = TRUE),
                                                        checkboxInput(inputId = "value", 
                                                                      label   = "Select a range of values", 
                                                                      value   = TRUE),
                                                        conditionalPanel(condition = "input.value == true",
                                                                         sliderInput(inputId = name_input,
                                                                                     label   = NULL,
                                                                                     min     = min(name_choices %>% pull() %>% sort()),
                                                                                     max     = max(name_choices %>% pull() %>% sort()), 
                                                                                     value   = c(min(name_choices %>% pull() %>% sort()), max(name_choices %>% pull() %>% sort())),
                                                                                     step    = 1)
                                                        )
                                                      )
      } else if (name_var == "EchangeTechNbQuestion"){
                                                      div(
                                                        checkboxGroupInput(inputId  = name_missing,
                                                                           label    = NULL,
                                                                           choices  = func_list_char(df, name_var),
                                                                           selected = func_list_char(df, name_var),
                                                                           inline   = FALSE),
                                                        checkboxInput(inputId = "value", 
                                                                      label   = "Select a range of values", 
                                                                      value   = TRUE),
                                                        conditionalPanel(condition = "input.value == true",
                                                                         sliderInput(inputId = name_input,
                                                                                     label   = NULL,
                                                                                     min     = min(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE),
                                                                                     max     = max(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE),
                                                                                     value   = c(min(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE), max(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE)),
                                                                                     step    = 1)
                                                        )
                                                      )
      } else if (name_var %in% list_var_num_range){
                                                      div(
                                                        checkboxInput(inputId = name_missing, 
                                                                      label   = "Missing data?",
                                                                      value   = TRUE),
                                                        checkboxInput(inputId = "value", 
                                                                      label   = "Select a range of values", 
                                                                      value   = TRUE),
                                                        conditionalPanel(condition = "input.value == true",
                                                                         numericRangeInput(inputId = name_input,
                                                                                           label     = NULL,
                                                                                           min       = min(name_choices %>% pull() %>% sort()),
                                                                                           separator = " to ",
                                                                                           max       = max(name_choices %>% pull() %>% sort()), 
                                                                                           value     = c(min(name_choices %>% pull() %>% sort()), max(name_choices %>% pull() %>% sort())),
                                                                                           step      = 1)
                                                        )
                                                      )
      } else if (name_var %in% list_var_char_num_range){
                                                      div(
                                                        checkboxGroupInput(inputId  = name_missing,
                                                                           label    = NULL, 
                                                                           choices  = func_list_char(df, name_var),
                                                                           selected = func_list_char(df, name_var),
                                                                           inline   = FALSE),
                                                        checkboxInput(inputId = "value", 
                                                                      label   = "Select a range of values", 
                                                                      value   = TRUE),
                                                        conditionalPanel(condition = "input.value == true",
                                                                         numericRangeInput(inputId = name_input,
                                                                                           label     = NULL,
                                                                                           min       = min(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE),
                                                                                           separator = " to ",
                                                                                           max       = max(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE), 
                                                                                           value     = c(min(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE), max(as.numeric(name_choices %>% pull() %>% sort()), na.rm = TRUE)),
                                                                                           step      = 1)
                                                        )
                                                      )
      } else {
        pickerInput(inputId  = name_input,
                    label    = em("(Un)select the modality(-ies)"),
                    choices  = name_choices %>% pull() %>% sort(),
                    options  = list('actions-box' = TRUE, 'live-search' = TRUE),
                    multiple = TRUE)
      }
    }
  }
  
}

#-------------------------------------------------------------------------------------------------------------------#
####      Type of variables and Formatting DB                                                                    ####
#-------------------------------------------------------------------------------------------------------------------#
{ 
  
  # ----------------------------------> Type of variables
  
  # Numeric variable with a slider as visualization
  list_var_num_slider = c(
    ###########################################CENSORED######################
  )
  
  
  # Numeric variable with a range value as visualization
  list_var_num_range = c(
    ###########################################CENSORED######################
  )
  
  
  # Character & Numeric variable with a range value as visualization
  list_var_char_num_range = c(
    ###########################################CENSORED######################
  )
  
  
  
  # ----------------------------------> Formatting DB

  # Formatting
  HASView = HASView %>% 
    # update variable numerical variable with only NA/"-" as missing
    mutate(
      # Information Générale
      ###########################################CENSORED######################
      # Evaluation HAS
      ###########################################CENSORED######################
      # Population cible
      ###########################################CENSORED######################
      # Prix ==> attention - perte de l'info "voir table des assoc"
      ###########################################CENSORED######################
      # Posologie ==>  delete "table des assoc"
      ###########################################CENSORED######################
    # Delai
    ###########################################CENSORED######################
    ) %>%
    # need to replace date = dd-mm-yyy by date = yyyy-mm-dd
    mutate(
      ###########################################CENSORED######################
    ) 

}
 
#-------------------------------------------------------------------------------------------------------------------#
####      Filters                                                                                                ####
#-------------------------------------------------------------------------------------------------------------------#
{ 
  
  # Filter (6 variables maximum)
  
  # ------------------------------------------------- Variable 1
  
      output$select_filter1 <- renderUI({
        categories = names(as.data.frame(get_selected(input$tree, format = "slices")))
        categories = c("",dico$labelShiny[which(dico$labelShinytree %in% categories)])
        pickerInput(inputId = "select_filter1", 
                    label   = em("Select a FIRST variable"), 
                    choices = levels(as.factor(categories)), 
                    options = list('live-search' = TRUE))
      })


      output$filter1 <- renderUI({
        var1 = dico$Variable[which(dico$labelShiny %in% input$select_filter1)]
        modality_choices = df_selected_tree %>% select(all_of(var1)) %>% mutate_if(is.factor, as.character) %>% mutate_if(is.logical, as.character) %>% unique()
        func_typefilter(df = df_selected_tree, name_var = var1, name_input = "filter1", name_missing = "missing1", name_choices = modality_choices)
      })
      
      sf1 <- reactive({ input$select_filter1 })
       f1 <- reactive({ input$filter1 })

  # ------------------------------------------------- Variable 2 
  
      output$select_filter2 <- renderUI({
        req(f1())
        categories = names(as.data.frame(get_selected(input$tree, format = "slices")))
        categories = c("",dico$labelShiny[which(dico$labelShinytree %in% categories)])
        categories = categories[categories != input$select_filter1]
        pickerInput(inputId = "select_filter2", 
                    label   = em("Select a SECOND variable"), 
                    choices = levels(as.factor(c("",categories))), 
                    options = list('live-search' = TRUE))
      })
      
      output$filter2 <- renderUI({
        df1  = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
        var2 = dico$Variable[which(dico$labelShiny %in% input$select_filter2)]
        modality_choices = df1 %>% select(all_of(var2)) %>% mutate_if(is.factor, as.character) %>% mutate_if(is.logical, as.character) %>% unique()
        func_typefilter(df = df1, name_var = var2, name_input = "filter2", name_missing = "missing2", name_choices = modality_choices)
      })
      
      sf2 <- reactive({ input$select_filter2 })
       f2 <- reactive({ input$filter2 })
  
  # ------------------------------------------------- Variable 3 
  
      output$select_filter3 <- renderUI({
        req(f1())
        req(f2())
        categories = names(as.data.frame(get_selected(input$tree, format = "slices")))
        categories = c("",dico$labelShiny[which(dico$labelShinytree %in% categories)])
        categories = categories[! categories %in% c(input$select_filter1, input$select_filter2)]
        pickerInput(inputId = "select_filter3",
                    label   = em("Select a THIRD variable"),
                    choices = levels(as.factor(c("",categories))),
                    options = list('live-search' = TRUE))
      })
      
      output$filter3 <- renderUI({
        df1  = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
        df2  = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
        var3 = dico$Variable[which(dico$labelShiny %in% input$select_filter3)]
        modality_choices = df2 %>% select(all_of(var3)) %>% mutate_if(is.factor, as.character) %>% mutate_if(is.logical, as.character) %>% unique()
        func_typefilter(df = df2, name_var = var3, name_input = "filter3", name_missing = "missing3", name_choices = modality_choices)
      })
      
      sf3 <- reactive({ input$select_filter3 })
       f3 <- reactive({ input$filter3 })
  
  # ------------------------------------------------- Variable 4

      output$select_filter4 <- renderUI({
        req(f1())
        req(f2())
        req(f3())
        categories = names(as.data.frame(get_selected(input$tree, format = "slices")))
        categories = c("",dico$labelShiny[which(dico$labelShinytree %in% categories)])
        categories = categories[! categories %in% c(input$select_filter1, input$select_filter2, input$select_filter3)]
        pickerInput(inputId = "select_filter4",
                    label   = em("Select a FOURTH variable"),
                    choices = levels(as.factor(c("",categories))),
                    options = list('live-search' = TRUE))
      })
    
      output$filter4 <- renderUI({
        df1  = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
        df2  = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
        df3  = func_filter(df2, var3, input$select_filter3, input$filter3, input$missing3)
        var4 = dico$Variable[which(dico$labelShiny %in% input$select_filter4)]
        modality_choices = df3 %>% select(all_of(var4)) %>% mutate_if(is.factor, as.character) %>% mutate_if(is.logical, as.character) %>% unique()
        func_typefilter(df = df3, name_var = var4, name_input = "filter4", name_missing = "missing4", name_choices = modality_choices)
      })
    
      sf4 <- reactive({ input$select_filter4 })
       f4 <- reactive({ input$filter4 })

  # ------------------------------------------------- Variable 5

      output$select_filter5 <- renderUI({
        req(f1())
        req(f2())
        req(f3())
        req(f4())
        categories = names(as.data.frame(get_selected(input$tree, format = "slices")))
        categories = c("",dico$labelShiny[which(dico$labelShinytree %in% categories)])
        categories = categories[! categories %in% c(input$select_filter1, input$select_filter2, input$select_filter3, input$select_filter4)]
        pickerInput(inputId = "select_filter5",
                    label   = em("Select a FIFTH variable"),
                    choices = levels(as.factor(c("",categories))),
                    options = list('live-search' = TRUE))
      })
    
      output$filter5 <- renderUI({
        df1  = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
        df2  = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
        df3  = func_filter(df2, var3, input$select_filter3, input$filter3, input$missing3)
        df4  = func_filter(df3, var4, input$select_filter4, input$filter4, input$missing4)
        var5 = dico$Variable[which(dico$labelShiny %in% input$select_filter5)]
        modality_choices = df4 %>% select(all_of(var5)) %>% mutate_if(is.factor, as.character) %>% mutate_if(is.logical, as.character) %>% unique()
        func_typefilter(df = df4, name_var = var5, name_input = "filter5", name_missing = "missing5", name_choices = modality_choices)
      })
    
      sf5 <- reactive({ input$select_filter5 })
       f5 <- reactive({ input$filter5 })

  # ------------------------------------------------- Variable 6

      output$select_filter6 <- renderUI({
        req(f1())
        req(f2())
        req(f3())
        req(f4())
        req(f5())
        categories = names(as.data.frame(get_selected(input$tree, format = "slices")))
        categories = c("",dico$labelShiny[which(dico$labelShinytree %in% categories)])
        categories = categories[! categories %in% c(input$select_filter1, input$select_filter2, input$select_filter3, input$select_filter4, input$select_filter5)]
        pickerInput(inputId = "select_filter6",
                    label   = em("Select a SIXTH variable"),
                    choices = levels(as.factor(c("",categories))),
                    options = list('live-search' = TRUE))
      })
    
      output$filter6 <- renderUI({
        df1  = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
        df2  = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
        df3  = func_filter(df2, var3, input$select_filter3, input$filter3, input$missing3)
        df4  = func_filter(df3, var4, input$select_filter4, input$filter4, input$missing4)
        df5  = func_filter(df4, var5, input$select_filter5, input$filter5, input$missing5)
        var6 = dico$Variable[which(dico$labelShiny %in% input$select_filter6)]  
        modality_choices = df5 %>% select(all_of(var6)) %>% mutate_if(is.factor, as.character) %>% mutate_if(is.logical, as.character) %>% unique()
        func_typefilter(df = df5, name_var = var6, name_input = "filter6", name_missing = "missing6", name_choices = modality_choices)
      })
    
      sf6 <- reactive({ input$select_filter6 })
       f6 <- reactive({ input$filter6 })

}

#-------------------------------------------------------------------------------------------------------------------#
####      Dictionary of variables                                                                               ####
#-------------------------------------------------------------------------------------------------------------------#
{ 

  # Render the output element based on the dictionary of variables
  output$dico <- renderDT(
    dico %>% select(Database, Section, labelShiny, `French label`) %>% rename(Variable = labelShiny),
    escape = FALSE,
    rownames = FALSE,
    filter = "top",
    options = list(
                    processing = FALSE,
                    paging = FALSE,
                    scrollX = TRUE,
                    scrollY = '60vh',
                    scrollCollapse = TRUE,
                    autoWidth = TRUE,
                    fixedHeader = TRUE,
                    initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#451284', 'color': 'white'});","}"),
                    columnDefs = list(list(width = '100px', targets = "Database"),
                                      list(width = '200px', targets = c("Section","Variable")),
                                      list(width = '500px', targets = "French label"))
    )
  )
  
}

#-------------------------------------------------------------------------------------------------------------------#
####      Dataframe - reactive / View of data (HAS)                                                              ####
#-------------------------------------------------------------------------------------------------------------------#
{ 
  
  # Creation on databases after selecting variables wanted in link with the TREE
  filteredData <- reactive({
    
    # Selection of variables in the TREE
    var_selected_tree  <- names(as.data.frame(get_selected(input$tree, format = "slices")))
    var_selected_tree  <- dico$Variable[which(dico$labelShinytree %in% var_selected_tree)]
    save_name_selected <- names(HASView)[names(HASView) %in% var_selected_tree]   
    df_selected_tree   <<- HASView %>% select(c("Buttons",save_name_selected))

    # Variables
    var1 <- dico$Variable[which(dico$labelShiny %in% input$select_filter1)]
    var2 <- dico$Variable[which(dico$labelShiny %in% input$select_filter2)]
    var3 <- dico$Variable[which(dico$labelShiny %in% input$select_filter3)]
    var4 <- dico$Variable[which(dico$labelShiny %in% input$select_filter4)]
    var5 <- dico$Variable[which(dico$labelShiny %in% input$select_filter5)]
    var6 <- dico$Variable[which(dico$labelShiny %in% input$select_filter6)]
    
    # Without filters !
    if (input$id_filter%%2 == 0) {
      return(df_selected_tree)
    }

    # With filters !
    if(input$HASfilter == TRUE) {

        if (length(var1)>0 & isTruthy(sf1()) & isTruthy(f1())) {
          df = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
        }
        
        if (length(var2)>0 & isTruthy(sf2()) & isTruthy(f2())) {
          df1 = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
          df  = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
        }
    
        if (length(var3)>0 & isTruthy(sf3()) & isTruthy(f3())) {
          df1 = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
          df2 = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
          df  = func_filter(df2, var3, input$select_filter3, input$filter3, input$missing3)
        }
        
        if (length(var4)>0 & isTruthy(sf4()) & isTruthy(f4())) {
          df1 = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
          df2 = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
          df3 = func_filter(df2, var3, input$select_filter3, input$filter3, input$missing3)
          df  = func_filter(df3, var4, input$select_filter4, input$filter4, input$missing4)
        }
        
        if (length(var5)>0 & isTruthy(sf5()) & isTruthy(f5())) {
          df1 = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
          df2 = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
          df3 = func_filter(df2, var3, input$select_filter3, input$filter3, input$missing3)
          df4 = func_filter(df3, var4, input$select_filter4, input$filter4, input$missing4)
          df  = func_filter(df4, var5, input$select_filter5, input$filter5, input$missing5)
        }
        
        if (length(var6)>0 & isTruthy(sf6()) & isTruthy(f6())) {
          df1 = func_filter(df_selected_tree, var1, input$select_filter1, input$filter1, input$missing1)
          df2 = func_filter(df1, var2, input$select_filter2, input$filter2, input$missing2)
          df3 = func_filter(df2, var3, input$select_filter3, input$filter3, input$missing3)
          df4 = func_filter(df3, var4, input$select_filter4, input$filter4, input$missing4)
          df5 = func_filter(df4, var5, input$select_filter5, input$filter5, input$missing5)
          df  = func_filter(df5, var6, input$select_filter6, input$filter6, input$missing6)
        }
      
      return(df)
    }
    
  })

  
  # Downloadable xlsx of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(today(),"-",""),"_Putnam_CEESPlorer.xlsx")
    },
    content = function(file) {
      ###########################################CENSORED######################
    }
  )

  
  # Render the output element based on the filtered data
  output$view <- renderDT({
    df <- filteredData() %>% mutate(
        # Source
      ###########################################CENSORED######################
        # Euro - 2 decimal
      ###########################################CENSORED######################
      # Percentage - 2 decimal 
      ###########################################CENSORED######################
        # Number - 0 decimal 
      ###########################################CENSORED######################
        # Number - 2 decimal  
      ###########################################CENSORED######################
        # Jours - 0 decimal  
      ###########################################CENSORED######################
        # Character - formatage puissance
      ###########################################CENSORED######################
      )
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = dico$labelShiny[which(dico$Variable %in% colnames(df_selected_tree))],
      extensions = c("FixedHeader","FixedColumns"),
      options = list(
                      searching = FALSE,
                      processing = FALSE,
                      paging = FALSE,
                      scrollX = TRUE,
                      scrollY = '60vh',
                      scrollCollapse = TRUE,
                      autoWidth = TRUE,
                      fixedHeader = TRUE,
                      fixedColumns = list(leftColumns = 3),
                      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#EAD5FD', 'color': 'black'});","}"),
                      columnDefs = list(list(visible = FALSE, targets = c(which(colnames(df_selected_tree) %in% c("Index")))-1),
                                        list(width = '30px',  targets = c(which(colnames(df_selected_tree) %in% c("ID")))-1),
                                        list(width = '30px', targets = c(which(colnames(df_selected_tree) %in% c("Molecule")))-1),
                                        list(width = '500px', targets = c(which(colnames(df_selected_tree) %in% c("Indication","TypeModeleHAS","Posologie_Detail","Horizon")))-1),
                                        list(width = '200px', targets = c(which(colnames(df_selected_tree) %in% c("ATC_Label","MotifReevaluation","HorizonTemporel","TypeModelePutnam","CEM_Precision","BIM_Precision")))-1),
                                        list(width = '100px', targets = c(which(!(colnames(df_selected_tree) %in% c("ID","Molecule","Indication","Posologie_Detail","Horizon","ATC_Label","MotifReevaluation","HorizonTemporel","TypeModeleHAS","TypeModelePutnam","CEM_Precision","BIM_Precision"))))-1))
    )
  )
  })
  
}

#-------------------------------------------------------------------------------------------------------------------#
####      Read comment button                                                                                    ####
#-------------------------------------------------------------------------------------------------------------------#
{
  
  rv_read_comm <- reactiveValues(dt_view_row = 0) 
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "readcommentQC"))
    rv_read_comm$dt_row <- which(stringr::str_detect(HASView$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    
    ui_elements <- lapply(1:length(str_split(HASView$CommentsQC[rv_read_comm$dt_row[1] ], "\n")[[1]]), function(i) {
      tagList(
        str_split(HASView$CommentsQC[rv_read_comm$dt_row[1] ], "\n")[[1]][i],
        br()
      )
    })
    
    shiny::modalDialog(
      title = "Commentaires",
      strong("Commentaires du reviewer lors du QC"), br(),
      do.call(tagList, ui_elements),
      br(),
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-12 text-right",
            shiny::actionButton(inputId = "close_commentQCView", label = "Close", class = "btn-danger"),
          )
        )
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$close_commentQCView, {
    shiny::removeModal()
  })
  
}

#-------------------------------------------------------------------------------------------------------------------#
####      Put Comment in View - Form                                                                             ####
#-------------------------------------------------------------------------------------------------------------------#
{
  
  rv_edit_comm <- reactiveValues(dt_view_row = 0) 
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "writecommentView"))
    rv_edit_comm$dt_view_row <- which(stringr::str_detect(HASView$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    
    shiny::modalDialog(
      title = "Comment this line",
      textInput(inputId = "namecommentsview", label = HTML('<span style="color: black;">Name:</span> <span style="color: red;">*</span>'), placeholder = "Your name", width = "100%"),
      radioButtons(inputId = "subjectcommentsview", label = HTML('<span style="color: black;">Subject:</span> <span style="color: red;">*</span>'), choices = c("Correction","Question","Error","Observation","Other"), width = "100%", selected = "Correction"),
      textAreaInput(inputId = "commentsview", label = "Your comment:", placeholder = "Comments", width = "100%"),
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-6 text-left",
            shiny::actionButton(inputId = "cancel_commentView", label = "Cancel", class = "btn-danger"),
          ),
          div(
            class = "col-md-6 text-right",
            shiny::actionButton(inputId = "valid_commentView", label = "Validate", class = "btn-info"),
          )
        )
      )
    ) %>% shiny::showModal()
  })
  
  shiny::observeEvent(input$valid_commentView, {
    commentsView_qc <- read_excel(paste0(getwd(),"/",folder_data,"/toQC_CommentsView.xlsx"))
    id_buttons_commview_qc <- ifelse(nrow(commentsView_qc) == 0, 1, max(as.numeric(str_extract(commentsView_qc$Buttons, "(\\d+)"))) +1)
    
    qc_commentView_newRow <- {dplyr::tibble(
      Buttons      = create_btns_comm_view_qc(id_buttons_commview_qc),
      ID           = as.character(HASView$ID[rv_edit_comm$dt_view_row[1]]),
      NomProduit   = HASView$NomProduit[rv_edit_comm$dt_view_row[1]],
      Molecule     = HASView$Molecule[rv_edit_comm$dt_view_row[1]],
      NameUser     = input$namecommentsview,
      DateComment  = as.character(Sys.Date()),
      SubjectComm  = input$subjectcommentsview,
      CommentsView = input$commentsview
    )}
    
    commentsView_qc <- commentsView_qc %>% mutate_if(is.logical, as.character) 
    commentsView_qc <- qc_commentView_newRow %>% dplyr::bind_rows(commentsView_qc)   
    write_xlsx(commentsView_qc, paste0(getwd(),"/",folder_data,"/toQC_CommentsView.xlsx"))
    
    shiny::modalDialog(
      title = "Confirmation",
      "Your comment has been sent to the QC", br(),
      size = "s",
      easyClose = FALSE,
      footer = tagList(
        div(
          class = "row",
          div(
            class = "col-md-12 text-right",
            shiny::actionButton(inputId = "valid_commentView2", label = "Ok", class = "btn-success"),
          )
        )
      )
    ) %>% shiny::showModal()
  })
  
  shiny::observeEvent(input$valid_commentView2, {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$cancel_commentView, {
    shiny::removeModal()
  })
  
}

#-------------------------------------------------------------------------------------------------------------------#
####      Table des associations                                                                                  ####
#-------------------------------------------------------------------------------------------------------------------#
{ 
  
  # Tale des associations
  output$view_assoc <- renderDT({
    df <- func_FormattingAssoc(baseAssoc_shiny, "no_select")
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c(varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(baseAssoc_shiny) & varlistHAS$Variable %in% c("ID","NomProduit"))],
                   "Nom du produit Assoc.",
                   varlistHAS$`Label des colonnes dans Rshiny`[which(varlistHAS$Variable %in% colnames(baseAssoc_shiny) & !varlistHAS$Variable %in% c("ID","NomProduit"))]),
      extensions = c("FixedHeader","FixedColumns"),
      options = list(
                      searching = FALSE,
                      processing = FALSE,
                      paging = FALSE,
                      scrollX = TRUE,
                      scrollY = '60vh',
                      scrollCollapse = TRUE,
                      autoWidth = TRUE,
                      fixedHeader = TRUE,
                      fixedColumns = list(leftColumns = 3),
                      initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#EAD5FD', 'color': 'black'});","}"),
                      columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("DateAMM","Demande","TypeProduit")))-1),
                                        list(width = '50px',  targets = c(which(names(df) %in% c("ID")))-1),
                                        list(width = '500px', targets = c(which(names(df) %in% c("Indication","Posologie_Detail","Horizon")))-1),
                                        list(width = '100px', targets = 1:(ncol(df)-1)))
      )
    )
  })
  
}

#-------------------------------------------------------------------------------------------------------------------#
####      Reserves CEM                                                                                           ####
#-------------------------------------------------------------------------------------------------------------------#
{ 
  
  # Creation on databases after selecting variables wanted in link with the TREE
  filteredCEM <- reactive({
    # Without filters !
    if (input$CEMfilter == FALSE) { return(baseCEM_shiny) }
    # With filters !
    if (input$CEMfilter == TRUE) { return(inner_join(baseCEM_shiny, filteredData() %>% select(ID) %>% mutate(ID = as.character(ID)), by = "ID")) }
  })
  
  # ReservesCEM
  output$view_CEM <- renderDT({
    df <- func_FormattingRes(filteredCEM())
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Numéro", "Numéro réserve", as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resCEM$df) & dico$Variable != "ID")])),
      extensions = c("FixedHeader","FixedColumns"),
      options = list(
        searching = TRUE,
        processing = FALSE,
        paging = FALSE,
        scrollX = TRUE,
        scrollY = '60vh',
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 3),
        initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#EAD5FD', 'color': 'black'});","}"),
        columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("Index","Molecule")))-1),
                          list(width = '50px',  targets = c(which(names(df) %in% c("ID","IDRes")))-1),
                          list(width = '200px', targets = c(which(names(df) %in% c("CEM_DimHAS","CEM_DimPutnam")))-1),
                          list(width = '500px', targets = c(which(names(df) %in% c("CEM_Precision")))-1),
                          list(width = '100px', targets = 1:(ncol(df)-1)))
      ),
      filter = 'top'
    )
  })
  
  
  # Downloadable xlsx of selected dataset
  output$downloadCEM <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(today(),"-",""),"_Putnam_CEESPlorerCEM.xlsx")
    },
    content = function(file) {
      df_download <- baseCEM_shiny %>% select(-Index)
      if("SourceAvisCEESP" %in% names(df_download)){df_download$SourceAvisCEESP <- paste0("http://172.17.46.13/CEESPlorer/",df_download$SourceAvisCEESP)}
      write_xlsx(df_download, file)
    }
  )

}

#-------------------------------------------------------------------------------------------------------------------#
####      Reserves BIM                                                                                           ####
#-------------------------------------------------------------------------------------------------------------------#
{ 
  
  # Creation on databases after selecting variables wanted in link with the TREE
  filteredBIM <- reactive({
    # Without filters !
    if (input$BIMfilter == FALSE) { return(baseBIM_shiny) }
    # With filters !
    if (input$BIMfilter == TRUE) { return(inner_join(baseBIM_shiny, filteredData() %>% select(ID) %>% mutate(ID = as.character(ID)), by = "ID")) } 
  })
  
  # ReservesBIM
  output$view_BIM <- renderDT({
    df <- func_FormattingRes(filteredBIM())
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Numéro", "Numéro réserve", as.character(dico$labelShiny[which(dico$Variable %in% colnames(rv_resBIM$df) & dico$Variable != "ID")])),
      extensions = c("FixedHeader","FixedColumns"),
      options = list(
        searching = TRUE,
        processing = FALSE,
        paging = FALSE,
        scrollX = TRUE,
        scrollY = '60vh',
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 3),
        initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#EAD5FD', 'color': 'black'});","}"),
        columnDefs = list(list(visible = FALSE, targets = c(which(names(df) %in% c("Index","Molecule")))-1),
                          list(width = '50px',  targets = c(which(names(df) %in% c("ID","IDRes")))-1),
                          list(width = '200px', targets = c(which(names(df) %in% c("BIM_DimHAS","BIM_DimPutnam")))-1),
                          list(width = '500px', targets = c(which(names(df) %in% c("BIM_Precision")))-1),
                          list(width = '100px', targets = 1:(ncol(df)-1)))
      ),
      filter = 'top'
    )
  })
  
  
  # Downloadable xlsx of selected dataset
  output$downloadBIM <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(today(),"-",""),"_Putnam_CEESPlorerBIM.xlsx")
    },
    content = function(file) {
      df_download <- baseBIM_shiny %>% select(-Index)
      if("SourceAvisCEESP" %in% names(df_download)){df_download$SourceAvisCEESP <- paste0("http://172.17.46.13/CEESPlorer/",df_download$SourceAvisCEESP)}
      write_xlsx(df_download, file)
    }
  )
  
}



## END
