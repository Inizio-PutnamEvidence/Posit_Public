#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Edit Table des associations Form
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  05 December 2023
#   LAST MODIFIED DATE    16 October 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of a form
# 
#--------------------------------------------------------------------------------------------------------



modal_dialog_assoc <- function(
    # Informations Générales
  ###########################################CENSORED######################
    # Prix
  ###########################################CENSORED######################
    # Posologie
  ###########################################CENSORED######################
    # Edit 
            edit, num_produit = 1, nb_produit = 2) {
  
  
  if (edit) {
    x <- "Submit Edits"
    id_button  <- "final_edit_assoc"
    title_form <- paste("Edit Table des associations -", nom_produit, "(date de validation CEESP :", paste0(date_val_CEESP, ")"))
  } else {
    if (num_produit < nb_produit){x <- "Next"}
    else {x <- "Add New Row"}
    id_button  <- paste0("final_edit_assoc_",num_produit)
    title_form <- paste("Add New Row - ", nom_produit, " - Product n°", num_produit)
  }
  
  shiny::modalDialog(
    title = title_form,
    textInput(inputId = "nom_produit_assoc",label = strong("Nom du produit de l'association :", style = "color:#5343a3"), value = nom_produit_assoc, width = "100%"),
    br(),
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #E2E2F2;  color:black}
    .tabbable > .nav > li > a[data-value='Réserves CEM'] {background-color: #FEBEFC; color:black}
    .tabbable > .nav > li > a[data-value='Réserves BIM'] {background-color: #FEBEFC; color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: #7949F0; color:white}
    .tabbable > .nav > li[class=active]    > a[data-value='Réserves CEM'] {background-color: #B502B1; color:white}
    .tabbable > .nav > li[class=active]    > a[data-value='Réserves BIM'] {background-color: #B502B1; color:white}
    ")),
    
    tabsetPanel(type = "tabs",
                {
                  #### --------------------------------------------------------  Prix  -------------------------------------------------------- ####
                  tabPanel("Prix",
                           h2("JO et ATU/Accès précoce"),
                           h3("Information"),
                           radioButtons(inputId = "ATU_presence", label = "ATU/Accès précoce", width = "100%", choices = c("Oui","Non"), selected = ATU_presence),
                           selectInput(inputId = "JO_QualifPrix", label = "Qualification du prix", width = "100%", choices = c(unique(sort(rv$df$JO_QualifPrix[which(rv$df$JO_QualifPrix != "Voir table des associations")])),"Autre (à préciser)"), selected = JO_QualifPrix),
                           conditionalPanel(condition = "input.JO_QualifPrix == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_JO_QualifPrix", label = em("Merci de préciser la qualification du prix", style = "color:#00CA9F"), width = "100%", value = autre_JO_QualifPrix)
                           ),
                           conditionalPanel(condition = "input.JO_QualifPrix != '-'",
                                            h3("Dates des JO"),
                                            em("Si la seule donnée de prix disponible au moment de l'évaluation est le 1er JO, merci de renseigner uniquement les informations dans les variables du 1er JO.", style = "color:#00CA9F"),
                                            h4("1er JO"),
                                            checkboxInput(inputId = "afficher_1erJO", label   = "Ajouter le 1er JO ?",value = afficher_1erJO),
                                            conditionalPanel(condition = "input.afficher_1erJO == 1",
                                                             if(JO_Source1erJO == "-")({
                                                               div(
                                                                 em("Vous n'avez pas encore de source téléchargée pour ce JO.", br(), style = "color:#00CA9F")
                                                               )
                                                             }),
                                                             if(JO_Source1erJO != "-")({
                                                               div(
                                                                 em("PDF actuel :",  style = "color:#00CA9F"),
                                                                 a("Source",target="_blank",href=JO_Source1erJO)
                                                               )
                                                             }),
                                                             column(3, dateInput(inputId = "JO_Date1erJO", label = "Date de publication", width = "100%", value = JO_Date1erJO)),
                                                             column(9, textInput(inputId = "url_JO_Source1erJO", label = "Nouveau lien URL", width = "100%", value = "")),
                                                             br(), 
                                                             h4("JO pré-évaluation"),
                                                             conditionalPanel(condition = "input.demande != 'Inscription'",
                                                                              checkboxInput(inputId = "afficher_JOvigueur", label = "Ajouter le JO pré-évaluation ?", value = afficher_JOvigueur),
                                                                              conditionalPanel(condition = "input.afficher_JOvigueur == 1",
                                                                                               if(JO_SourceJOpre == "-")({
                                                                                                 div(
                                                                                                   em("Vous n'avez pas encore de source téléchargée pour ce JO.", br(), style = "color:#00CA9F")
                                                                                                 )
                                                                                               }),
                                                                                               if(JO_SourceJOpre != "-")({
                                                                                                 div(
                                                                                                   em("PDF actuel :",  style = "color:#00CA9F"),
                                                                                                   a("Source",target="_blank",href=JO_SourceJOpre)
                                                                                                 )
                                                                                               }),
                                                                                               column(3, dateInput(inputId = "JO_DateJOpre", label = "Date de publication", width = "100%", value = JO_DateJOpre)),
                                                                                               column(9, textInput(inputId = "url_JO_SourceJOpre", label = "Nouveau lien URL", width = "100%", value = ""))
                                                                              ),
                                                             ),
                                                             br(),
                                                             h4("1er JO post-évaluation"),
                                                             checkboxInput(inputId = "afficher_1erJOpostavis", label = "Ajouter le 1er JO post-évaluation ?", value = afficher_1erJOpostavis),
                                                             conditionalPanel(condition = "input.afficher_1erJOpostavis == 1",
                                                                              if(JO_Source1erJOpost == "-")({
                                                                                div(
                                                                                  em("Vous n'avez pas encore de source téléchargée pour ce JO.", br(), style = "color:#00CA9F")
                                                                                )
                                                                              }),
                                                                              if(JO_Source1erJOpost != "-")({
                                                                                div(
                                                                                  em("PDF actuel :",  style = "color:#00CA9F"),
                                                                                  a("Source",target="_blank",href=JO_Source1erJOpost)
                                                                                )
                                                                              }),
                                                                              column(3,dateInput(inputId = "JO_Date1erJOpost", label = "Date de publication", width = "100%", value = JO_Date1erJOpost)),
                                                                              column(9,textInput(inputId = "url_JO_Source1erJOpost", label = "Nouveau lien URL", width = "100%", value = ""))
                                                             ),
                                            ),
                                            if(edit == TRUE){
                                              div(
                                                add_item_Prix(count_Prix = 1, value_JO_InfoPrix = get("JO_InfoPrix_1"), value_JO_Prix1erJO = get("JO_Prix1erJO_1"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_1"), value_JO_PrixPostJO = get("JO_PrixPostJO_1"), value_UCD_Flacon = get("UCD_Flacon_1"), value_UCD_Dose = get("UCD_Dose_1"), value_UCD_Volume = get("UCD_Volume_1"), value_UCD_Quantite = get("UCD_Quantite_1"), value_UCD_Nb = get("UCD_Nb_1"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_1")),
                                                add_item_Prix(count_Prix = 2, value_JO_InfoPrix = get("JO_InfoPrix_2"), value_JO_Prix1erJO = get("JO_Prix1erJO_2"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_2"), value_JO_PrixPostJO = get("JO_PrixPostJO_2"), value_UCD_Flacon = get("UCD_Flacon_2"), value_UCD_Dose = get("UCD_Dose_2"), value_UCD_Volume = get("UCD_Volume_2"), value_UCD_Quantite = get("UCD_Quantite_2"), value_UCD_Nb = get("UCD_Nb_2"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_2")),
                                                add_item_Prix(count_Prix = 3, value_JO_InfoPrix = get("JO_InfoPrix_3"), value_JO_Prix1erJO = get("JO_Prix1erJO_3"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_3"), value_JO_PrixPostJO = get("JO_PrixPostJO_3"), value_UCD_Flacon = get("UCD_Flacon_3"), value_UCD_Dose = get("UCD_Dose_3"), value_UCD_Volume = get("UCD_Volume_3"), value_UCD_Quantite = get("UCD_Quantite_3"), value_UCD_Nb = get("UCD_Nb_3"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_3")),
                                                add_item_Prix(count_Prix = 4, value_JO_InfoPrix = get("JO_InfoPrix_4"), value_JO_Prix1erJO = get("JO_Prix1erJO_4"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_4"), value_JO_PrixPostJO = get("JO_PrixPostJO_4"), value_UCD_Flacon = get("UCD_Flacon_4"), value_UCD_Dose = get("UCD_Dose_4"), value_UCD_Volume = get("UCD_Volume_4"), value_UCD_Quantite = get("UCD_Quantite_4"), value_UCD_Nb = get("UCD_Nb_4"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_4")),
                                                add_item_Prix(count_Prix = 5, value_JO_InfoPrix = get("JO_InfoPrix_5"), value_JO_Prix1erJO = get("JO_Prix1erJO_5"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_5"), value_JO_PrixPostJO = get("JO_PrixPostJO_5"), value_UCD_Flacon = get("UCD_Flacon_5"), value_UCD_Dose = get("UCD_Dose_5"), value_UCD_Volume = get("UCD_Volume_5"), value_UCD_Quantite = get("UCD_Quantite_5"), value_UCD_Nb = get("UCD_Nb_5"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_5")),
                                                add_item_Prix(count_Prix = 6, value_JO_InfoPrix = get("JO_InfoPrix_6"), value_JO_Prix1erJO = get("JO_Prix1erJO_6"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_6"), value_JO_PrixPostJO = get("JO_PrixPostJO_6"), value_UCD_Flacon = get("UCD_Flacon_6"), value_UCD_Dose = get("UCD_Dose_6"), value_UCD_Volume = get("UCD_Volume_6"), value_UCD_Quantite = get("UCD_Quantite_6"), value_UCD_Nb = get("UCD_Nb_6"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_6")),
                                                add_item_Prix(count_Prix = 7, value_JO_InfoPrix = get("JO_InfoPrix_7"), value_JO_Prix1erJO = get("JO_Prix1erJO_7"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_7"), value_JO_PrixPostJO = get("JO_PrixPostJO_7"), value_UCD_Flacon = get("UCD_Flacon_7"), value_UCD_Dose = get("UCD_Dose_7"), value_UCD_Volume = get("UCD_Volume_7"), value_UCD_Quantite = get("UCD_Quantite_7"), value_UCD_Nb = get("UCD_Nb_7"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_7")),
                                                add_item_Prix(count_Prix = 8, value_JO_InfoPrix = get("JO_InfoPrix_8"), value_JO_Prix1erJO = get("JO_Prix1erJO_8"), value_JO_PrixVigueurJO = get("JO_PrixVigueurJO_8"), value_JO_PrixPostJO = get("JO_PrixPostJO_8"), value_UCD_Flacon = get("UCD_Flacon_8"), value_UCD_Dose = get("UCD_Dose_8"), value_UCD_Volume = get("UCD_Volume_8"), value_UCD_Quantite = get("UCD_Quantite_8"), value_UCD_Nb = get("UCD_Nb_8"), value_ATU_IndemMaxUCD = get("ATU_IndemMaxUCD_8")),
                                              )
                                            }
                                            else {
                                              div(
                                                 div(id = "modal_ui_Prix",
                                                    add_item_Prix(count_Prix             = count_Prix(),
                                                                  value_JO_InfoPrix      = get(paste0("JO_InfoPrix_", count_Prix())),
                                                                  value_JO_Prix1erJO     = get(paste0("JO_Prix1erJO_", count_Prix())),
                                                                  value_JO_PrixVigueurJO = get(paste0("JO_PrixVigueurJO_", count_Prix())),
                                                                  value_JO_PrixPostJO    = get(paste0("JO_PrixPostJO_", count_Prix())),
                                                                  value_UCD_Flacon       = get(paste0("UCD_Flacon_", count_Prix())),
                                                                  value_UCD_Dose         = get(paste0("UCD_Dose_", count_Prix())),  
                                                                  value_UCD_Volume       = get(paste0("UCD_Volume_", count_Prix())),
                                                                  value_UCD_Quantite     = get(paste0("UCD_Quantite_", count_Prix())),
                                                                  value_UCD_Nb           = get(paste0("UCD_Nb_", count_Prix())),
                                                                  value_ATU_IndemMaxUCD  = get(paste0("ATU_IndemMaxUCD_", count_Prix())))
                                                ), 
                                                if(count_Prix() < 8){
                                                  actionButton(inputId = "add_entry_Prix",
                                                               style  = "border: 0px",
                                                               label  = NULL,
                                                               icon("circle-plus")
                                                  )}
                                              )
                                            }
                           )
                  )
                },
                
                {
                  #### --------------------------------------------------------  Posologie  -------------------------------------------------------- ####
                  tabPanel("Posologie", 
                           conditionalPanel(condition = "input.type_produit == 'Mono'", h2 ("Traitement")),
                           conditionalPanel(condition = "input.type_produit == 'Asso'", h2 ("Association de Traitements")),
                           conditionalPanel(condition = "input.type_produit == 'Dispositif Médical'", h2 ("Dispositif Médical")),
                           conditionalPanel(condition = "input.type_produit == 'Vaccin'", h2 ("Vaccin")),
                           
                           h3("Information Posologie"),
                           conditionalPanel(condition = "input.type_produit != 'Vaccin' && input.type_produit != 'Dispositif Médical'",
                                            radioButtons(inputId = "sel_traitement_chronique", label = "Traitement chronique", choices = c("Oui","Non"), selected = sel_traitement_chronique, width = "100%"),
                           ),
                           conditionalPanel(condition = "input.type_produit == 'Dispositif Médical'",
                                            radioButtons(inputId = "sel_utilisation_dm", label = "Utilisation à long terme ?", choices = c("Oui","Non"), selected = sel_utilisation_dm, width = "100%"),
                           ),
                           conditionalPanel(condition = "input.type_produit != 'Vaccin'",
                                            textAreaInput(inputId = "horizon", label = "Horizon du traitement", width = "100%", value = horizon),
                           ),
                           em("Pour écrire une puissance, merci d'utiliser le format :", style = "color:#00CA9F"), strong("x10^N", style = "color:#00CA9F"),em("où N est la puissance.", style = "color:#00CA9F"), br(),  
                           textAreaInput(inputId = "posologie_detail", label = "Détail de la posologie", width = "100%", value = posologie_detail),
                           conditionalPanel(condition = "input.type_produit != 'Dispositif Médical'",
                                            div(
                                              h4("Source du RCP"),
                                              if(source_rcp == "-")({
                                                div(
                                                  em("Vous n'avez pas encore de source téléchargée pour ce RCP.", br(), style = "color:#00CA9F")
                                                )
                                              }),
                                              if(source_rcp != "-")({
                                                div(
                                                  em("PDF actuel :",  style = "color:#00CA9F"),
                                                  a("Source RCP",target="_blank",href=source_rcp)
                                                )
                                              }),
                                              textInput(inputId = "url_source_rcp", label = "Nouveau lien du document pdf", width = "100%", value = "")
                                            )
                           ),
                           conditionalPanel(condition = "(input.type_produit != 'Dispositif Médical' && input.type_produit != 'Vaccin') && input.sel_traitement_chronique == 'Oui'",
                                            radioButtons(inputId = "escalade_dose", label = "Escalade de dose", choices = c("Oui","Non"), selected = sel_escalade_dose, width = "100%"),
                                            conditionalPanel(condition = "input.escalade_dose == 'Non'",
                                                             if (edit == TRUE){
                                                               div(
                                                                 add_item_PTC(count_PTC = 1, value_PTC_typedose = get("sel_type_poso_1"), value_PTC_cate_age = get("sel_cate_age_1"), value_PTC_age = get("sel_age_1"), value_PTC_dose = get("PTC_DosePrise_1"), value_PTC_grandunite = get("PTC_GrandeurUniteDose_1"), value_PTC_unite = get("PTC_UniteDose_1"), value_PTC_nb = get("PTC_NbPrise_1"), value_PTC_nbadm = get("PTC_NbAdmCycle_1"), value_PTC_duree = get("PTC_DureeCycle_1")),
                                                                 add_item_PTC(count_PTC = 2, value_PTC_typedose = get("sel_type_poso_2"), value_PTC_cate_age = get("sel_cate_age_2"), value_PTC_age = get("sel_age_2"), value_PTC_dose = get("PTC_DosePrise_2"), value_PTC_grandunite = get("PTC_GrandeurUniteDose_2"), value_PTC_unite = get("PTC_UniteDose_2"), value_PTC_nb = get("PTC_NbPrise_2"), value_PTC_nbadm = get("PTC_NbAdmCycle_2"), value_PTC_duree = get("PTC_DureeCycle_2")),
                                                                 add_item_PTC(count_PTC = 3, value_PTC_typedose = get("sel_type_poso_3"), value_PTC_cate_age = get("sel_cate_age_3"), value_PTC_age = get("sel_age_3"), value_PTC_dose = get("PTC_DosePrise_3"), value_PTC_grandunite = get("PTC_GrandeurUniteDose_3"), value_PTC_unite = get("PTC_UniteDose_3"), value_PTC_nb = get("PTC_NbPrise_3"), value_PTC_nbadm = get("PTC_NbAdmCycle_3"), value_PTC_duree = get("PTC_DureeCycle_3")),
                                                                 add_item_PTC(count_PTC = 4, value_PTC_typedose = get("sel_type_poso_4"), value_PTC_cate_age = get("sel_cate_age_4"), value_PTC_age = get("sel_age_4"), value_PTC_dose = get("PTC_DosePrise_4"), value_PTC_grandunite = get("PTC_GrandeurUniteDose_4"), value_PTC_unite = get("PTC_UniteDose_4"), value_PTC_nb = get("PTC_NbPrise_4"), value_PTC_nbadm = get("PTC_NbAdmCycle_4"), value_PTC_duree = get("PTC_DureeCycle_4"))
                                                               )
                                                             }
                                                             else {
                                                               div(
                                                                 div(id = "modal_ui_PTC",
                                                                     add_item_PTC(count_PTC            = count_PTC(), 
                                                                                  value_PTC_typedose   = get(paste0("sel_type_poso_", count_PTC())), 
                                                                                  value_PTC_cate_age   = get(paste0("sel_cate_age_", count_PTC())), 
                                                                                  value_PTC_age        = get(paste0("sel_age_", count_PTC())), 
                                                                                  value_PTC_dose       = get(paste0("PTC_DosePrise_", count_PTC())), 
                                                                                  value_PTC_grandunite = get(paste0("PTC_GrandeurUniteDose_", count_PTC())), 
                                                                                  value_PTC_unite      = get(paste0("PTC_UniteDose_", count_PTC())), 
                                                                                  value_PTC_nb         = get(paste0("PTC_NbPrise_", count_PTC())),
                                                                                  value_PTC_nbadm      = get(paste0("PTC_NbAdmCycle_", count_PTC())),
                                                                                  value_PTC_duree      = get(paste0("PTC_DureeCycle_", count_PTC())))
                                                                 ),
                                                                 if(count_PTC() < 4){
                                                                   actionButton(inputId = "add_entry_PTC",
                                                                                style  = "border: 0px",
                                                                                label  = NULL,
                                                                                icon("circle-plus")
                                                                   )}
                                                               )
                                                             }
                                            ),
                                            conditionalPanel(condition = "input.escalade_dose == 'Oui'",
                                                             h3("Escalade de Dose"),
                                                             h4("Phase d'attaque"),
                                                             selectInput(inputId = "PTCED_TypeDoseAttaque", label = "Type de posologie", choices = c("-","Dose Fixe","Dose Poids","Dose Surface Corporelle"), selected = sel_type_poso_attaque, width = "100%"),
                                                             conditionalPanel(condition = "input.PTCED_TypeDoseAttaque == 'Dose Poids' || input.PTCED_TypeDoseAttaque == 'Dose Surface Corporelle'",
                                                                              selectInput(inputId = "PTCED_Attaque_cate_age", label = "Catégorie d'âge", choices = c("Adulte","Adolescent","Enfant","Nourrison"), selected = sel_cate_age_attaque, width = "100%"),
                                                                              conditionalPanel(condition = "input.PTCED_Attaque_cate_age != 'Adulte'",
                                                                                               div(
                                                                                                 strong("Age (années)", br()),
                                                                                                 em("Considérer la moyenne ou l'âge moyen pour des intervalles d'âge. Exemple - si la population est âgée de 6-11 ans, sélectionner 9 ans dans la liste déroulante."),
                                                                                                 
                                                                                                 conditionalPanel(condition = "input.PTCED_Attaque_cate_age == 'Adolescent'",
                                                                                                                  selectInput(inputId = "PTCED_Attaque_age_ado", label = NULL, choices = c(12:17,"Inconnu"), selected = sel_age_attaque, width = "100%")
                                                                                                 ),
                                                                                                 conditionalPanel(condition = "input.PTCED_Attaque_cate_age == 'Enfant'",
                                                                                                                  selectInput(inputId = "PTCED_Attaque_age_enf", label = NULL, choices = c(3:11,"Inconnu"), selected = sel_age_attaque, width = "100%")
                                                                                                 ),
                                                                                                 conditionalPanel(condition = "input.PTCED_Attaque_cate_age == 'Nourrisson'",
                                                                                                                  selectInput(inputId = "PTCED_Attaque_age_nour", label = NULL, choices = c(0:2,"Inconnu"), selected = sel_age_attaque, width = "100%")
                                                                                                 )
                                                                                               )
                                                                              )
                                                             ),
                                                             if (edit == TRUE){
                                                               div(
                                                                 add_item_PTCED(count_PTCED = 1, value_PTCED_dose = get("PTCED_DosePrise_1"), value_PTCED_grandunite = get("PTCED_GrandeurUniteDose_1"), value_PTCED_unite = get("PTCED_UniteDose_1"), value_PTCED_nbdose = get("PTCED_NbPrise_1"), value_PTCED_nbadm = get("PTCED_NbAdmCycleAttaque_1"), value_PTCED_duree = get("PTCED_DureeCycleAttaque_1"), value_PTCED_nbcycle = get("PTCED_NbCycleAttaque_1")),
                                                                 add_item_PTCED(count_PTCED = 2, value_PTCED_dose = get("PTCED_DosePrise_2"), value_PTCED_grandunite = get("PTCED_GrandeurUniteDose_2"), value_PTCED_unite = get("PTCED_UniteDose_2"), value_PTCED_nbdose = get("PTCED_NbPrise_2"), value_PTCED_nbadm = get("PTCED_NbAdmCycleAttaque_2"), value_PTCED_duree = get("PTCED_DureeCycleAttaque_2"), value_PTCED_nbcycle = get("PTCED_NbCycleAttaque_2")),
                                                                 add_item_PTCED(count_PTCED = 3, value_PTCED_dose = get("PTCED_DosePrise_3"), value_PTCED_grandunite = get("PTCED_GrandeurUniteDose_3"), value_PTCED_unite = get("PTCED_UniteDose_3"), value_PTCED_nbdose = get("PTCED_NbPrise_3"), value_PTCED_nbadm = get("PTCED_NbAdmCycleAttaque_3"), value_PTCED_duree = get("PTCED_DureeCycleAttaque_3"), value_PTCED_nbcycle = get("PTCED_NbCycleAttaque_3")),
                                                                 add_item_PTCED(count_PTCED = 4, value_PTCED_dose = get("PTCED_DosePrise_4"), value_PTCED_grandunite = get("PTCED_GrandeurUniteDose_4"), value_PTCED_unite = get("PTCED_UniteDose_4"), value_PTCED_nbdose = get("PTCED_NbPrise_4"), value_PTCED_nbadm = get("PTCED_NbAdmCycleAttaque_4"), value_PTCED_duree = get("PTCED_DureeCycleAttaque_4"), value_PTCED_nbcycle = get("PTCED_NbCycleAttaque_4"))
                                                               )
                                                             }
                                                             else {
                                                               div(
                                                                 div(id = "modal_ui_PTCED",
                                                                     add_item_PTCED(count_PTCED            = count_PTCED(), 
                                                                                    value_PTCED_dose       = get(paste0("PTCED_DosePrise_", count_PTCED())), 
                                                                                    value_PTCED_grandunite = get(paste0("PTCED_GrandeurUniteDose_", count_PTCED())), 
                                                                                    value_PTCED_unite      = get(paste0("PTCED_UniteDose_", count_PTCED())), 
                                                                                    value_PTCED_nbdose     = get(paste0("PTCED_NbPrise_", count_PTCED())),
                                                                                    value_PTCED_nbadm      = get(paste0("PTCED_NbAdmCycleAttaque_", count_PTCED())),
                                                                                    value_PTCED_duree      = get(paste0("PTCED_DureeCycleAttaque_", count_PTCED())),
                                                                                    value_PTCED_nbcycle    = get(paste0("PTCED_NbCycleAttaque_", count_PTCED())))
                                                                 ),
                                                                 if(count_PTCED() < 4){
                                                                   actionButton(inputId = "add_entry_PTCED",
                                                                                style   = "border: 0px",
                                                                                label   = NULL,
                                                                                icon("circle-plus")
                                                                   )}
                                                               )
                                                             },
                                                             
                                                             h4("Phase d'entretien"),
                                                             selectInput(inputId = "PTCED_TypeDoseEntretien", label = "Type de posologie", choices = c("-","Dose Fixe","Dose Poids","Dose Surface Corporelle"), selected = sel_type_poso_entretien, width = "100%"),
                                                             conditionalPanel(condition = "input.PTCED_TypeDoseEntretien == 'Dose Poids' || input.PTCED_TypeDoseEntretien == 'Dose Surface Corporelle'",
                                                                              selectInput(inputId = "PTCED_Entretien_cate_age", label = "Catégorie d'âge", choices = c("Adulte","Adolescent","Enfant","Nourrison"), selected = sel_cate_age_entretien, width = "100%"),
                                                                              conditionalPanel(condition = "input.PTCED_Entretien_cate_age != 'Adulte'",
                                                                                               div(
                                                                                                 strong("Age (années)", br()),
                                                                                                 em("Considérer la moyenne ou l'âge moyen pour des intervalles d'âge. Exemple - si la population est âgée de 6-11 ans, sélectionner 9 ans dans la liste déroulante."),
                                                                                                 conditionalPanel(condition = "input.PTCED_Entretien_cate_age == 'Adolescent'",
                                                                                                                  selectInput(inputId = "PTCED_Entretien_age_ado", label = NULL, choices = c(12:17,"Inconnu"), selected = sel_age_entretien, width = "100%")
                                                                                                 ),
                                                                                                 conditionalPanel(condition = "input.PTCED_Entretien_cate_age == 'Enfant'",
                                                                                                                  selectInput(inputId = "PTCED_Entretien_age_enf", label = NULL, choices = c(3:11,"Inconnu"), selected = sel_age_entretien, width = "100%")
                                                                                                 ),
                                                                                                 conditionalPanel(condition = "input.PTCED_Entretien_cate_age == 'Nourrisson'",
                                                                                                                  selectInput(inputId = "PTCED_Entretien_age_nour", label = NULL, choices = c(0:2,"Inconnu"), selected = sel_age_entretien, width = "100%")
                                                                                                 )
                                                                                               )
                                                                              )
                                                             ),
                                                             numericInput(inputId = "PTCED_DosePriseEntretien", label = "Dose par unité de prise pendant la phase d'entretien", width = "100%", value = PTCED_DosePriseEntretien),
                                                             div(
                                                               strong("Grandeur d'unité", br()),
                                                               em("p = Pico; n = Nano; μ = Micro; m = Mili; k = Kilo; M = Mega; G = Giga"),
                                                               selectInput(inputId = "PTCED_GrandeurUniteDoseEntretien", label = NULL, choices = list_grandeurunite, selected = PTCED_GrandeurUniteDoseEntretien, width = "100%")
                                                             ),
                                                             conditionalPanel(condition = "input.PTCED_TypeDoseEntretien == 'Dose Fixe'",
                                                                              div(
                                                                                strong("Unité", br()),
                                                                                em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"),
                                                                                selectInput(inputId = "PTCED_UniteDoseEntretienFixe", label = NULL, choices = list_unitedosefixe, selected = PTCED_UniteDoseEntretien, width = "100%")
                                                                              )
                                                             ),
                                                             conditionalPanel(condition = "input.PTCED_TypeDoseEntretien == 'Dose Poids'",
                                                                              div(
                                                                                strong("Unité", br()),
                                                                                em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel; c = Cellule; vg = Vector genomes"),
                                                                                selectInput(inputId = "PTCED_UniteDoseEntretienPoids", label = NULL, choices = list_unitedosepoids, selected = PTCED_UniteDoseEntretien, width = "100%")
                                                                              )
                                                             ),
                                                             conditionalPanel(condition = "input.PTCED_TypeDoseEntretien == 'Dose Surface Corporelle'",
                                                                              div(
                                                                                strong("Unité", br()),
                                                                                em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"),
                                                                                selectInput(inputId = "PTCED_UniteDoseEntretienSurf", label = NULL, choices = list_unitedosesurf, selected = PTCED_UniteDoseEntretien, width = "100%")
                                                                              )
                                                             ),
                                                             textInput(inputId = "PTCED_NbPriseEntretien", label = "Nombre d'unité prise pendant la phase d'entretien", width = "100%", value = PTCED_NbPriseEntretien),
                                                             textInput(inputId = "PTCED_NbAdmCycleEntretien", label = "Nombre d'administration par cycle pendant la phase d'entretien", width = "100%", value = PTCED_NbAdmCycleEntretien),
                                                             textInput(inputId = "PTCED_DureeCycleEntretien", label = "Durée d'un cycle pendant la phase d'entretien (jours)", width = "100%", value = PTCED_DureeCycleEntretien)
                                            )
                           ),
                           conditionalPanel(condition = "(input.type_produit != 'Dispositif Médical' && input.type_produit != 'Vaccin') && input.sel_traitement_chronique == 'Non'",
                                            h3("Posologie : Traitement Non Chronique"),
                                            selectInput(inputId = "PTNC_TypeDose", label = "Type de posologie", choices = c("-","Dose Fixe","Dose Poids","Dose Surface Corporelle","One Shot"), selected = sel_type_produit_NC, width = "100%"),
                                            conditionalPanel(condition = "input.PTNC_TypeDose != 'One Shot'",
                                                             conditionalPanel(condition = "input.PTNC_TypeDose == 'Dose Poids' || input.PTNC_TypeDose == 'Dose Surface Corporelle'",
                                                                              selectInput("PTNC_cate_age", "Catégorie d'âge", choices = c("Adulte","Adolescent","Enfant","Nourrison"), selected = sel_cate_age_NC, width = "100%"),
                                                                              conditionalPanel(condition = "input.PTNC_cate_age != 'Adulte'",
                                                                                               div(
                                                                                                 strong("Age (années)", br()),
                                                                                                 em("Considérer la moyenne ou l'âge moyen pour des intervalles d'âge. Exemple - si la population est âgée de 6-11 ans, sélectionner 9 ans dans la liste déroulante."),
                                                                                                 conditionalPanel(condition =  "input.PTNC_cate_age == 'Adolescent'",
                                                                                                                  selectInput(inputId = "PTNC_age_ado", label = NULL, choices = c(12:17,"Inconnu"), selected = sel_age_NC, width = "100%")
                                                                                                 ), 
                                                                                                 conditionalPanel(condition = "input.PTNC_cate_age == 'Enfant'",
                                                                                                                  selectInput(inputId = "PTNC_age_enf", label = NULL, choices = c(3:11,"Inconnu"), selected = sel_age_NC, width = "100%")
                                                                                                 ), 
                                                                                                 conditionalPanel(condition = "input.PTNC_cate_age == 'Nourrisson'",
                                                                                                                  selectInput(inputId = "PTNC_age_nour", label = NULL, choices = c(0:2,"Inconnu"), selected = sel_age_NC, width = "100%")
                                                                                                 )
                                                                                               )
                                                                              )
                                                             ),
                                                             numericInput(inputId = "PTNC_DosePrise", label = "Dose par unité de prise", width = "100%", value = PTNC_DosePrise),
                                                             div(
                                                               strong("Grandeur d'unité", br()),
                                                               em("p = Pico; n = Nano; μ = Micro; m = Mili; k = Kilo; M = Mega; G = Giga"),
                                                               selectInput(inputId = "PTNC_GrandeurUniteDose", label = NULL, choices = list_grandeurunite, selected = PTNC_GrandeurUniteDose, width = "100%")
                                                             ),
                                                             conditionalPanel(condition = "input.PTNC_TypeDose == 'Dose Fixe'",
                                                                              div(
                                                                                strong("Unité", br()),
                                                                                em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"),
                                                                                selectInput(inputId = "PTNC_UniteDoseFixe", label = NULL, choices = list_unitedosefixe, selected = PTNC_UniteDose, width = "100%")
                                                                              )
                                                             ),
                                                             conditionalPanel(condition = "input.PTNC_TypeDose == 'Dose Poids'",
                                                                              div(
                                                                                strong("Unité", br()),
                                                                                em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel; c = Cellule; vg = Vector genomes"), 
                                                                                selectInput(inputId = "PTNC_UniteDosePoids", label = NULL, choices = list_unitedosepoids, selected = PTNC_UniteDose, width = "100%")
                                                                              )
                                                             ),
                                                             conditionalPanel(condition = "input.PTNC_TypeDose == 'Dose Surface Corporelle'",
                                                                              div(
                                                                                strong("Unité", br()),
                                                                                em("g = Gramme; L = Litre; u = Unité; inj = Injection; Bq = Becquerel"),
                                                                                selectInput(inputId = "PTNC_UniteDoseSurf", label = NULL, choices = list_unitedosesurf, selected = PTNC_UniteDose, width = "100%")
                                                                              )
                                                             ),
                                                             textInput(inputId = "PTNC_NbPriseCycle", label = "Nombre d'unité prise par cycle / Nombre d'administration par cycle", width = "100%", value = PTNC_NbPriseCycle),
                                                             textInput(inputId = "PTNC_NbCycle", label = "Nombre de cycle", width = "100%", value = PTNC_NbCycle)
                                            ),
                                            conditionalPanel(condition = "input.PTNC_TypeDose == 'One Shot'",
                                                             textInput(inputId = "PTNC_NbPriseCycleOneShot", label = "Nombre d'administration", width = "100%", value = PTNC_NbPriseCycleOneShot),
                                            ),
                           ), 
                           conditionalPanel(condition = "input.type_produit == 'Dispositif Médical' && input.sel_utilisation_dm == 'Oui'",
                                            textInput(inputId = "PDMNC_Nbutil", label = "Nombre d'unité utilisée", width = "100%", value = PDMNC_Nbutil),
                           ),
                           conditionalPanel(condition = "input.type_produit == 'Dispositif Médical' && input.sel_utilisation_dm == 'Non'",
                                            textInput(inputId = "PDMC_Nbutil", label = "Nombre d'utilisation quotidienne", width = "100%", value = PDMC_Nbutil),
                                            textInput(inputId = "PDMC_Dureeutil", label = "Durée du traitement (jours)", width = "100%", value = PDMC_Dureeutil),
                           ), 
                           conditionalPanel(condition = "input.type_produit == 'Vaccin'",
                                            textInput(inputId = "PVC_Nbinj", label = "Nombre d'injection", width = "100%", value = PVC_Nbinj),
                           ),
                  )},
                

    ),
    
    size = "l",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "dismiss_modal_assoc", label = "Close", class = "btn-danger"),
        ),
        div(
          class = "col-md-6 text-right",
          shiny::actionButton(inputId = id_button, label = x, icon = shiny::icon("edit"), class = "btn-info"),
        )
      )
    )
  ) %>% shiny::showModal()
  
}



## END
