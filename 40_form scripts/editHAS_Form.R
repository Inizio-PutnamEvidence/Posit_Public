#--------------------------------------------------------------------------------------------------------
#
#   PROJECT               Internal projects/23-INT04DBHTA_Database HTA FR
#   PROGRAM               Edit HAS Form  
#   AUTHOR(S)             Margaux MORIN (MMO) / Charlene TOURNIER (CTO)
#   DATE                  09 August 2023
#   LAST MODIFIED DATE    16 October 2024
#   RSTUDIO VERSION       2021.11.01 (MMO) / 2023.03.0 (CTO)
#   R VERSION             4.2.2 (2022-10-31 ucrt) (MMO) / 4.3.1 (2023-06-16 ucrt) (CTO)
#   DESCRIPTION           Creation of a form
# 
#--------------------------------------------------------------------------------------------------------



modal_dialog <- function(
    # Informations Générales
  ###########################################CENSORED######################
    # Evaluation HAS
  ###########################################CENSORED######################
    # Population Cible
  ###########################################CENSORED######################
    # Prix
  ###########################################CENSORED######################
    # Posologie                  
  ###########################################CENSORED######################
    # Edit 
            edit,
    # Edit module
            edit_module ) {
  
  
  if (edit) {
    x <- "Submit Edits"
    title_form = paste("Edit -", nom_produit, "(date de validation CEESP :", paste0(date_val_CEESP, ")"))
  } else {
    x <- "Add New Row"
    title_form <- "Add New Row"
  }
  
  if (edit_module == FALSE) {
    x <- "Submit Edits"
    title_form = paste("Edit for QC -", nom_produit, "(date de validation CEESP :", paste0(date_val_CEESP, ")"))
    comment_user = rv_HAS_to_qc$df$CommentsEdit[rv_HAS_to_qc$dt_row ]
  }
  
  shiny::modalDialog(
    title = title_form,
    if(edit_module == FALSE){
      renderUI(HTML(paste(strong("Commentaire de l'auteur :", style = "color:#5343a3"), comment_user, '<br/><br/>')))
    },
    # MMO : Code temporaire le temps que toutes les posologies des DM soient modifiées
    if(sel_type_produit == "Dispositif Médical"){
      renderUI(HTML(paste(strong("Attention, pour modifier cette ligne vous devez mettre à jour l'onglet 'Posologie'.", style = "color:#800000"), '<br/><br/>')))
    },
    #
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #E2E2F2;  color:black}
    .tabbable > .nav > li > a[data-value='Réserves CEM'] {background-color: #FEBEFC; color:black}
    .tabbable > .nav > li > a[data-value='Réserves BIM'] {background-color: #FEBEFC; color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: #7949F0; color:white}
    .tabbable > .nav > li[class=active]    > a[data-value='Réserves CEM'] {background-color: #B502B1; color:white}
    .tabbable > .nav > li[class=active]    > a[data-value='Réserves BIM'] {background-color: #B502B1; color:white}
    ")),
    
    tabsetPanel(type = "tabs",id = "tabsform",
                {
                  #### --------------------------------------------------------  Informations Générales  -------------------------------------------------------- ####
                  tabPanel("Informations Générales",
                           h2("Information Pharmaceutique"),
                           textInput(inputId = "nom_produit", label = "Nom du produit", width = "100%", value = nom_produit),
                           textInput(inputId = "molecule", label = "Molécule", width = "100%", value = molecule),
                           selectInput(inputId = "sponsor", label = "Sponsor", width = "100%", choices = c("-",sort(unique(rv$df$Sponsor)),"Autre (à préciser)"), selected = sponsor),
                           conditionalPanel(condition = "input.sponsor == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_sponsor", label = em("Merci de préciser le sponsor", style = "color:#00CA9F"), width = "100%", value = autre_sponsor),
                           ),
                           radioButtons(inputId = "LES", label = "Inscription sur la liste en sus de la T2A", width = "100%", choices = c("Oui","Non"), selected = LES),
                           dateInput(inputId = "date_AMM", label = "Date d'obtention de l'AMM", width = "100%", value = ifelse(is.na(date_AMM),as.Date("01012000", format = "%d%m%Y"), as.Date(date_AMM))),
                           radioButtons(inputId = "statut_med_orph", label = "Statut de médicament orphelin", width = "100%", choices = c("Oui","Non"), selected = statut_med_orph),
                           conditionalPanel(condition = "input.statut_med_orph == 'Oui'",
                                            dateInput(inputId = "date_COMP", label = "Date de désignation de statut de médicament orphelin", width = "100%", value = date_COMP),
                           ),
                           textInput(inputId = "ATC", label = "Code ATC", width = "100%", value = ATC),
                           dateInput(inputId = "date_elig_HAS", label = "Date de décision d'éligibilité par la HAS", width = "100%", value = date_elig_HAS),
                           dateInput(inputId = "date_CT", label = "Date de l'avis CT", width = "100%", value = date_CT),
                           dateInput(inputId = "date_val_CEESP", label = "Date de validation par la CEESP", width = "100%", value = date_val_CEESP),
                           selectInput(inputId = "demande", label = "Type de demande", width = "100%", choices = c("-",sort(unique(rv$df$Demande)),"Autre (à préciser)"), selected = demande),
                           conditionalPanel(condition = "input.demande == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_demande", label = em("Merci de préciser le type de demande", style = "color:#00CA9F"), width = "100%", value = autre_demande),
                           ),
                           conditionalPanel(condition = "input.demande == 'Réévaluation'",
                                            textInput(inputId = "motif_reeval", label = "Motif de la réévaluation", width = "100%", value = motif_reeval),
                           ),
                           textAreaInput(inputId = "indication", label = "Indication faisant l'objet de l'évaluation", width = "100%", value = indication),
                           selectInput(inputId = "aire_thera_HAS", label = "Aire thérapeutique mentionnée par la HAS", width = "100%", choices = c(unique(sort(rv$df$AireTheraHAS)),"Autre (à préciser)"), selected = aire_thera_HAS),
                           conditionalPanel(condition = "input.aire_thera_HAS == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_aire_thera_HAS", label = em("Merci de préciser l'aire thérapeutique mentionnée par la HAS", style = "color:#00CA9F"), width = "100%", value = autre_aire_thera_HAS),
                           ),
                           selectInput(inputId = "aire_thera_Putnam", label = "Aire thérapeutique corrigée par Putnam", width = "100%", choices = c("-",unique(sort(rv$df$AireTheraPutnam)),"Autre (à préciser)"), selected = aire_thera_Putnam),
                           conditionalPanel(condition = "input.aire_thera_Putnam == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_aire_thera_Putnam", label = em("Merci de préciser l'aire thérapeutique corrigée par Putnam", style = "color:#00CA9F"), width = "100%", value = autre_aire_thera_Putnam),
                           ),
                           selectInput(inputId = "aire_thera2_Putnam", label = "Aire thérapeutique secondaire précisée", width = "100%", choices = c("-",unique(sort(rv$df$AireThera2Putnam)),"Autre (à préciser)"), selected = aire_thera2_Putnam),
                           conditionalPanel(condition = "input.aire_thera2_Putnam == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_aire_thera2_Putnam", label = em("Merci de préciser l'aire thérapeutique secondaire précisée", style = "color:#00CA9F"), width = "100%", value = autre_aire_thera2_Putnam),
                           ),
                           radioButtons(inputId = "maladie_rare", label = "Maladie rare", width = "100%", choices = c("Oui","Non"), selected = maladie_rare),
                           selectInput(inputId = "ligne", label = "Ligne de traitement", width = "100%", choices = c(sort(unique(rv$df$Ligne)),"Autre (à préciser)"), selected = ligne),
                           conditionalPanel(condition = "input.ligne == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_ligne", label = em("Merci de préciser la ligne de traitement", style = "color:#00CA9F"), width = "100%", value = autre_ligne),
                           ),
                           selectInput(inputId = "type_produit", label = "Type de produit", choices = c("-",sort(unique(rv$df$TypeProduit))), width = "100%", selected = sel_type_produit),
                           conditionalPanel(condition = "input.type_produit == 'Asso'",
                                            radioButtons(inputId = "Asso_Several_Products", label = HTML('<span style="color: #7949F0;">Association composée de plusieurs produits hors GHS</span>'), width = "100%", choices = c("Oui","Non"), selected = asso_oui_non),
                                            conditionalPanel(condition = "input.Asso_Several_Products == 'Oui'",
                                                             numericInput(inputId = "Nb_Products_Asso", label =HTML('<span style="color: #7949F0;">Nombre de produits dans l&#39;association</span>'), width = "100%", min=2, value = nb_prod_asso),
                                                             em("Vous pourrez remplir les informations de prix et de posologie à la page suivante.", style = "color:#7949F0"),
                                                             br(),
                                                             em("Pensez à bien remplir tous les onglets présents (Informations Générales, Evaluation HAS, Population Cible, et les Réserves CEM/BIM) avant de valider la page pour remplir les informations de prix et de posologie.
                                                                 Il n'est pas possible de revenir en arrière, une fois la page validée.", style = "color:#7949F0"),
                                            )
                           ),
                           radioButtons(inputId = "sel_horizon_temp", label = "Type de l'horizon temporel", width = "100%", choices = c("Durée déterminée","Vie entière"), selected = sel_horizon_temp),
                           textAreaInput(inputId = "horizon_temp", label = "Durée de l'horizon temporel", width = "100%", value = horizon_temp),
                           textAreaInput(inputId = "type_model_HAS", label = "Type de modèle mentionné par la HAS", width = "100%", value = type_model_HAS),
                           selectInput(inputId = "type_model_Putnam", label = "Type de modèle corrigé par Putnam", width = "100%", choices = c(unique(sort(rv$df$TypeModelePutnam)),"Autre (à préciser)"), selected = type_model_Putnam),
                           conditionalPanel(condition = "input.type_model_Putnam == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_type_model_Putnam", label = em("Merci de préciser le type de modèle corrigé par Putnam", style = "color:#00CA9F"), width = "100%", value = autre_type_model_Putnam),
                           ),
                           selectInput(inputId = "meth_comp_indirecte", label = "Méthode de comparaison indirecte utilisée", width = "100%", choices = c(unique(sort(rv$df$MethodeComparaisonIndirecte)),"Autre (à préciser)"), selected = meth_comp_indirecte),
                           conditionalPanel(condition = "input.meth_comp_indirecte == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_meth_comp_indirecte", label = em("Merci de préciser le type de méthode de comparaison indirecte utilisée", style = "color:#00CA9F"), width = "100%", value = autre_meth_comp_indirecte),
                           ),
                           radioButtons(inputId = "audition_gp", label = "Audition", width = "100%", choices = c("Oui","Non"), selected = audition_gp),
                           div(
                             h4("Source de l'avis CT"),
                             if(source_ct == "-")({
                               div(
                                 em("Vous n'avez pas encore de source téléchargée pour cet avis.", br(), style = "color:#00CA9F")
                               )
                             }),
                             if(source_ct != "-")({
                               div(
                                 em("PDF actuel :", style = "color:#00CA9F"),
                                 a("Source avis CT", target="_blank", href=source_ct)
                               )
                             }),
                             textInput(inputId = "url_source_ct", label = "Nouveau lien du document pdf", width = "100%", value = "")
                           ),
                           div(
                             h4("Source de l'avis CEESP"),
                             if(source_avis_CEESP == "-")({
                               div(
                                 em("Vous n'avez pas encore de source téléchargée pour cet avis.", br(), style = "color:#00CA9F")
                               )
                             }),
                             if(source_avis_CEESP != "-")({
                               div(
                                 em("PDF actuel :", style = "color:#00CA9F"),
                                 a("Source avis CEESP", target="_blank", href=source_avis_CEESP)
                               )
                             }),
                             textInput(inputId = "url_source_avis_CEESP", label = "Nouveau lien du document pdf", width = "100%", value = "")
                           )
                  )
                },

                {
                  #### --------------------------------------------------------  Evaluation HAS  -------------------------------------------------------- ####
                  tabPanel("Evaluation HAS",
                           h2("Analyse de référence"),
                           em("Pour ajouter une ou plusieurs réserves, se reporter au formulaire 'Réserves CEM'.", br(),
                              "Pour modifier une réserve existante, aller dans l'onglet 'Edit > Réserves CEM'.", style = "color:#7949F0"),
                           div(
                             strong("Valeur du RDCR (euros) / QALY revendiqué", br()),
                             em("Merci d'inscrire les valeurs minimum/maximum ci-dessous.", br()),
                             column(6, textInput(inputId = "rdcr_qaly_min", label = NULL, placeholder = "minimum", width = "100%", value = rdcr_qaly_min)),
                             column(6, textInput(inputId = "rdcr_qaly_max", label = NULL, placeholder = "maximum", width = "100%", value = rdcr_qaly_max)),
                           ),
                           div(
                             strong("Valeur du RDCR (euros) / QALY hors conclusion", br()),
                             em("Merci d'inscrire les valeurs minimum/maximum ci-dessous.", br()),
                             column(6, textInput(inputId = "rdcr_qaly_horsccl_min", label = NULL, placeholder = "minimum", width = "100%", value = rdcr_qaly_horsccl_min)),
                             column(6, textInput(inputId = "rdcr_qaly_horsccl_max", label = NULL, placeholder = "maximum", width = "100%", value = rdcr_qaly_horsccl_max))
                           ),
                           div(
                             strong("Valeur du RDCR (euros) / QALY dans la conclusion", br()),
                             em("Merci d'inscrire les valeurs minimum/maximum ci-dessous.", br()),
                             column(6, textInput(inputId = "rdcr_qaly_minref", label = NULL, placeholder = "minimum", width = "100%", value = rdcr_qaly_minref)),
                             column(6, textInput(inputId = "rdcr_qaly_maxref", label = NULL, placeholder = "maximum", width = "100%", value = rdcr_qaly_maxref))
                           ),
                           div(
                             strong("Valeur du RDCR (euros par années de vie gagnée) / LY", br()),
                             em("Merci d'inscrire les valeurs hors conclusion/dans la conclusion ci-dessous.", br()),
                             column(6, textInput(inputId = "rdcr_ly_horsccl", label = NULL, placeholder = "hors conclusion", width = "100%", value = rdcr_ly_horsccl)),
                             column(6, textInput(inputId = "rdcr_ly_ccl", label = NULL, placeholder = "dans la conclusion", width = "100%", value = rdcr_ly_ccl))
                           ),
                           div(
                             strong("Valeur du RDCR (euros) / unité dans la conclusion et unité de mesure du RDCR", br()),
                             em("Merci d'inscrire la valeur et l'unité de mesure asociée ci-dessous.", br()),
                             column(6, textInput(inputId = "rdcr_unite_ccl", label = NULL, placeholder = "valeur", width = "100%", value = rdcr_unite_ccl)),
                             column(6, textInput(inputId = "rdcr_unite", label = NULL, placeholder = "unité de mesure associée", width = "100%", value = rdcr_unite))
                           ),
                           selectInput(inputId = "rdcr_qualif", label = "Qualification du RDCR", width = "100%", choices = c(unique(sort(rv$df$RDCR_Qualif)),"Autre (à préciser)"), selected = rdcr_qualif),
                           conditionalPanel(condition = "input.rdcr_qualif == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_rdcr_qualif", label = em("Merci de préciser le type de qualification", style = "color:#00CA9F"), width = "100%", value = autre_rdcr_qualif),
                           ),
                           selectInput(inputId = "rdcr_conclusionCEESP", label = "Conclusion de la CEESP", width = "100%", choices = c(unique(sort(rv$df$RDCR_ConclusionCEESP)),"Autre (à préciser)"), selected = rdcr_conclusionCEESP),
                           conditionalPanel(condition = "input.rdcr_conclusionCEESP == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_rdcr_conclusionCEESP", label = em("Merci de préciser la conclusion de la CEESP", style = "color:#00CA9F"), width = "100%", value = autre_rdcr_conclusionCEESP),
                           ),
                           selectInput(inputId = "rdcr_qualif_incertitude", label = "Qualification de l'incertitude", width = "100%", choices = c(unique(sort(rv$df$RDCR_QualifIncertitude)),"Autre (à préciser)"), selected = rdcr_qualif_incertitude),
                           conditionalPanel(condition = "input.rdcr_qualif_incertitude == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_rdcr_qualif_incertitude", label = em("Merci de préciser le type de qualification de l'incertitude", style = "color:#00CA9F"), width = "100%", value = autre_rdcr_qualif_incertitude),
                           ),


                           h2("Analyse d'impact budgétaire"),
                           em("Pour ajouter une ou plusieurs réserves, se reporter au formulaire 'Réserves BIM'.", br(),
                              "Pour modifier une réserve existante, aller dans l'onglet 'Edit > Réserves BIM'.", style = "color:#7949F0"),
                           radioButtons(inputId = "aib_presence", label = "Présence d'AIB", width = "100%", choices = c("Oui","Non"), selected = aib_presence),
                           conditionalPanel(condition = "input.aib_presence == 'Oui'",
                                           selectInput(inputId = "aib_qualif", label = "Qualification de l'AIB par la CEESP", width = "100%", choices = c(unique(sort(rv$df$AIB_Qualif)),"Autre (à préciser)"), selected = aib_qualif),
                                           conditionalPanel(condition = "input.aib_qualif == 'Autre (à préciser)'",
                                                            textInput(inputId = "autre_aib_qualif", label = em("Merci de préciser le type de qualification par la CEESP", style = "color:#00CA9F"), width = "100%", value = autre_aib_qualif),
                                           ),
                                           selectInput(inputId = "aib_incertitude", label = "Qualification de l'incertitude de l'AIB", width = "100%", choices = c(unique(sort(rv$df$AIB_Incertitude)),"Autre (à préciser)"), selected = aib_incertitude),
                                           conditionalPanel(condition = "input.aib_incertitude == 'Autre (à préciser)'",
                                                            textInput(inputId = "autre_aib_incertitude", label = em("Merci de préciser le type de qualification de l'incertitude", style = "color:#00CA9F"), width = "100%", value = autre_aib_incertitude),
                                           ),
                                           radioButtons(inputId = "aib_analyse_critique", label = "Analyse crtique de l'AIB", width = "100%", choices = c("Recevable","Irrecevable"), selected = aib_analyse_critique),
                           ),


                           h2("Echange technique"),
                           radioButtons(inputId = "echange_tech", label = "Echange technique entre la HAS et l'industriel", width = "100%", choices = c("Oui","Non"), selected = echange_tech),
                           conditionalPanel(condition = "input.echange_tech == 'Oui'",
                                            textInput(inputId = "echange_tech_nbquestion", label = "Nombre de questions pendant l'échange technique", width = "100%", value = echange_tech_nbquestion)
                           ),


                           h2("Evaluation CT"),
                           selectInput(inputId = "CT_ASMR_Rev", label = "ASMR revendiquée par l'industriel", width = "100%", choices = c("-",unique(sort(rv$df$CT_ASMR_Rev)),"Autre (à préciser)"), selected = CT_ASMR_Rev),
                           conditionalPanel(condition = "input.CT_ASMR_Rev == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_CT_ASMR_Rev", label = em("Merci de préciser le type de la ASMR revendiquée par l'industriel", style = "color:#00CA9F"), width = "100%", value = autre_CT_ASMR_Rev),
                           ),
                           selectInput(inputId = "CT_ASMR_Obt", label = "ASMR obtenue", width = "100%", choices = c("-",unique(sort(rv$df$CT_ASMR_Obt)),"Autre (à préciser)"), selected = CT_ASMR_Obt),
                           conditionalPanel(condition = "input.CT_ASMR_Obt == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_CT_ASMR_Obt", label = em("Merci de préciser le type de la ASMR obtenue", style = "color:#00CA9F"), width = "100%", value = autre_CT_ASMR_Obt),
                           ),
                           selectInput(inputId = "CT_SMR", label = "SMR obtenu", width = "100%", choices = c("-",unique(sort(rv$df$CT_SMR)),"Autre (à préciser)"), selected = CT_SMR),
                           conditionalPanel(condition = "input.CT_SMR == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_CT_SMR", label = em("Merci de préciser le type du SMR", style = "color:#00CA9F"), width = "100%", value = autre_CT_SMR),
                           ),
                           selectInput(inputId = "alternative_thera", label = "Existance d'alternative thérapeutique", width = "100%", choices = c("-",unique(sort(rv$df$AlternativeThera)),"Autre (à préciser)"), selected = alternative_thera),
                           conditionalPanel(condition = "input.alternative_thera == 'Autre (à préciser)'",
                                            textInput(inputId = "autre_alternative_thera", label = em("Merci de préciser le type d'existance d'alternative thérapeutique", style = "color:#00CA9F"), width = "100%", value = autre_alternative_thera),
                            ),
                           selectInput(inputId = "alternative_thera1", label = "Alternative thérapeutique : quantification", choices = c("-","1",">1","Aucune"), width = "100%", selected = alternative_thera1),
                           selectInput(inputId = "pronostic_vital_engage", label = "Engagement du pronostic vital ?", choices = c("-","Oui","Non","Donnée manquante"), width = "100%", selected = pronostic_vital_engage),
                  )
                },
                
                {
                  #### --------------------------------------------------------  Population Cible  -------------------------------------------------------- ####
                  tabPanel("Population Cible",
                           h2("Historique des avis CT"),
                           conditionalPanel(condition = "input.demande != `Inscription (projet d'avis - demande retirée)`",
                                            conditionalPanel(condition = "input.demande != 'Inscription'",
                                                             div(
                                                               strong("Date de l'avis CT de l'évaluation CEESP précédente", br()),
                                                               em("La liste déroulante fait référence aux informations suivantes:", br(),
                                                                  "molécule // date validation CEESP // date avis CT // date avis CT de l'évaluation CEESP précédente", br()),
                                                                selectInput(inputId = "ct_concat_info", label = NULL, width = "100%", choices = c("-", sort(rv$df$CT_concat_info)), selected = ct_concat_info)
                                                              )
                                            ),
                                            selectInput(inputId = "ct_pop1", label = "Type de population 1", width = "100%", choices = c("-",sort(unique(rv$df$CT_Pop1)),"Autre (à préciser)"), selected = ct_pop1),
                                            conditionalPanel(condition = "input.ct_pop1 == 'Autre (à préciser)'",
                                                             textInput(inputId = "autre_ct_pop1", label = em("Merci de préciser le type de population 1", style = "color:#00CA9F"), width = "100%", value = autre_ct_pop1),
                                            ),
                                            selectInput(inputId = "ct_pop2", label = "Type de population 2", width = "100%", choices = c(sort(unique(rv$df$CT_Pop2)),"Autre (à préciser)"), selected = ct_pop2),
                                            conditionalPanel(condition = "input.ct_pop2 == 'Autre (à préciser)'",
                                                             textInput(inputId = "autre_ct_pop2", label = em("Merci de préciser le type de population 2", style = "color:#00CA9F"), width = "100%", value = autre_ct_pop2),
                                            ),
                                            div(
                                              h3("Avis N"),
                                              strong("Population cible de l'avis n", br()),
                                              em("Dans le cas où 2 valeurs sont nécessaires, remplir les 2 valeurs (la moyenne des 2 valeurs sera calculée et conservée pour la suite).", br(), 
                                                 "Sinon, remplir uniquement la valeur 1.", br(), style = "color:#00CA9F"),
                                              column(3, textInput(inputId = "ct_popciblespe1", label = "Valeur 1", width = "100%", value = ct_popciblespe1)),
                                              column(3, textInput(inputId = "ct_popciblespe2", label = "Valeur 2", width = "100%", value = ct_popciblespe2)),
                                              column(6, textInput(inputId = "ct_popciblespe_comm", label = "Commentaires", width = "100%", value = ct_popciblespe_comm))
                                            ),
                                            conditionalPanel(condition = "input.ct_popciblespe != ''",
                                                             radioButtons(inputId = "ct_precision", label = "Précision de l'avis n", width = "100%", choices = c("incidente/an","prévalente/totale","-"), selected = ct_precision)
                                            ),
                                            conditionalPanel(condition = "input.demande != 'Inscription'",
                                                             if(edit == TRUE){
                                                               div(
                                                                 add_item_PopCible(count_PopCible = 1, value_CT_PopCible1 = get("ct_popcible_11"), value_CT_PopCible2 = get("ct_popcible_12"), value_CT_PopCibleComm = get("ct_popcible_comm_1"), value_CT_Precision = get("ct_precision_1"), value_CT_Source = get("ct_source_1")),
                                                                 add_item_PopCible(count_PopCible = 2, value_CT_PopCible1 = get("ct_popcible_21"), value_CT_PopCible2 = get("ct_popcible_22"), value_CT_PopCibleComm = get("ct_popcible_comm_2"), value_CT_Precision = get("ct_precision_2"), value_CT_Source = get("ct_source_2")),
                                                                 add_item_PopCible(count_PopCible = 3, value_CT_PopCible1 = get("ct_popcible_31"), value_CT_PopCible2 = get("ct_popcible_32"), value_CT_PopCibleComm = get("ct_popcible_comm_3"), value_CT_Precision = get("ct_precision_3"), value_CT_Source = get("ct_source_3")),
                                                                 add_item_PopCible(count_PopCible = 4, value_CT_PopCible1 = get("ct_popcible_41"), value_CT_PopCible2 = get("ct_popcible_42"), value_CT_PopCibleComm = get("ct_popcible_comm_4"), value_CT_Precision = get("ct_precision_4"), value_CT_Source = get("ct_source_4"))
                                                               )
                                                             }
                                                             else {
                                                                 div(
                                                                   div(id = "modal_ui_PopCible",
                                                                       add_item_PopCible(count_PopCible        = count_PopCible(),
                                                                                         value_CT_PopCible1    = get(paste0("ct_popcible_", count_PopCible(), "1")),
                                                                                         value_CT_PopCible2    = get(paste0("ct_popcible_", count_PopCible(), "2")),
                                                                                         value_CT_PopCibleComm = get(paste0("ct_popcible_comm_", count_PopCible())),
                                                                                         value_CT_Precision    = get(paste0("ct_precision_", count_PopCible())),
                                                                                         value_CT_Source       = get(paste0("ct_source_", count_PopCible())))
                                                                       ),
                                                                   if(count_PopCible() < 4){
                                                                     actionButton(inputId = "add_entry_PopCible",
                                                                                  style   = "border: 0px",
                                                                                  label   = NULL,
                                                                                  icon("circle-plus")
                                                                     )}
                                                                 )
                                                             }
                                            ),
                           ),
                           conditionalPanel(condition = "input.demande == `Inscription (projet d'avis - demande retirée)`",
                                            em("Cette section est non applicable dans le cas d'une inscription avec demande retirée.", style = "color:#00CA9F"),
                           )
                  )
                },
                
                {
                  #### --------------------------------------------------------  Prix  -------------------------------------------------------- ####
                  tabPanel("Prix",
                           br(),
                           conditionalPanel(condition = "input.Asso_Several_Products == 'Oui'",
                                            if (edit) {
                                              em("Voir dans l'onglet 'Table des associations'")
                                            } else {
                                              em("Vous pourrez remplir ces informations à la page suivante.")
                                            }
                           ),
                           conditionalPanel(condition = "input.type_produit != 'Asso' || (input.type_produit == 'Asso' && input.Asso_Several_Products == 'Non')",
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
                                                             checkboxInput(inputId = "afficher_1erJO", label = "Ajouter le 1er JO ?", value = afficher_1erJO),
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
                  )
                },
                
                {
                  #### --------------------------------------------------------  Posologie  -------------------------------------------------------- ####
                  tabPanel("Posologie", 
                           br(),
                           conditionalPanel(condition = "input.Asso_Several_Products == 'Oui'",
                                            if (edit) {
                                              em("Voir dans l'onglet 'Table des associations'")
                                            } else {
                                              em("Vous pourrez remplir ces informations à la page suivante.")
                                            }
                           ),
                           conditionalPanel(condition = "input.type_produit != 'Asso' || (input.type_produit == 'Asso' && input.Asso_Several_Products == 'Non')",
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
                                                                                               selectInput(inputId = "PTCED_Attaque_cate_age", label = "Catégorie d'âge", choices = c("Adulte","Adolescent","Enfant","Nourrisson"), selected = sel_cate_age_attaque, width = "100%"),
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
                                                                                               selectInput(inputId = "PTCED_Entretien_cate_age", label = "Catégorie d'âge", choices = c("Adulte","Adolescent","Enfant","Nourrisson"), selected = sel_cate_age_entretien, width = "100%"),
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
                                                                                               selectInput("PTNC_cate_age", "Catégorie d'âge", choices = c("Adulte","Adolescent","Enfant","Nourrisson"), selected = sel_cate_age_NC, width = "100%"),
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
                                            )
                           )
                )},
 
                {
                  #### --------------------------------------------------------  Reserves CEM  -------------------------------------------------------- ####
                  if(edit_module == TRUE){
                    tabPanel("Réserves CEM", 
                             checkboxInput(inputId = "add_resCEM",
                                           label   = "Ajouter Nouvelle(s) Réserve(s) CEM",
                            ),
                            em("Pour modifier une réserve existante, merci d'aller dans l'onglet 'Edit > Réserves CEM'.", style = "color:#7949F0"),
                            conditionalPanel(condition = "input.add_resCEM == 1", 
                                             div(id = "modal_ui_resCEM",
                                                 add_item_resCEM(count_resCEM = count_resCEM())
                                             ),
                                             actionButton(inputId = "add_entry_resCEM",
                                                          style   = "border: 0px",
                                                          label   = NULL,
                                                          icon("circle-plus")
                                             )
                            ),
                    )
                  }
                },
                
                {
                  #### --------------------------------------------------------  Reserves BIM  -------------------------------------------------------- ####
                  if(edit_module == TRUE){
                    tabPanel("Réserves BIM", 
                             checkboxInput(inputId = "add_resBIM",
                                           label   = "Ajouter Nouvelle(s) Réserve(s) BIM",
                            ),
                            em("Pour modifier une réserve existante, merci d'aller dans l'onglet 'Edit > Réserves BIM'.", style = "color:#7949F0"),
                            conditionalPanel(condition = "input.add_resBIM == 1", 
                                             div(id = "modal_ui_resBIM",
                                                 add_item_resBIM(count_resBIM = count_resBIM())
                                             ),
                                             actionButton(inputId = "add_entry_resBIM",
                                                          style   = "border: 0px",
                                                          label   = NULL,
                                                          icon("circle-plus")
                                             )
                            ),
                    )
                  }
                },
                
    ),
    
    size = "l",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "row",
        div(
          class = "col-md-6 text-left",
          shiny::actionButton(inputId = "dismiss_modal", label = "Close", class = "btn-danger" ),
        ),
        div(
          class = "col-md-6 text-right",
          if(edit_module == TRUE) {
            shiny::actionButton(inputId = "final_edit", label = x, icon = shiny::icon("edit"), class = "btn-info")
          }
          else {
            shiny::actionButton(inputId = "final_edit_qc", label = x, icon = shiny::icon("edit"), class = "btn-info")
          }
        
        )
      )
    )
    
  ) %>% shiny::showModal()

  observeEvent(input$add_entry_PopCible, {
    if(count_PopCible() < 4){
      count_PopCible(count_PopCible() + 1)
      insertUI(selector = "#modal_ui_PopCible",
               ui =  add_item_PopCible(count_PopCible        = count_PopCible(),
                                       value_CT_PopCible1    = get(paste0("ct_popcible_", count_PopCible(), "1")),
                                       value_CT_PopCible2    = get(paste0("ct_popcible_", count_PopCible(), "2")),
                                       value_CT_PopCibleComm = get(paste0("ct_popcible_comm_", count_PopCible())),
                                       value_CT_Precision    = get(paste0("ct_precision_", count_PopCible())),
                                       value_CT_Source       = get(paste0("ct_source_", count_PopCible()))))
    }
  })

  observeEvent(input$add_entry_Prix, {
     if(count_Prix() < 8){
      count_Prix(count_Prix() + 1)
      insertUI(selector = "#modal_ui_Prix",
               ui = add_item_Prix(count_Prix             = count_Prix(),
                                  value_JO_InfoPrix      = get(paste0("JO_InfoPrix_", count_Prix())),
                                  value_JO_Prix1erJO     = get(paste0("JO_Prix1erJO_", count_Prix())),
                                  value_JO_PrixVigueurJO = get(paste0("JO_PrixVigueurJO_", count_Prix())),
                                  value_JO_PrixPostJO    = get(paste0("JO_PrixPostJO_", count_Prix())),
                                  value_UCD_Flacon       = get(paste0("UCD_Flacon_", count_Prix())),
                                  value_UCD_Dose         = get(paste0("UCD_Dose_", count_Prix())),  
                                  value_UCD_Volume       = get(paste0("UCD_Volume_", count_Prix())),
                                  value_UCD_Quantite     = get(paste0("UCD_Quantite_", count_Prix())),
                                  value_UCD_Nb           = get(paste0("UCD_Nb_", count_Prix())),
                                  value_ATU_IndemMaxUCD  = get(paste0("ATU_IndemMaxUCD_", count_Prix()))))
    }
  })
  
  observeEvent(input$add_entry_PTC, {
    if(count_PTC() < 4){
      count_PTC(count_PTC() + 1)
      insertUI(selector = "#modal_ui_PTC", 
               ui = add_item_PTC(count_PTC            = count_PTC(), 
                                 value_PTC_typedose   = get(paste0("sel_type_poso_", count_PTC())), 
                                 value_PTC_cate_age   = get(paste0("sel_cate_age_", count_PTC())), 
                                 value_PTC_age        = get(paste0("sel_age_", count_PTC())), 
                                 value_PTC_dose       = get(paste0("PTC_DosePrise_", count_PTC())), 
                                 value_PTC_grandunite = get(paste0("PTC_GrandeurUniteDose_", count_PTC())), 
                                 value_PTC_unite      = get(paste0("PTC_UniteDose_", count_PTC())), 
                                 value_PTC_nb         = get(paste0("PTC_NbPrise_", count_PTC())),
                                 value_PTC_nbadm      = get(paste0("PTC_NbAdmCycle_", count_PTC())), 
                                 value_PTC_duree      = get(paste0("PTC_DureeCycle_", count_PTC()))))
    }
  })
  
  observeEvent(input$add_entry_PTCED, {
    if(count_PTCED() < 4){
      count_PTCED(count_PTCED() + 1)
      insertUI(selector = "#modal_ui_PTCED", 
               ui = add_item_PTCED(count_PTCED            = count_PTCED(), 
                                   value_PTCED_dose       = get(paste0("PTCED_DosePrise_", count_PTCED())),
                                   value_PTCED_grandunite = get(paste0("PTCED_GrandeurUniteDose_", count_PTCED())), 
                                   value_PTCED_unite      = get(paste0("PTCED_UniteDose_", count_PTCED())), 
                                   value_PTCED_nbdose     = get(paste0("PTCED_NbPrise_", count_PTCED())),
                                   value_PTCED_nbadm      = get(paste0("PTCED_NbAdmCycleAttaque_", count_PTCED())),
                                   value_PTCED_duree      = get(paste0("PTCED_DureeCycleAttaque_", count_PTCED())),
                                   value_PTCED_nbcycle    = get(paste0("PTCED_NbCycleAttaque_", count_PTCED()))))
    }
  })
  
  observeEvent(input$add_entry_resCEM, {
    {
      count_resCEM(count_resCEM() + 1)
      insertUI(selector = "#modal_ui_resCEM", ui = add_item_resCEM(count_resCEM = count_resCEM()))
    }
  })
  
  observeEvent(input$add_entry_resBIM, {
    {
      count_resBIM(count_resBIM() + 1)
      insertUI(selector = "#modal_ui_resBIM", ui = add_item_resBIM(count_resBIM = count_resBIM()))
    }
  })
  
}



## END
