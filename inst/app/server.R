# AE app


# Define server logic required ####
shiny::shinyServer(function(input, output, session) {
  # ~~~~~ UPLOAD DATA ####
  # Upload AE data
  AE_data_upload <- selected_data_upload <- shiny::eventReactive(input$data_file, {
    if (is.null(input$data_file)) return(NULL)
    rio::import(input$data_file$datapath) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y") #%>% dplyr::select(-dplyr::any_of(Initials))
  })
  # Upload DEMOGRAPHICS data
  demographics_upload <- shiny::eventReactive(input$demographics_file, {
    if (is.null(input$demographics_file)) return(NULL)
    rio::import(input$demographics_file$datapath) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y")  #%>% dplyr::select(-dplyr::any_of(Initials))
  })
  # Upload follow up  data
  fu_data_upload0  <- shiny::eventReactive(input$fu_file, {
    if (is.null(input$fu_file)) return(NULL)
    rio::import(input$fu_file$datapath) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y") # %>% dplyr::select(-dplyr::any_of(Initials))
  })
  # Upload drug administration data
  da_data_upload0  <- shiny::eventReactive(input$da_file, {
    if (is.null(input$da_file)) return(NULL)
    rio::import(input$da_file$datapath) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y") # %>% dplyr::select(-dplyr::any_of(Initials))
  })
  # Upload drug administration data
  re_data_upload0  <- shiny::eventReactive(input$re_file, {
    if (is.null(input$re_file)) return(NULL)
    rio::import(input$re_file$datapath) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y") # %>% dplyr::select(-dplyr::any_of(Initials))
  })


  # ~~~~~ IMPORT DEM0 DATA ~~~~~~ ####
  # data sets for displaying data before user upload ###
  demo_data <- shiny::reactive({
    #rio::import("demo_data.xls") %>%
    rio::import("www/demo_ae_data.csv") %>%
      janitor::clean_names() %>%
    #rio::import(here::here("inst","app","www","demo_ae_data.csv")) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      #dplyr::mutate(form=gsub("Moffitt","",form)) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y")  
   })
  demo_data2 <- shiny::reactive({
    #rio::import("demo_data2.xls") %>%
    rio::import("www/demo_demo_data.csv") %>%
      janitor::clean_names() %>%
    #rio::import(here::here("inst","app","www","demo_demo_data.csv") ) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y")
  })
  demo_fu_data <- shiny::reactive({
    rio::import("www/demo_fu_data.csv") %>%
      janitor::clean_names() %>%
    #rio::import(here::here("inst","app","www","demo_fu_data.csv") ) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y")
  })
  demo_da_data <- shiny::reactive({
    rio::import("www/demo_da_data.csv") %>%
      janitor::clean_names() %>%
    #rio::import(here::here("inst","app","www","demo_da_data.csv") ) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y")
  })
  demo_re_data <- shiny::reactive({
    rio::import("www/demo_recist_data.csv") %>%
      janitor::clean_names() %>%
      #rio::import(here::here("inst","app","www","demo_recist_data.csv") ) %>%
      # if integer, conver it to numeric
      dplyr::mutate_if(~ is.integer(.) & !is.Date(.), as.numeric) %>%
      # if any numeric variable has less than or equal to 5 unique categories,
      # convert it to factor
      dplyr::mutate_if(~ is.numeric(.) & dplyr::n_distinct(.) <= 5, as.factor) %>%
      dplyr::mutate_at(vars(any_of(grep("date",colnames(.), value = T, ignore.case = T))), as.Date, format="%m/%d/%Y")
  })
  # End demo data import ###


  # ~~~~~ USE DEMO DATA ~~~~~ ####
  # some AE file versions do not have cdus_toxicity_type_code or cdus_ctcae_toxicity_type_code they may only have toxicity ###
  AE_data <- selected_data <- shiny::reactive({ if (is.null(input$data_file)) {

    output <- demo_data()  %>% janitor::clean_names() %>%
      dplyr::mutate(sequence_no = as.character(sprintf("%03d", as.numeric(sequence_no)))) %>%
      dplyr::filter(is.na(sequence_no) == FALSE)

    if ( "cdus_ctcae_toxicity_type_code" %in% names(output)){output <- output %>%
      dplyr::mutate(cdus_toxicity_type_code = cdus_ctcae_toxicity_type_code) 
    
    }

  } else {
    output <- AE_data_upload() %>% janitor::clean_names() %>%
      dplyr::mutate(sequence_no = as.character(sprintf("%03d", as.numeric(sequence_no)))) %>%
      dplyr::filter(is.na(sequence_no) == FALSE)

    if ( "cdus_ctcae_toxicity_type_code" %in% names(output)){output <- output %>%
      dplyr::mutate(cdus_toxicity_type_code = cdus_ctcae_toxicity_type_code)}
  }

    if("cdus_toxicity_type_code" %in% names(output)){
      output$cdus_ctcae_toxicity_type_code <- output$cdus_toxicity_type_code }

    if("toxicity" %in% names(output)){output$cdus_ctcae_toxicity_type_code <- output$toxicity;
    output$cdus_toxicity_type_code       <- output$toxicity}

    if (( !"cdus_toxicity_type_code" %in% names(output)) & ("toxicity" %in% names(output))){
      output$cdus_toxicity_type_code       <-  output$toxicity
    }

    output<-as.data.frame(output %>% dplyr::filter(is.na(grade)==FALSE)  %>%
                            dplyr::select(-cdus_ctcae_toxicity_type_code,-cycle)) %>%
      dplyr::select(sequence_no, visit_date, start_date_of_course , onset_date , resolved_date, 
             cdus_toxicity_type_code, toxicity_category, grade, 
             attribution_possible, attribution_probable, attribution_definite)
    output
  })

  demographics_data <- shiny::reactive({
    if (is.null(input$demographics_file)) {demographics_data <- demo_data2()} else {demographics_data <- demographics_upload()}
    demographics_data %>%
      janitor::clean_names() %>%
      dplyr::mutate(sequence_no = as.numeric(sequence_no)) %>%
      dplyr::filter(!is.na(sequence_no)) %>%
      dplyr::mutate(sequence_no = as.character(sprintf("%03d", sequence_no))) %>%
      dplyr::arrange(sequence_no)
  })
  fu_data_upload <- shiny::reactive({
    if (is.null(input$fu_file)) {fu_data_upload <- demo_fu_data()} else {fu_data_upload <- fu_data_upload0()}
    fu_data_upload %>%
      janitor::clean_names() %>%
      dplyr::mutate(sequence_no = as.numeric(sequence_no)) %>%
      dplyr::filter(!is.na(sequence_no)) %>%
      dplyr::mutate(sequence_no = as.character(sprintf("%03d", sequence_no))) %>%
      dplyr::arrange(sequence_no)
  })
  da_data_upload <- shiny::reactive({
    if (is.null(input$da_file)) {da_data_upload <- demo_da_data()} else {da_data_upload <- da_data_upload0()}
    da_data_upload %>%
      janitor::clean_names() %>%
      dplyr::mutate(sequence_no = as.numeric(sequence_no)) %>%
      dplyr::filter(!is.na(sequence_no)) %>%
      dplyr::mutate(sequence_no = as.character(sprintf("%03d", sequence_no))) %>%
      dplyr::rename(any_of(c(start_date_of_drug = "first_dose_date"))) %>%
      dplyr::arrange(sequence_no)
  })
  re_data_upload <- shiny::reactive({
    re_data_upload <- demo_re_data()
    if (any(c(!is.null(input$data_file),!is.null(input$demographics_file),!is.null(input$fu_file),!is.null(input$da_file),!is.null(input$re_file)))) {
      re_data_upload <- re_data_upload0()
    }
    #if (is.null(input$re_file)) {re_data_upload <- demo_re_data()} else {re_data_upload <- re_data_upload0()}
    re_data_upload %>%
      janitor::clean_names() %>%
      dplyr::mutate(sequence_no = as.numeric(sequence_no)) %>%
      dplyr::filter(!is.na(sequence_no)) %>%
      dplyr::mutate(sequence_no = as.character(sprintf("%03d", sequence_no))) %>%
      dplyr::arrange(sequence_no)
  })

  # ~~~~~ Make toxicity data by merging AE data and Demographics #####
  toxicity_data <- shiny::reactive({

    AE_data <-  AE_data() %>% janitor::clean_names() %>%  dplyr::filter(is.na(sequence_no) == FALSE)
    demographics_data <-  demographics_data() %>% janitor::clean_names() %>%   dplyr::filter(is.na(sequence_no) == FALSE)

    #save(list = ls(), file = "toxicity_data.RData", envir = environment())

    if("onset_date" %in% names(AE_data)){AE_data$onset_date_of_ae <- AE_data$onset_date }
    if("cdus_toxicity_type_code" %in% names(AE_data)){AE_data$cdus_ctcae_toxicity_type_code <- AE_data$cdus_toxicity_type_code }
    if("toxicity" %in% names(AE_data)){AE_data$cdus_ctcae_toxicity_type_code <- AE_data$toxicity }
    if(!"attribution_possible" %in% names(AE_data)){AE_data <- AE_data %>% dplyr::mutate(attribution_possible = dplyr::case_when(attribution == "Possible" ~ "Yes" ,
                                                                                                                   attribution != "Possible" ~ "Not  Applicable" ),
                                                                                  attribution_probable = dplyr::case_when(attribution == "Probable" ~ "Yes",
                                                                                                                   attribution != "Probable" ~ "Not  Applicable" ),
                                                                                  attribution_definite = dplyr::case_when(attribution == "Definite" ~ "Yes",
                                                                                                                   attribution != "Definite" ~ "Not  Applicable" ))
    }


    AE_data <- AE_data %>% dplyr::select(-any_of(c( "form", "form_desc")))

    toxicity.data <-  merge(AE_data %>%  janitor::clean_names() %>% dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE ),
                            demographics_data %>% janitor::clean_names() %>% dplyr::filter(is.na(on_treatment_date)==FALSE) %>% dplyr::select(sequence_no, on_treatment_date),
                            by = "sequence_no")

    #---calculate AE time---
    if( "start_date_of_course" %in% names(toxicity.data)){
      toxicity.data$start_date_of_course_cycle <- toxicity.data$start_date_of_course} else {
        toxicity.data$start_date_of_course_cycle <- toxicity.data$start_date

      }

    # print(toxicity.data %>% dplyr::select(onset_date_of_ae, on_treatment_date))
    # print(str(toxicity.data %>% dplyr::select(onset_date_of_ae, on_treatment_date),1,1))

    toxicity.data <- toxicity.data %>% dplyr::mutate(pid = sequence_no, time = start_date_of_course_cycle,
                                              AE.time = difftime(onset_date_of_ae, on_treatment_date, units = "days"))#
    toxicity.data <- toxicity.data[!is.na(toxicity.data$time), ]

    toxicity.data
    #}

  }) # end shiny::reactive toxicity data code chunk stuff thing

  recist_data <- shiny::reactive({

    recist_data <- re_data_upload() %>% janitor::clean_names() %>% dplyr::filter(is.na(sequence_no) == FALSE)

    response_cols <- grep("response",colnames(recist_data), ignore.case = T, value = T)
    if (length(response_cols) > 0) {
      col_to_check <- sapply(response_cols,function(x) {
        vec <- recist_data[,x][which(!is.na(recist_data[,x]))]
        if (all(!is.na(as.numeric(vec)))) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      })
      col_to_check_true <- names(col_to_check[which(col_to_check == TRUE)])
      if (length(col_to_check_true) == 1) {
        response_cats <- c("1" = "Baseline (BL)", "2" = "Not Evaluable (NE)", "3" = "Stable Disease (SD)",
                           "4" = "Partial Response (PR)", "5" = "Complete Response (CR)", "6" = "Progressive Disease (PD)")
        Response <- response_cats[as.character(recist_data[,col_to_check_true])]
        recist_data$response <- Response
      }
    }
    recist_data

  })


  # ~~~~~ render datatables TO DISPLAY ~~~~~~~~#####

  output$selected_data <- DT::renderDataTable({
    displayAE_data <- AE_data() %>% dplyr::select(-dplyr::any_of( "initials"))  %>%
      dplyr::select( dplyr::where( ~!all(is.na(.x)) ) ) #%>% select(sequence_no,grade)  
    DT::datatable(displayAE_data,    rownames = FALSE)
  })

  output$demographics_data <- DT::renderDataTable({
    displaydemographics_data <- demographics_data() %>% dplyr::select(-dplyr::any_of( "initials")) %>%
      dplyr::select( dplyr::where( ~!all(is.na(.x)) ) )
    DT::datatable(displaydemographics_data, rownames = FALSE)
  })

  output$followup_data <- DT::renderDataTable({
    displayfu_data_upload <- fu_data_upload() %>% dplyr::select(-dplyr::any_of( "initials")) %>%
      dplyr::select( dplyr::where( ~!all(is.na(.x)) ) ) 
    DT::datatable(displayfu_data_upload, rownames = FALSE)
  })

  output$tox_data <- DT::renderDataTable({
    displaytoxicity_data <- toxicity_data() %>% dplyr::select(-dplyr::any_of( "initials")) %>%
      dplyr::select( dplyr::where( ~!all(is.na(.x)) ) )
    DT::datatable(displaytoxicity_data, rownames = FALSE)
  })

  output$drug_data <- DT::renderDataTable({
    displayda_data_upload <- da_data_upload() %>% dplyr::select(-dplyr::any_of( "initials")) %>%
      dplyr::select( dplyr::where( ~!all(is.na(.x)) ) )
    DT::datatable(displayda_data_upload, rownames = FALSE)
  })

  output$RECIST_Data_Message1 <- renderText({
    if (any(c(!is.null(input$data_file),!is.null(input$demographics_file),!is.null(input$fu_file),!is.null(input$da_file)))) {
      if (is.null(input$re_file)) {
        "Upload RECIST data to view."
      }
    }
  })
  output$RECIST_Data_Message2 <- renderText({
    if (any(c(!is.null(input$data_file),!is.null(input$demographics_file),!is.null(input$fu_file),!is.null(input$da_file)))) {
      if (is.null(input$re_file)) {
        "Upload RECIST data to view."
      }
    }
  })
  output$RECIST_Data_Message3 <- renderText({
    if (any(c(!is.null(input$data_file),!is.null(input$demographics_file),!is.null(input$fu_file),!is.null(input$da_file)))) {
      if (is.null(input$re_file)) {
        "Upload RECIST data to view."
      }
    }
  })

  output$re_data <- DT::renderDataTable({
    req(recist_data())
    displayre_data_upload <- recist_data() %>% dplyr::select(-dplyr::any_of( "initials")) %>%
      dplyr::select( dplyr::where( ~!all(is.na(.x)) ) )
    DT::datatable(displayre_data_upload, rownames = FALSE)
  })



  # MERGE AE AND on_treatment date from DEMOGRAPHICS changing names in uploaded data if necessary ####
  AEandDemoData <- AE_data_test2 <- shiny::reactive({
    AE_data <-  AE_data()  %>% janitor::clean_names()
    demographics_data  <-  demographics_data()   %>% janitor::clean_names()

    if("onset_date" %in% names(AE_data)){AE_data$onset_date_of_ae <- AE_data$onset_date }
    if("cdus_toxicity_type_code" %in% names(AE_data)){AE_data$cdus_ctcae_toxicity_type_code <- AE_data$cdus_toxicity_type_code }
    if("toxicity" %in% names(AE_data)){AE_data$cdus_ctcae_toxicity_type_code <- AE_data$toxicity }

    if (( !"cdus_toxicity_type_code" %in% names(AE_data)) & ("toxicity" %in% names(AE_data))){
      AE_data$cdus_toxicity_type_code <-  AE_data$toxicity
    }



    if(!"attribution_possible" %in% names(AE_data)){AE_data <- AE_data %>% dplyr::mutate(attribution_possible = dplyr::case_when(attribution == "Possible" ~ "Yes" ,
                                                                                                                   attribution != "Possible" ~ "Not  Applicable" ),
                                                                                  attribution_probable = dplyr::case_when(attribution == "Probable" ~ "Yes",
                                                                                                                   attribution != "Probable" ~ "Not  Applicable" ),
                                                                                  attribution_definite = dplyr::case_when(attribution == "Definite" ~ "Yes",
                                                                                                                   attribution != "Definite" ~ "Not  Applicable" ))
    }


    #AE_data$sequence_no <- as.numeric(AE_data$sequence_no)
    #demographics_data$sequence_no <- as.numeric(demographics_data$sequence_no)

    merge(AE_data %>%  janitor::clean_names() %>% dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE ),
          demographics_data %>% janitor::clean_names() %>% dplyr::filter(is.na(on_treatment_date)==FALSE) %>%
            dplyr::select(dplyr::any_of(c("sequence_no", "on_treatment_date","last_visit_date"))),
          by = "sequence_no")
  })



  # start: individual AE table tab  vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv####

  AEcounttableind <- shiny::reactive({


    subject<-input$single_var_input

    #adverse_events0<- AE_data() %>% dplyr::filter(sequence_no == subject)
    adverse_events0<-AEandDemoData() %>% dplyr::filter(sequence_no == subject)  %>%
      dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE  & is.na(on_treatment_date) == FALSE)

    if (!"cdus_toxicity_type_code" %in% names(adverse_events0)){
      adverse_events0$cdus_toxicity_type_code<- adverse_events0$toxicity_category}

    if (!"cdus_ctcae_toxicity_type_code" %in% names(adverse_events0)){
      adverse_events0$cdus_ctcae_toxicity_type_code<- adverse_events0$cdus_toxicity_type_code}

    if (!class(adverse_events0$grade) == "numeric"){
      adverse_events0$grade<- as.numeric(adverse_events0$grade)
    }

    # Toxicity table
    # This is a AE frequency not the max AE----

    adverse_events2 <- adverse_events0 %>% dplyr::filter(is.na(grade)==FALSE) %>%
      dplyr::group_by(sequence_no, cdus_ctcae_toxicity_type_code) %>%
      #dplyr::filter(grade == max(grade, na.rm = TRUE)) %>%
      dplyr::ungroup() #%>%
    #dplyr::distinct(sequence_no, cdus_ctcae_toxicity_type_code, grade, .keep_all = TRUE)
    #dplyr::distinct(sequence_no, cdus_ctcae_toxicity_type_code, .keep_all = TRUE)

    summary_ae_drug =adverse_events2%>%
      dplyr::mutate(grade = recode_factor(grade, `1` = "Grade 1", `2` = "Grade 2", `3` = "Grade 3",`4` = "Grade 4",`5` = "Grade 5")) %>%
      dplyr::count( `Adverse event` = cdus_ctcae_toxicity_type_code, grade) %>%
      tidyr::pivot_wider(names_from = grade, values_from = n, names_sort = TRUE)  %>%
      dplyr::filter(is.na(`Adverse event`)==FALSE)

    summary_ae_drug # ->summary_ae_drugMCC18494 #->summary_ae_drugMCC18494
    summary_ae_drug[is.na(summary_ae_drug)] <- 0

    return(summary_ae_drug)
  })

  output$AEcounttableind <- DT::renderDataTable({
    DT::datatable(AEcounttableind() ,
              rownames = FALSE, options = list(pageLength = 100))})


  # Download handler for AE data #####
  {
    output$AEcounttableinddownload <- shiny::downloadHandler(
      filename = function(){"AEcounttableind.csv"},
      content = function(fname){
        write.csv(AEcounttableind(), fname, row.names = FALSE)
      }
    )
  }

  # end: individual AE table tab   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ########


  # start: AE table tab  vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv####

  AEcounttables  <- shiny::reactive({

    #adverse_events0<- AE_data()
    adverse_events0<-AEandDemoData() %>%
      dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE  & is.na(on_treatment_date) == FALSE)

    if (!"cdus_toxicity_type_code" %in% names(adverse_events0)){
      adverse_events0$cdus_toxicity_type_code<- adverse_events0$toxicity_category}

    if (!"cdus_ctcae_toxicity_type_code" %in% names(adverse_events0)){
      adverse_events0$cdus_ctcae_toxicity_type_code<- adverse_events0$cdus_toxicity_type_code}

    if (!class(adverse_events0$grade) == "numeric"){
      adverse_events0$grade<- as.numeric(adverse_events0$grade)
    }

    # Toxicity table
    # This is a AE frequency counting ALL ggplot2::aes  not the max AE for each type--

    adverse_events2 <- adverse_events0 %>% dplyr::filter(is.na(grade)==FALSE) %>%
      dplyr::group_by(sequence_no, cdus_ctcae_toxicity_type_code) %>%
      #dplyr::filter(grade == max(grade, na.rm = TRUE)) %>%
      dplyr::ungroup() # %>%
    #dplyr::distinct(sequence_no, cdus_ctcae_toxicity_type_code, grade, .keep_all = TRUE)

    summary_ae_drug <- adverse_events2 %>%
      dplyr::mutate(grade = recode_factor(grade, `1` = "Grade 1", `2` = "Grade 2", `3` = "Grade 3",`4` = "Grade 4",`5` = "Grade 5")) %>%
      dplyr::count( `Adverse event` = cdus_ctcae_toxicity_type_code,grade) %>%
      tidyr::pivot_wider(names_from = grade, values_from = n, names_sort = TRUE) %>%
      dplyr::filter(is.na(`Adverse event`)==FALSE)


    summary_ae_drug[is.na(summary_ae_drug)] <- 0

    return(summary_ae_drug)
  })

  output$AEcounttables <- DT::renderDataTable({

    DT::datatable(AEcounttables(),   rownames = FALSE, options = list(pageLength = 100))  })

  # Download handler for AE dplyr::count table  data #####
  {
    output$AEcounttabledownload <- shiny::downloadHandler(
      filename = function(){"AEcounttable.csv"},
      content = function(fname){
        write.csv(AEcounttables(), fname, row.names = FALSE)
      }
    )
  }
  # end: AE table tab   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ########



  # start: AE days tab vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  ####
  # Calculate number of days of ggplot2::aes and number of unique Days of ggplot2::aes to display with render table ###
  output$selected_data_AEDAYS <- DT::renderDataTable({


    a2 <- AEandDemoData()  %>%  dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date ) == FALSE & is.na(on_treatment_date ) == FALSE) %>%
      dplyr::mutate(attribution_possible = replace_na(attribution_possible, 'Not  Applicable'),
                    attribution_probable = replace_na(attribution_probable, 'Not  Applicable'),
                    attribution_definite = replace_na(attribution_definite, 'Not  Applicable')) %>%
      dplyr::group_by(sequence_no) %>%
      dplyr::mutate( treatment_related = !apply(as.matrix(vars(name2)),1,function(x) all(x=='Not  Applicable')),
              treatment_related = factor(treatment_related,level=c(F,T),label=c('No','Yes')),
              t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
              t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
              t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days')),
              code=cdus_ctcae_toxicity_type_code,
              index = as.numeric(factor(code)),
              listdaynumber = map2(t1,t2,function(.x, .y){.x:.y}),
              totalnumberofdayswithAEs = length(unlist( listdaynumber)),
              numberofdayswithAEs = length(unique(unlist( listdaynumber)))
      )


    # dplyr::select only what we need to show ###
    a3 <- as.data.frame(a2 %>% dplyr::select(sequence_no, totalnumberofdayswithAEs,  numberofdayswithAEs) %>%
                          dplyr::distinct(sequence_no, .keep_all = TRUE))

    DT::datatable(a3 ,   rownames = FALSE, options = list(pageLength = 100))
  })

  AEdays <- shiny::reactive({
    a2 <- AEandDemoData()  %>%  dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date ) == FALSE & is.na(on_treatment_date ) == FALSE) %>%
      dplyr::mutate(attribution_possible = replace_na(attribution_possible, 'Not  Applicable'),
                    attribution_probable = replace_na(attribution_probable, 'Not  Applicable'),
                    attribution_definite = replace_na(attribution_definite, 'Not  Applicable')) %>%
      dplyr::group_by(sequence_no) %>%
      dplyr::mutate( treatment_related = !apply(as.matrix(vars(name2)),1,function(x) all(x=='Not  Applicable')),
              treatment_related = factor(treatment_related,level=c(F,T),label=c('No','Yes')),
              t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
              t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
              t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days')),
              code=cdus_ctcae_toxicity_type_code,
              index = as.numeric(factor(code)),
              listdaynumber = map2(t1,t2,function(.x, .y){.x:.y}),
              totalnumberofdayswithAEs = length(unlist( listdaynumber)),
              numberofdayswithAEs = length(unique(unlist( listdaynumber)))
      )


    # dplyr::select only what we need to show ###
    a3 <- as.data.frame(a2 %>% dplyr::select(sequence_no, totalnumberofdayswithAEs,  numberofdayswithAEs) %>%
                          dplyr::distinct(sequence_no, .keep_all = TRUE))

    a3

  })

  # Download handler for AE data #####
  {
    output$AEDAYSdownload <- shiny::downloadHandler(
      filename = function(){"AEdays.csv"},
      content = function(fname){
        write.csv(AEdays(), fname, row.names = FALSE)
      }
    )
  }

  # end: AE days tab ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ####


  # start: AE plot tab vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  ####
  # ~~~~~~AE swimmers plot ~~~~~~~~~~~~~~~~~~
  observe({ shiny::updateSelectizeInput(session, "single_var_input",
                                 choices = unique(AEandDemoData()$sequence_no),
                                 #selected = character(0),
                                 selected = unique(AEandDemoData()$sequence_no)[1],
                                 server = TRUE) })

  # Custom ggplot2::theme for ggplot2::ggplot
  my_theme = ggplot2::theme(axis.title = ggplot2::element_text(size = 20), axis.text = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15), strip.text = ggplot2::element_text(size = 15))

  # AE LINE-swimmers PLOT ######
  single_var_plot <- shiny::reactive({

    w1<-AEandDemoData() %>%
      dplyr::filter(is.na(onset_date_of_ae) == FALSE  &
               is.na(resolved_date) == FALSE  &
               is.na(on_treatment_date) == FALSE)

    subject<-input$single_var_input

    w1 <- w1 %>% dplyr::filter(sequence_no == subject)
    name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date' ,'grade')
    name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
    #w2<-w1%>%dplyr::select(c(name1,name2))
    w2<-w1%>%dplyr::select(dplyr::any_of(c(name1,name2)))
    w2[,name2][is.na(w2[,name2])] <- 'Not  Applicable'
    w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x=='Not  Applicable'))
    w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
    w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                   t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                   t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days')),
                   code=cdus_ctcae_toxicity_type_code)
    w2$index<-as.numeric(factor(w2$code))
    w2$grade<-as.factor(w2$grade)



    if("start_date_of_drug" %in% names( da_data_upload())) {
      da_data_subject <- da_data_upload() %>% dplyr::filter(sequence_no == subject)
    }else{

      da_data_subject <- da_data_upload() %>% dplyr::mutate(start_date_of_drug = start_date) %>%
        dplyr::filter(sequence_no == subject)

    }

    if("drug" %in% names( da_data_subject)) {
      da_data_subject <-da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)

    }else{

      da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)
    }

    w2.2 <- w2 %>% dplyr::select(on_treatment_date) %>% dplyr::distinct(on_treatment_date)

    # w2.2 <- w2 %>% dplyr::select(on_treatment_date) %>% dplyr::distinct(on_treatment_date)

    w3 <- merge(  da_data_subject, w2.2) %>% dplyr::mutate(dadaynumber = as.numeric(difftime(start_date_of_drug ,on_treatment_date,units='days')) )

    UL <- max(w2$t2)+10
    plot1 <- ggplot2::ggplot(w2, ggplot2::aes(t1, index, color=grade, shape=treatment_related, label = code)) +
      ggplot2::geom_point(ggplot2::aes(t2, index),size=2)+
      ggplot2::geom_segment(ggplot2::aes(xend = t2, yend = index), size = 1, lineend = "butt")+
      ggplot2::xlab('days')+ggplot2::ylab(paste0("Seq #: ", subject))+
      xlim(c(-4,UL)) +
      ggplot2::scale_y_continuous(breaks=w2$index,labels=w2$code)+
      ggplot2::theme(axis.text=ggplot2::element_text(size=18),
            axis.title=ggplot2::element_text(size=18,face="bold"),
            legend.text = ggplot2::element_text(size=14),
            legend.title = ggplot2::element_text(size=18)) +
      ggnewscale::new_scale_color() +
      ggplot2::geom_vline(data = w3, ggplot2::aes(xintercept = dadaynumber, color = drug))

  })

  output$single_var_plot <- shiny::renderPlot({   print(single_var_plot())    })

  ##--AE Plot--##

  ## Implemented early AE time cut point annotation
  ## Added drug  annotation vline
  ## Implemented option display of day annotation
  ## Rounded day annotation to whole number

  single_var_plot_early <- shiny::reactive({

    AEearly_Cut <- input$AEplot_EarlyAECut
    # print("AEearly_Cut")
    # print(AEearly_Cut)
    TimeDisplay <- input$AEplot_ShowTime
    # print("TimeDisplay")
    # print(TimeDisplay)
    AEearly_Cut_opt <- TRUE
    if (is.na(AEearly_Cut)) {
      AEearly_Cut_opt <- FALSE
    } else {}

    w1<-AEandDemoData() %>% dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE  & is.na(on_treatment_date) == FALSE)
    # print("names in w1")
    # print(names(w1))
    subject<-input$single_var_input

    w1 <- w1 %>% dplyr::filter(sequence_no == subject)
    name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date' ,'grade')
    name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
    #w2<-w1%>%dplyr::select(c(name1,name2))
    w2<-w1%>%dplyr::select(dplyr::any_of(c("sequence_no",name1,name2)))
    w2[,name2][is.na(w2[,name2])] <- 'Not  Applicable'
    w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x=='Not  Applicable'))
    w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
    w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                   t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                   t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days')),
                   code=cdus_ctcae_toxicity_type_code)
    w2$index<-as.numeric(factor(w2$code))
    w2$grade<-as.factor(w2$grade)



    if("start_date_of_drug" %in% names( da_data_upload())) {
      da_data_subject <- da_data_upload() %>% dplyr::filter(sequence_no == subject)
    }else{

      da_data_subject <- da_data_upload() %>% dplyr::mutate(start_date_of_drug = start_date) %>% dplyr::filter(sequence_no == subject)
    }

    if("drug" %in% names( da_data_subject)) {
      da_data_subject <-da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)


    }else{

      da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)


    }

    #w2.2 <- w2 %>% dplyr::select(on_treatment_date) %>% dplyr::distinct(on_treatment_date)
    w2.2early <- w2 %>% dplyr::distinct(on_treatment_date, .keep_all = TRUE)

    w3 <- merge(  da_data_subject, w2.2early %>% dplyr::select(on_treatment_date)) %>% dplyr::mutate(dadaynumber = as.numeric(difftime(start_date_of_drug ,on_treatment_date,units='days')) )

    UL <- max(w2$t2)+10

      
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    lastAEdaynumber <- max(w2$t2)

    #print("WHAT ABOUT w3 the drug admin data....:")
    #print(w3%>% dplyr::select(sequence_no,start_date_of_drug , drug,dadaynumber))

    w3<-w3%>% dplyr::select(sequence_no,start_date_of_drug , drug,dadaynumber) %>% dplyr::filter(dadaynumber<=lastAEdaynumber)
    #print("unique(w3$drug)")
    #print(unique(w3$drug))
    #print("^^^^^^^^^^^ THIS IS THE DATA TO PLOT ^^^^^^^^^^^^^^^^")

    #early_AE_plot_manuscript.fun<-function(AE.data= w2,early.time=365,k=1.1,k1=5,early.AE.status=FALSE)
    early_AE_plot_manuscript.fun<-function(AE.data= w2,early.time=AEearly_Cut,k=1.1,k1=2,early.AE.status=AEearly_Cut_opt)

    {

      if (is.na(early.time)) {
        early.time <- 30
      }

      vline.fun<-function(early.AE.status.tmp,early.time.tmp) if(early.AE.status.tmp) ggplot2::geom_vline(xintercept = early.time.tmp,linetype="dotted")
      geom_label.no_early_AE.fun<-function(early.AE.status.tmp) if(!early.AE.status.tmp) ggplot2::geom_label(ggplot2::aes(x=t2-(t12/2),y=index,label = t12), inherit.aes = F,hjust=0.5,size = 7)
      geom_label.early_AE.fun<-function(early.AE.status.tmp) if(early.AE.status.tmp) ggplot2::geom_label(ggplot2::aes(x=t2-(t12/2),y=index,label = t12.early), inherit.aes = F,hjust=0.5,size = 7)

      # w1<-AE.data
      g.cols=c('lightgreen','yellow','orange','red',"purple",'blue','green','gray25','gray75' )
      #g.cols=c("pink","purple",  'red',"blue",'black' )
      w1<-AE.data %>% dplyr::filter(is.na(resolved_date ) == FALSE  & is.na(on_treatment_date ) == FALSE)

      subject<-unique(w1$sequence_no)
      name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date',#'AE.time',
               'grade')
      name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
      #w2<-w1%>%dplyr::select(c(name1,name2))
      w2<-w1%>%dplyr::select(dplyr::any_of(c(name1,name2)))
      #  w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x=='Not  Applicable'))
      w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x%in%c('Not  Applicable',NA)))
      w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
      w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                     t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                     t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days'))+1,
                     code=cdus_ctcae_toxicity_type_code)
      w2$index<-as.numeric(factor(w2$code))

      w2$t1 <- round(w2$t1)
      w2$t2 <- round(w2$t2)
      w2$t12 <- round(w2$t12)

      if(early.AE.status){
        w2=w2%>%dplyr::mutate(AE.early.indicator=(t1<=early.time)*(t2>=early.time))%>%
          dplyr::mutate(t12.early=ifelse(AE.early.indicator%in%1,paste(early.time-t1+ifelse(early.time==0,1,0),'+',t2-early.time+1,sep=''),t12))
      }


      #       print("WE MUST PRINT w2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      #       print(names(w2))
      #       print(dim(w2))
      # print(w2)
      # print("^^^^^^^^^^^^^^^^^^^^^^^^^^ w2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      # print("^^^^^^^^^^^^^^^^^^^^^^^^^^ w2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      # print("^^^^^^^^^^^^^^^^^^^^^^^^^^ w2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      # print("^^^^^^^^^^^^^^^^^^^^^^^^^^ w2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      # print("^^^^^^^^^^^^^^^^^^^^^^^^^^ w2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      #g.cols=c("purple","blue",'green','black','gray75','gray25' ,'tan','tan2'  )
      # if(length((0:floor(max(w2$t2)/early.time))*early.time)==5){      g.cols=c("pink","purple",  'red',"blue",'black' )}
      # if(length((0:floor(max(w2$t2)/early.time))*early.time)==6){      g.cols=c("pink","purple",  'red',"blue",'black','yellow' )}
      # if(length((0:floor(max(w2$t2)/early.time))*early.time)==7){      g.cols=c("pink","purple",  'red',"blue",'black','yellow','orange' )}
      # if(length((0:floor(max(w2$t2)/early.time))*early.time)==8){      g.cols=c("pink","purple",  'red',"blue",'black','yellow','orange','green' )}
      # if(length((0:floor(max(w2$t2)/early.time))*early.time)==9){      g.cols=c("pink","purple",  'red',"blue",'black','yellow','gray' )}
      #
      plot1 <- ggplot2::ggplot(w2, ggplot2::aes(t1, index,color=grade,shape=treatment_related, label = code)) +
        #ggplot2::geom_vline(data = w3, ggplot2::aes(xintercept = dadaynumber, color = drug)) +
        ggplot2::geom_point(ggplot2::aes(t2, index),size=2*k1)+
        #ggplot2::geom_segment(ggplot2::aes(xend = t2, yend = index), size = 1*k1, lineend = "butt")
        ggplot2::geom_segment(ggplot2::aes(xend = t2, yend = index), linewidth = 1*k1, lineend = "butt")
      if (TimeDisplay == TRUE) {
        plot1 <- plot1 +
          geom_label.no_early_AE.fun(early.AE.status.tmp=early.AE.status)+  ## Display full numbers
          geom_label.early_AE.fun(early.AE.status.tmp=early.AE.status)      ## Display Cut numbers
      }
      #geom_label.no_early_AE.fun(early.AE.status.tmp=early.AE.status)+
      #geom_label.early_AE.fun(early.AE.status.tmp=early.AE.status)+
      #    geom_label(ggplot2::aes(x=t2-(t12/2),y=index,label = t12), inherit.aes = F,hjust=1,size = 7)+

      plot1 <- plot1 +
        ggplot2::xlab('days')+ggplot2::ylab('')+
        ggplot2::scale_y_continuous(breaks=w2$index,labels=w2$code)+
        ggplot2::scale_x_continuous(breaks=(0:floor(max(w2$t2)/early.time))*early.time,limits=c(0, max(w2$t2)))+
        #    xlim(0,NA)+
        ggplot2::scale_color_manual(values=g.cols, drop=FALSE) +
        vline.fun(early.AE.status.tmp=early.AE.status,early.time.tmp=early.time)+
        ggplot2::theme(plot.title = ggplot2::element_text(size=18*k,face="bold"),
              legend.position="top",
              axis.text=ggplot2::element_text(size=14*k),
              axis.title=ggplot2::element_text(size=18*k,face="bold"),
              legend.text = ggplot2::element_text(size=16*k),
              legend.title = ggplot2::element_text(size=18*k),
              legend.direction = "vertical", legend.box = "horizontal")+
        ggnewscale::new_scale_color() +
        ggplot2::geom_vline(data = w3, ggplot2::aes(xintercept = dadaynumber, color = drug)) +
        ggplot2::ggtitle(paste0("Adverse events for ID: ", as.character(subject) ))#+
      # guides(color = guide_legend(nrow = 2))
      #scale_color_viridis() #discrete ==TRUE
      # w2$listdaynumber <- map2(w2$t1,w2$t2,function(.x, .y){.x:.y})
      # w2$totalnumberofdayswithAEs = length(unlist( w2$listdaynumber))
      # w2$numberofdayswithAEs <- length(unique(unlist( w2$listdaynumber)))
      #list(data= w2, plot=plot1)
      plot1
    }

    early_AE_plot_manuscript.fun()
  })

  output$single_var_plot_early <- shiny::renderPlot({   print(single_var_plot_early())    })





  # allow users to download the AE swimmers plot ####
  { output$download_single_var_plot_early <- shiny::downloadHandler(
    filename = function() {
      #IDnum<-IDnum()
      paste0("plot_of_ID_", as.character(input$single_var_input), "_", Sys.Date(), ".png")

    },
    content = function(file) { ggplot2::ggsave(file, plot = single_var_plot_early(),  width = 14,
                                      height = 8,
                                      units = c("in"),#, "cm", "mm", "px"),
                                      dpi = 300 )   }
  )
  }

  output$renddownload_ALL_var_plot_early <- shiny::renderUI({

    req(draw_AEplots())
    downloadButton('download_ALL_var_plot_early',"Save all plots")

  })

  draw_AEplots <- shiny::eventReactive(input$runAllAEplots, {

    plot_list <- list()
    i <- 1
    for (subjectID in unique(AEandDemoData()[,1])) {
      AEearly_Cut <- input$AEplot_EarlyAECut
      TimeDisplay <- input$AEplot_ShowTime
      AEearly_Cut_opt <- TRUE
      if (is.na(AEearly_Cut)) {
        AEearly_Cut_opt <- FALSE
      } else {}

      w1<-AEandDemoData() %>% dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE  & is.na(on_treatment_date) == FALSE)
      subject<-subjectID

      w1 <- w1 %>% dplyr::filter(sequence_no == subject)
      name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date' ,'grade')
      name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
      w2<-w1%>%dplyr::select(dplyr::any_of(c(name1,name2)))
      w2[,name2][is.na(w2[,name2])] <- 'Not  Applicable'
      w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x=='Not  Applicable'))
      w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
      w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                     t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                     t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days')),
                     code=cdus_ctcae_toxicity_type_code)
      w2$index<-as.numeric(factor(w2$code))
      w2$grade<-as.factor(w2$grade)
      if("start_date_of_drug" %in% names( da_data_upload())) {
        da_data_subject <- da_data_upload() %>% dplyr::filter(sequence_no == subject)
      }else{
        da_data_subject <- da_data_upload() %>% dplyr::mutate(start_date_of_drug = start_date) %>% dplyr::filter(sequence_no == subject)
      }
      if("drug" %in% names( da_data_subject)) {
        da_data_subject <-da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }else{
        da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }
      w2.2early <- w2 %>% dplyr::distinct(on_treatment_date, .keep_all = TRUE)

      w3 <- merge(  da_data_subject, w2.2early %>% dplyr::select(on_treatment_date)) %>% dplyr::mutate(dadaynumber = as.numeric(difftime(start_date_of_drug ,on_treatment_date,units='days')) )

      UL <- max(w2$t2)+10

      lastAEdaynumber <- max(w2$t2)
      #print(w3%>% dplyr::select(sequence_no,start_date_of_drug , drug,dadaynumber))

      w3<-w3%>% dplyr::select(sequence_no,start_date_of_drug , drug,dadaynumber) %>% dplyr::filter(dadaynumber<=lastAEdaynumber)

      #early_AE_plot_manuscript.fun<-function(AE.data= w2,early.time=365,k=1.1,k1=5,early.AE.status=FALSE)
      early_AE_plot_manuscript.fun<-function(AE.data= w2,early.time=AEearly_Cut,k=1.1,k1=2,early.AE.status=AEearly_Cut_opt) {

        if (is.na(early.time)) {
          early.time <- 30
        }

        vline.fun<-function(early.AE.status.tmp,early.time.tmp) if(early.AE.status.tmp)    ggplot2::geom_vline(xintercept = early.time.tmp,linetype="dotted")
        geom_label.no_early_AE.fun<-function(early.AE.status.tmp) if(!early.AE.status.tmp) geom_label(ggplot2::aes(x=t2-(t12/2),y=index,label = t12), inherit.aes = F,hjust=0.5,size = 3)
        geom_label.early_AE.fun<-function(early.AE.status.tmp) if(early.AE.status.tmp) geom_label(ggplot2::aes(x=t2-(t12/2),y=index,label = t12.early), inherit.aes = F,hjust=0.5,size = 3)

        # w1<-AE.data
        g.cols=c('lightgreen','yellow','orange','red',"purple",'blue','green','gray25','gray75' )
        w1<-AE.data %>% dplyr::filter(is.na(resolved_date ) == FALSE  & is.na(on_treatment_date ) == FALSE)

        subject<-unique(w1$sequence_no)
        name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date',#'AE.time',
                 'grade')
        name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
        w2<-w1%>%dplyr::select(dplyr::any_of(c(name1,name2)))
        w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x%in%c('Not  Applicable',NA)))
        w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
        w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                       t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                       t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days'))+1,
                       code=cdus_ctcae_toxicity_type_code)
        w2$index<-as.numeric(factor(w2$code))
        w2$t1 <- round(w2$t1)
        w2$t2 <- round(w2$t2)
        w2$t12 <- round(w2$t12)
        if(early.AE.status){
          w2=w2%>%dplyr::mutate(AE.early.indicator=(t1<=early.time)*(t2>=early.time))%>%
            dplyr::mutate(t12.early=ifelse(AE.early.indicator%in%1,paste(early.time-t1+ifelse(early.time==0,1,0),'+',t2-early.time+1,sep=''),t12))
        }
        plot1 <- ggplot2::ggplot(w2, ggplot2::aes(t1, index,color=grade,shape=treatment_related, label = code)) +
          ggplot2::geom_point(ggplot2::aes(t2, index),size=2*k1)+
          ggplot2::geom_segment(ggplot2::aes(xend = t2, yend = index), linewidth = 1*k1, lineend = "butt")
        plot1 <- plot1 +
          ggplot2::xlab('days')+ggplot2::ylab('')+
          ggplot2::scale_y_continuous(breaks=w2$index,labels=w2$code)+
          ggplot2::scale_x_continuous(breaks=(0:floor(max(w2$t2)/early.time))*early.time,limits=c(0, max(w2$t2)))+
          #    xlim(0,NA)+
          ggplot2::scale_color_manual(values=g.cols, drop=FALSE) +
          vline.fun(early.AE.status.tmp=early.AE.status,early.time.tmp=early.time)+
          ggplot2::theme(plot.title = ggplot2::element_text(size=14*k,face="bold"),
                legend.position="top",
                axis.text=ggplot2::element_text(size=10*k),
                axis.title=ggplot2::element_text(size=12*k,face="bold"),
                legend.text = ggplot2::element_text(size=10*k),
                legend.title = ggplot2::element_text(size=12*k),
                legend.direction = "vertical", legend.box = "horizontal")+
          ggnewscale::new_scale_color() +
          ggplot2::geom_vline(data = w3, ggplot2::aes(xintercept = dadaynumber, color = drug)) +
          ggplot2::ggtitle(paste0("Adverse events for ID: ", as.character(input$single_var_input) ))
        if (TimeDisplay == TRUE) {
          plot1 <- plot1 +
            geom_label.no_early_AE.fun(early.AE.status.tmp=early.AE.status)+  ## Display full numbers
            geom_label.early_AE.fun(early.AE.status.tmp=early.AE.status)      ## Display Cut numbers
        }
        plot1
      }

      plot_list[[i]] <- early_AE_plot_manuscript.fun()
      i <- i+1
    }
    plot_list

  })



  # AE and boxplots of responses####
  draw_AEplot <- function() {  draw_AEplots()  }

  draw_AEplots_react <- shiny::reactive({})

  # Download AE and box plots ####
  #output$plotallAE <- shiny::renderPlot({ draw_AEplot() })

  # Download AE plots pdf ###

  # hopefully for either or PC OR biostools
  # Download AE plots pdf ####
  output$download_ALL_var_plot_early <- shiny::downloadHandler(
    filename = "AE_plots_report.pdf",
    content = function(file) {

      #subject<-input$single_var_input

      filetorender <-  "template_AEplots.Rmd"

      res <- rmarkdown::render(
        input = filetorender , # "template.Rmd",
        params = list(
          draw_AEplot = draw_AEplot
        )
      )

      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }


    }
  )

  # end: AE days tab ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ####
  
  # startl: AE measures tab ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^####
  # Calculate all AE measures!!!!!!!!!!!!. ####
  #alldataoutput <- shiny::reactive({

  output$rendAEmeasuresAEcatselect <- shiny::renderUI({

    AE_data <- AE_data()
    AE_Cats <- unique(AE_data[,"toxicity_category"])
    AE_Cats <- c("All AE Categories",AE_Cats)
    shiny::selectInput("AEmeasuresAEcatselect","Select AE Category", choices = AE_Cats)

  })

  output$rendAEmeasuresAEtypeselect <- shiny::renderUI({

    req(input$AEmeasuresAEcatselect)
    print(input$AEmeasuresAEcatselect)
    if (input$AEmeasuresAEcatselect != "All AE Categories") {
      AE_data <- AE_data()
      AE_Cat <- input$AEmeasuresAEcatselect
      AE_Data_Cat <- AE_data[which(AE_data[,"toxicity_category"] == AE_Cat),]
      print("AE_Data_Cat~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      print(AE_Data_Cat)
     
      if ( !"cdus_ctcae_toxicity_type_code" %in% names(AE_Data_Cat)){
       AE_Data_Cat$cdus_ctcae_toxicity_type_code<-AE_Data_Cat$cdus_toxicity_type_code
      }
       
       AE_Types <- unique(AE_Data_Cat[,"cdus_ctcae_toxicity_type_code"])
      AE_Types <- c("All AE Types",AE_Types)
      print(AE_Types)
      shiny::selectInput("AEmeasuresAEtypeselect","Select AE Type", choices = AE_Types)
    }

  })

  alldataoutput <- shiny::reactive({

    ## New variables
    toxicity_data_input <- toxicity_data()
    AE_CatSelect <- input$AEmeasuresAEcatselect
    AE_TypeSelect <- input$AEmeasuresAEtypeselect
    EarlyAE_CutP <- input$AEmeasuresEarlyAEcut
    if (is.na(EarlyAE_CutP)) {
      EarlyAE_CutP <- NULL
    }
    if (!is.null(AE_CatSelect)) {
      if (AE_CatSelect != "All AE Categories") {
        toxicity_data_input_cat <- toxicity_data_input[which(toxicity_data_input[,"toxicity_category"] == AE_CatSelect),]
        toxicity_data_input <- toxicity_data_input_cat
        if (!is.null(AE_TypeSelect)) {
          if (AE_TypeSelect == "All AE Types") {
            AE_TypeSelect <- NULL
          }
          else if (AE_TypeSelect != "All AE Types") {
            toxicity_data_input_type <- toxicity_data_input_cat[which(toxicity_data_input_cat[,"cdus_ctcae_toxicity_type_code"] == AE_TypeSelect),]
            toxicity_data_input <- toxicity_data_input_type
          }
        }
      }
    }

    #---functions-----
    toxicity.out.fun <- function(toxicity.data = toxicity.data, AE.time.cutoff = NULL) {
      #---identify subject without AE at the AE.time.cutoff--
      id.tmp <- sort(unique(toxicity.data$pid))
      # print("HOW MANY PATIENTS")
      # print(length(id.tmp))
      # print(paste("NROW original tox:", NROW(toxicity.data)))
      if (!is.null(AE.time.cutoff)) { print("WE HAVE A CUTOFF!!!")
        toxicity.data <- toxicity.data %>% dplyr::filter(AE.time < AE.time.cutoff)  # TRUNCATE TIME
        # print(paste("NROW after truncating tox:", NROW(toxicity.data)))
        # print(paste("now how many patients?:",length(sort(unique(toxicity.data$pid)))))
      }
      
      
      toxicity.data.by.id <- by(toxicity.data, toxicity.data$pid, data.frame) # makes a data frame for each id.
      
      
      null.status <- sapply(toxicity.data.by.id, is.null) # tests if null?
      
      
      toxicity.data.by.id <- toxicity.data.by.id[!null.status] # takes if null.status is FALSE
      
      
      id.in.data <- names(toxicity.data.by.id)
      id.no.AE <- id.tmp[!id.tmp %in% id.in.data] # gets patient numbers with out AE
      
      name.group <- c("cdus_ctcae_toxicity_type_code", "toxicity_category")
      name.time <- c("onset_date_of_ae", "resolved_date")
      name.grade <- "grade"
      name.treatment.related <- c("attribution_possible", "attribution_probable", "attribution_definite")
      
      toxicity.type.name <- names(table(as.vector(toxicity.data[, name.group[1]])))
      toxicity.category.name <- names(table(as.vector(toxicity.data[, name.group[2]])))
 
      table1 <- table(toxicity.data$cdus_ctcae_toxicity_type_code, toxicity.data$toxicity_category)
      
      toxicity.type.within.category <- apply(table1, 2, function(x) dimnames(table1)[[1]][x != 0])

    
      # 1136 ADD BIOMARKER NAMES ####
         name.tox.summary <-
        c(
          "all.grade.occurrence", "all.grade.fre", "all.grade.duration",
          "LG.occurrence", "LG.fre", "LG.duration",
          "HG.occurrence", "HG.fre", "HG.duration",
          "all.grade.trt.occurrence", "all.grade.trt.fre", "all.grade.trt.duration",
          "LG.trt.occurrence", "LG.trt.fre", "LG.trt.duration",
          "HG.trt.occurrence", "HG.trt.fre", "HG.trt.duration",
          "LG.ntr.fre","LG.ntr.occurrence","LG.ntr.duration",
          "HG.ntr.fre","HG.ntr.occurrence","HG.ntr.duration",
          "g1.fre","g2.fre","g3.fre","g4.fre","g5.fre", 
          "all.grade.ntr.fre",
          "all.grade.ntr.occurrence" ,
          "all.grade.ntr.duration",
          "g1.trt.fre","g2.trt.fre","g3.trt.fre","g4.trt.fre","g5.trt.fre",
          "g1.nontrt.fre","g2.nontrt.fre","g3.nontrt.fre","g4.nontrt.fre","g5.nontrt.fre",
          "g1.occurrence","g2.occurrence","g3.occurrence","g4.occurrence","g5.occurrence",  
          "g1.duration","g2.duration","g3.duration","g4.duration","g5.duration",
          "g1.trt.duration","g2.trt.duration","g3.trt.duration","g4.trt.duration","g5.trt.duration",
          "g1.ntr.duration","g2.ntr.duration","g3.ntr.duration","g4.ntr.duration","g5.ntr.duration",
          "g1.trt.occurrence","g2.trt.occurrence","g3.trt.occurrence","g4.trt.occurrence","g5.trt.occurrence"
        )
      # LG.ntr.fre <- round(sum(index.all * index.LG * index.not.tretament.related, na.rm = T),0)
      # LG.ntr.occurrence <- round(as.numeric(LG.ntr.fre > 0),0)
      # LG.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.LG & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
      # 
      # HG.ntr.fre <- round(sum(index.all * index.HG * index.not.tretament.related, na.rm = T),0)
      # HG.ntr.occurrence <- round(as.numeric(HG.ntr.fre > 0),0)
      # HG.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.HG & index.not.tretament.related, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
      
      
      duration.fun <- function(x, index.tmp, AE.time.cutoff.tmp) {
        x <- x[index.tmp, , drop = F]
        AE.whole.duration <- as.numeric(difftime(x$resolved_date, x$onset_date_of_ae, units = "days")) + 1 #--add 1 day to avoid 0 for same day of onset and resolved
        
        if (!is.null(AE.time.cutoff.tmp)) {
          max.AE <- AE.time.cutoff.tmp - x$AE.time + 1 #--add 1 day to avoid 0 for same day of onset and initial treatment day
          ans <- ifelse(AE.whole.duration > max.AE, max.AE, AE.whole.duration)
        } else {
          ans <- AE.whole.duration
        }
        
        ans
      }
      
      #---generate long format data--
      tmp1 <- numeric()
      for (i in 1:length(toxicity.type.name))
      {
        #print(toxicity.type.name[i])
        #print(toxicity.category.name)
        tmp0 <- sapply(
          toxicity.data.by.id,
          function(x) {
            # print("dim(x)<-----------------------")
            # print(dim(x))
            # print(x)
            index.all <- x[, name.group[1]] == toxicity.type.name[i]
            index.LG <- (as.numeric(as.vector(x[, name.grade])) < 3)
            index.HG <- (as.numeric(as.vector(x[, name.grade])) >= 3)
            index.tretament.related <- apply(x[, name.treatment.related], 1, function(x) any(x != "Not  Applicable"))
            
            index.not.tretament.related <- apply(x[, name.treatment.related], 1, function(x) all(x == "Not  Applicable"))
            index.grade1 <- (as.numeric(as.vector(x[, name.grade])) == 1)
            index.grade2 <- (as.numeric(as.vector(x[, name.grade])) == 2)
            index.grade3 <- (as.numeric(as.vector(x[, name.grade])) == 3)
            index.grade4 <- (as.numeric(as.vector(x[, name.grade])) == 4)
            index.grade5 <- (as.numeric(as.vector(x[, name.grade])) == 5)
            
            all.grade.fre <- round(sum(index.all, na.rm = T),0)
            all.grade.occurrence <- round(as.numeric(all.grade.fre > 0),0)
            all.grade.duration <- round(sum(duration.fun(x, index.tmp = index.all, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            LG.fre <- round(sum(index.all * index.LG, na.rm = T),0)
            LG.occurrence <- round(as.numeric(LG.fre > 0),0)
            LG.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.LG, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            HG.fre <- round(sum(index.all * index.HG, na.rm = T),0)
            HG.occurrence <- round(as.numeric(HG.fre > 0),0)
            HG.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.HG, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            all.grade.trt.fre <- round(sum(index.all * index.tretament.related, na.rm = T),0)
            all.grade.trt.occurrence <- round(as.numeric(all.grade.trt.fre > 0),0)
            all.grade.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.tretament.related,
                                                                           AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            LG.trt.fre <- round(sum(index.all * index.LG * index.tretament.related, na.rm = T),0)
            LG.trt.occurrence <- round(as.numeric(LG.trt.fre > 0),0)
            LG.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.LG & index.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            HG.trt.fre <- round(sum(index.all * index.HG * index.tretament.related, na.rm = T),0)
            HG.trt.occurrence <- round(as.numeric(HG.trt.fre > 0),0)
            HG.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.HG & index.tretament.related, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            g1.fre <- round(sum(index.all * index.grade1, na.rm = T),0)
            g2.fre <- round(sum(index.all * index.grade2, na.rm = T),0)
            g3.fre <- round(sum(index.all * index.grade3, na.rm = T),0)
            g4.fre <- round(sum(index.all * index.grade4, na.rm = T),0)
            g5.fre <- round(sum(index.all * index.grade5, na.rm = T),0)
            
             g1.occurrence <-  round(as.numeric(g1.fre > 0),0) #  NOT CORRECT...
             g2.occurrence <-  round(as.numeric(g2.fre > 0),0)
             g3.occurrence <-  round(as.numeric(g3.fre > 0),0)
             g4.occurrence <-  round(as.numeric(g4.fre > 0),0)
             g5.occurrence <-  round(as.numeric(g5.fre > 0),0)
            
            g1.trt.fre <- sum(index.all * index.grade1 * index.tretament.related, na.rm = T)
            g2.trt.fre <- sum(index.all * index.grade2 * index.tretament.related, na.rm = T)
            g3.trt.fre <- sum(index.all * index.grade3 * index.tretament.related, na.rm = T)
            g4.trt.fre <- sum(index.all * index.grade4 * index.tretament.related, na.rm = T)
            g5.trt.fre <- sum(index.all * index.grade5 * index.tretament.related, na.rm = T)
            
            g1.trt.occurrence <-  round(as.numeric(g1.trt.fre > 0),0) #  NOT CORRECT...
            g2.trt.occurrence <-  round(as.numeric(g2.trt.fre > 0),0)
            g3.trt.occurrence <-  round(as.numeric(g3.trt.fre > 0),0)
            g4.trt.occurrence <-  round(as.numeric(g4.trt.fre > 0),0)
            g5.trt.occurrence <-  round(as.numeric(g5.trt.fre > 0),0)
            
            all.grade.ntr.fre <- round(sum(index.all * index.not.tretament.related, na.rm = T),0)
            all.grade.ntr.occurrence <- round(as.numeric(all.grade.ntr.fre > 0),0)
            all.grade.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g1.nontrt.fre <- sum(index.all * index.grade1 * index.not.tretament.related, na.rm = T)
            g2.nontrt.fre <- sum(index.all * index.grade2 * index.not.tretament.related, na.rm = T)
            g3.nontrt.fre <- sum(index.all * index.grade3 * index.not.tretament.related, na.rm = T)
            g4.nontrt.fre <- sum(index.all * index.grade4 * index.not.tretament.related, na.rm = T)
            g5.nontrt.fre <- sum(index.all * index.grade5 * index.not.tretament.related, na.rm = T)
            
            LG.ntr.fre <- round(sum(index.all * index.LG * index.not.tretament.related, na.rm = T),0)
            LG.ntr.occurrence <- round(as.numeric(LG.ntr.fre > 0),0)
            LG.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.LG & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            HG.ntr.fre <- round(sum(index.all * index.HG * index.not.tretament.related, na.rm = T),0)
            HG.ntr.occurrence <- round(as.numeric(HG.ntr.fre > 0),0)
            HG.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.HG & index.not.tretament.related, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            
            
            g1.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade1, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g2.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade2, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g3.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade3, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g4.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade4, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g5.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade5, AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)

            g1.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade1 & index.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g2.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade2 & index.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g3.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade3 & index.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g4.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade4 & index.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g5.trt.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade5 & index.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
            g1.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade1 & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g2.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade2 & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g3.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade3 & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g4.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade4 & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            g5.ntr.duration <- round(sum(duration.fun(x, index.tmp = index.all & index.grade5 & index.not.tretament.related,AE.time.cutoff.tmp = AE.time.cutoff), na.rm = T),0)
            
          
               # 1293 ADD NAMES HERE #### 
            c(
              all.grade.occurrence, all.grade.fre, all.grade.duration,
              LG.occurrence, LG.fre, LG.duration,
              HG.occurrence, HG.fre, HG.duration,
              all.grade.trt.occurrence, all.grade.trt.fre, all.grade.trt.duration,
              LG.trt.occurrence, LG.trt.fre, LG.trt.duration,
              HG.trt.occurrence, HG.trt.fre, HG.trt.duration,
              LG.ntr.fre,LG.ntr.occurrence,LG.ntr.duration,
              HG.ntr.fre,HG.ntr.occurrence,HG.ntr.duration,
              g1.fre,g2.fre,g3.fre,g4.fre,g5.fre
              ,
              all.grade.ntr.fre,
              all.grade.ntr.occurrence
              ,all.grade.ntr.duration,
              g1.trt.fre,g2.trt.fre,g3.trt.fre,g4.trt.fre,g5.trt.fre,
              g1.nontrt.fre,g2.nontrt.fre,g3.nontrt.fre,g4.nontrt.fre,g5.nontrt.fre,
              g1.occurrence,g2.occurrence,g3.occurrence,g4.occurrence,g5.occurrence,  
              g1.duration,g2.duration,g3.duration,g4.duration,g5.duration,
              g1.trt.duration,g2.trt.duration,g3.trt.duration,g4.trt.duration,g5.trt.duration,
              g1.ntr.duration,g2.ntr.duration,g3.ntr.duration,g4.ntr.duration,g5.ntr.duration,
              g1.trt.occurrence,g2.trt.occurrence,g3.trt.occurrence,g4.trt.occurrence,g5.trt.occurrence
            )
          }
        )
        
        # print(str(tmp1,2,1))
        # print(class(tmp0))
        # print(str(tmp0,2,1))

        
        rownames(tmp0) <- name.tox.summary # not working now ?
        tmp0 <- as.data.frame(tmp0) %>%
          #dplyr::add_rownames(var = "measurement") %>%
          tibble::rownames_to_column(var = "measurement") %>%
          dplyr::relocate(measurement)
        tmp0 <- tmp0 %>% tidyr::pivot_longer(cols = -1, names_to = "pid", values_to = "value")
        tmp0$AE <- toxicity.type.name[i]
        tmp0$AE.category <- dimnames(table1)[[2]][table1[toxicity.type.name[i], ] != 0]
        
        # tmp1 <- rbind(tmp1, tmp0)
         # print(str(tmp1,2,1)) 
   
        tmp1 <- rbind(tmp1, tmp0)
      }
      
      # print("tmp1----------------------------------")
      # print(as.data.frame(tmp1))
      # 
      #---summary function---
      
      
      AE.summary.fun <- function(data, var1, summary.status = T, id.no.AE.tmp = id.no.AE) {
        # data is a long format matrix with pid, AE, AE.catergory, measurement type, and the value
        # var1 = NULL for overall summary over all ggplot2::aes
        if (summary.status) {
          data.summary.long <- data %>%
            dplyr::group_by(pid, measurement) %>%
            dplyr::summarise(sum = sum(value))
          data.summary.wide <- data.summary.long %>% tidyr::pivot_wider(names_from = measurement, values_from = sum)
          data.tmp <- data.summary.wide
          if (length(id.no.AE.tmp) > 0) {
            data.tmp.no.AE <- data.tmp[1:length(id.no.AE.tmp), , drop = F]
            data.tmp.no.AE$pid <- id.no.AE.tmp
            data.tmp.no.AE[, 2:dim(data.tmp.no.AE)[2]] <- 0
            data.tmp$pid <- as.character(data.tmp$pid)              ## Fix for issue with column classes not matching
            data.tmp.no.AE$pid <- as.character(data.tmp.no.AE$pid)  ## Fix for issue with column classes not matching
            data.tmp <- rbind(data.tmp, data.tmp.no.AE)
          }
          AE.data.list <- data.tmp
        } else {
          # this step is to sum the value within pid and AE or AE.category
          data.summary.long <- data %>%
            dplyr::group_by(pid, {{ var1 }}, measurement) %>%
            dplyr::summarise(sum = sum(value))
          data.summary.wide <- data.summary.long %>% tidyr::pivot_wider(names_from = measurement, values_from = sum)
          data.for.name <- data.summary.wide %>% dplyr::select({{ var1 }})
          name1 <- names(table(data.for.name[, 2]))
          
          AE.data.list <- map(
            as.list(name1),
            function(x) {
              # under each AE or AE.category
              data.tmp <- data.summary.wide %>% dplyr::filter({{ var1 }} %in% x)
              if (length(id.no.AE.tmp) > 0) {
                data.tmp.no.AE <- data.tmp[1:length(id.no.AE.tmp), , drop = F]
                data.tmp.no.AE$pid <- id.no.AE.tmp
                data.tmp.no.AE[, 3:dim(data.tmp.no.AE)[2]] <- 0
                data.tmp$pid <- as.character(data.tmp$pid)               ## Fix for issue with column classes not matching
                data.tmp.no.AE$pid <- as.character(data.tmp.no.AE$pid)   ## Fix for issue with column classes not matching
                data.tmp <- rbind(data.tmp, data.tmp.no.AE)
              }
              data.tmp
            }
          )
          names(AE.data.list) <- name1
        }
        AE.data.list
      } # END AE.summary.fun
      
      data.raw.long <- tmp1
      toxicity.whole.summary.data <- AE.summary.fun(data = tmp1, var1 = "", summary.status = T, id.no.AE.tmp = id.no.AE)
      toxicity.category.summary.data <- AE.summary.fun(data = tmp1, var1 = AE.category, summary.status = F, id.no.AE.tmp = id.no.AE)
      toxicity.type.summary.data <- AE.summary.fun(data = tmp1, var1 = AE, summary.status = F, id.no.AE.tmp = id.no.AE)
      #
      
      list(
        id.no.AE = id.no.AE
        ,data.raw.long = data.raw.long
        ,toxicity.type.within.category = toxicity.type.within.category
        ,toxicity.category.summary.data = toxicity.category.summary.data
        ,toxicity.type.summary.data = toxicity.type.summary.data
        ,toxicity.whole.summary.data = toxicity.whole.summary.data
      )
    } # end toxicity.out.fun
    
    

    #alldataoutput <- toxicity.out.fun(toxicity.data = toxicity_data(), AE.time.cutoff = NULL)
    alldataoutput <- toxicity.out.fun(toxicity.data = toxicity_data_input, AE.time.cutoff = EarlyAE_CutP)
    #save( alldataoutput which is the output of toxicity.out.fun)####
    # save(alldataoutput, file = "F:\\myGitRepo\\alldataoutput.RData" )
     alldataoutput

  })

  # MERGING AE measures with follow up and demographics to make TOXDATA with all the measures ####
  #toxicity.whole.summary.data <- shiny::reactive({
  toxicity.whole.summary.data <- shiny::eventReactive(input$goAEmeasures,{
    #input$goAEmeasures
    toxdata00 <- merge(alldataoutput()$toxicity.whole.summary.data,fu_data_upload(),by.x = "pid", by.y = "sequence_no")
    toxdata0 <- merge(toxdata00,demographics_data(),by.x = "pid", by.y = "sequence_no")


    #LINEs BELOW ADDED 20221207

    if(I("off_treatment_date.y" %in% names(toxdata0)) == FALSE){
      #print("off_treatment_date.y NOT IN DATA REPLACe WITH off_treatment_date")
      toxdata0 <- toxdata0  %>% dplyr::mutate(off_treatment_date.y = off_treatment_date )}

    if(I("last_visit_date" %in% names(toxdata0)) == FALSE){
      #print("LAST VISIT NOT IN DATA REPLACe WITH LAST_FOLLOWUP_DATE")
      toxdata0 <- toxdata0  %>% dplyr::mutate(last_visit_date = last_followup_date )}


    if(all(is.na(toxdata0$last_followup_date))){toxdata0$last_followup_date <- toxdata0$last_visit_date}


    #above ^^^added 20221207
    toxdata1 <- toxdata0  %>% dplyr::mutate(os_censor  = ifelse(is.na(expired_date) == FALSE, 1, 0)) %>%
      dplyr::mutate(sequence_no = pid,
             #treatment.time  = off_treatment_date.y - on_treatment_date,
             treatment.time  = difftime(off_treatment_date.y, on_treatment_date, units = "days"),
             #lastdate = pmax( last_followup_date, last_visit_date, na.rm = TRUE),
             lastdate = as.Date(pmax( last_followup_date, last_visit_date, na.rm = TRUE)),

             os_time  = ifelse(os_censor  == 1, lubridate::interval(on_treatment_date, expired_date) / months(1),
                               lubridate::interval(on_treatment_date, lastdate) / months(1)))

    toxdata <- toxdata1 %>%   dplyr::mutate(
      #lastdate = pmax( last_followup_date, last_visit_date, na.rm = TRUE),
      lastdate = as.Date(pmax( last_followup_date, last_visit_date, na.rm = TRUE)),
      pfs_censor  = ifelse(is.na(date_of_progression) == FALSE, 1, 0),
      pfs_time   =    ifelse(pfs_censor  == 1,
                             lubridate::interval(on_treatment_date, date_of_progression) / months(1),
                             lubridate::interval(on_treatment_date, lastdate) / months(1)) )  %>%
      dplyr::select(sequence_no, pid, everything()) 
    toxicity.whole.summary.data<-toxdata
    # save(list = ls(), file = "F:\\myGitRepo\\Tox_Env.RData", envir = environment())
    #save(toxicity.whole.summary.data#####
    # save(toxicity.whole.summary.data, file = "F:\\myGitRepo\\toxicity.whole.summary.data.withfollwupanddemographics.RData" )
    print("names(toxdata)!!!!!!!")
    print(names(toxdata))
    toxdata

  })
# *** DISPLAY OF AE DATA ####
  output$toxicitytableoutput <- DT::renderDataTable({
    print("DATA TO DISPLAY??")
    todisplay <- toxicity.whole.summary.data()
     todisplay <- todisplay[,c(1,3:NCOL(todisplay))]
    todisplay2<-todisplay <- todisplay %>% select("sequence_no",           
  "all.grade.fre","HG.fre","LG.fre",
  "g1.fre","g2.fre","g3.fre","g4.fre","g5.fre",
  "all.grade.trt.fre","HG.trt.fre","LG.trt.fre",
  "g1.trt.fre","g2.trt.fre","g3.trt.fre","g4.trt.fre","g5.trt.fre",
  "all.grade.ntr.fre","HG.ntr.fre","LG.ntr.fre", 
  "g1.nontrt.fre", "g2.nontrt.fre", "g3.nontrt.fre", "g4.nontrt.fre", "g5.nontrt.fre",
  
  "all.grade.duration","HG.duration", "LG.duration", 
  "g1.duration", "g2.duration", "g3.duration", "g4.duration", "g5.duration",
  "all.grade.trt.duration","HG.trt.duration", "LG.trt.duration", 
  "g1.trt.duration", "g2.trt.duration", "g3.trt.duration", "g4.trt.duration", "g5.trt.duration",
  "all.grade.ntr.duration","HG.ntr.duration","LG.ntr.duration",
  "g1.ntr.duration", "g2.ntr.duration","g3.ntr.duration", "g4.ntr.duration","g5.ntr.duration",
  "g1.occurrence","g2.occurrence","g3.occurrence","g4.occurrence","g5.occurrence",  
  
  "all.grade.occurrence","HG.occurrence","LG.occurrence",
  "HG.trt.occurrence","LG.trt.occurrence","all.grade.trt.occurrence" ,  
  "all.grade.ntr.occurrence","HG.ntr.occurrence","LG.ntr.occurrence",
everything()) %>% 
      select(-off_treatment_date.y,-off_study_date.y )
    # save(todisplay, file = "F:\\myGitRepo\\todisplay.RData" )
    # save(todisplay2, file = "F:\\myGitRepo\\todisplay2.RData" )
    # 
    
    
    print(names(toxicity.whole.summary.data()))
    print(dim(toxicity.whole.summary.data()))
    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 100) )
  })

  # Down load toxicity.whole.summary.data()  ####
  # output$toxicitymeasuresdownload <- shiny::downloadHandler(
  #   filename = function(){"toxicitymeasures.csv"},
  #   content = function(fname){
  #     write.csv(toxicity.whole.summary.data(), fname, row.names = FALSE)
  #   }
  # )
  
  output$toxicitymeasuresdownload <- shiny::downloadHandler(
    filename = function(){"toxicitymeasures.csv"},
    content = function(fname){
      tempoutdata<- toxicity.whole.summary.data() %>% select("sequence_no",           
                                                            "all.grade.fre","HG.fre","LG.fre",
                                                            "g1.fre","g2.fre","g3.fre","g4.fre","g5.fre",
                                                            "all.grade.trt.fre","HG.trt.fre","LG.trt.fre",
                                                            "g1.trt.fre","g2.trt.fre","g3.trt.fre","g4.trt.fre","g5.trt.fre",
                                                            "all.grade.ntr.fre","HG.ntr.fre","LG.ntr.fre", 
                                                            "g1.nontrt.fre", "g2.nontrt.fre", "g3.nontrt.fre", "g4.nontrt.fre", "g5.nontrt.fre",
                                                            
                                                            "all.grade.duration","HG.duration", "LG.duration", 
                                                            "g1.duration", "g2.duration", "g3.duration", "g4.duration", "g5.duration",
                                                            "all.grade.trt.duration","HG.trt.duration", "LG.trt.duration", 
                                                            "g1.trt.duration", "g2.trt.duration", "g3.trt.duration", "g4.trt.duration", "g5.trt.duration",
                                                            "all.grade.ntr.duration","HG.ntr.duration","LG.ntr.duration",
                                                            "g1.ntr.duration", "g2.ntr.duration","g3.ntr.duration", "g4.ntr.duration","g5.ntr.duration",
                                                            "g1.occurrence","g2.occurrence","g3.occurrence","g4.occurrence","g5.occurrence",  
                                                            
                                                            "all.grade.occurrence","HG.occurrence","LG.occurrence",
                                                            "HG.trt.occurrence","LG.trt.occurrence","all.grade.trt.occurrence" ,  
                                                            "all.grade.ntr.occurrence","HG.ntr.occurrence","LG.ntr.occurrence",
                                                            everything()) %>% 
        select(-off_treatment_date.y,-off_study_date.y )
      write.csv(tempoutdata, fname, row.names = FALSE)
    }
  )
  
  
  # end: AE measures tab ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^####
  
  # start: RECIST Data vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv####

  output$rendrecist_plot_patient <- shiny::renderUI({
    Patient_select <- unique(AEandDemoData()$sequence_no)[1]
    if (isTruthy(input$single_var_input)) {
      if (input$single_var_input %in% unique(AEandDemoData()$sequence_no)) {
        Patient_select <- input$single_var_input
      }
    }
    shiny::selectInput("recist_plot_patient","Patient number:",
                choices = unique(AEandDemoData()$sequence_no), selected = Patient_select)
  })

  observe({
    shiny::updateSelectizeInput(session,"single_var_input", selected = input$recist_plot_patient)
  })

  recist_plot_data <- shiny::reactive({

    req(input$recist_plot_patient)
    subject <- input$recist_plot_patient
    recist_data <- recist_data()

    # Subset to patient of interest
    w1_re <- recist_data %>% dplyr::filter(is.na(date_of_procedure) == FALSE)
    w1_re <- w1_re %>% dplyr::filter(sequence_no == subject)
    w2_re <- w1_re

    # Calculate days from baseline
    w2_re$Days_From_Baseline <- sapply(w2_re[,"date_of_procedure"],function(x) {
      difftime(x,w2_re[1,"date_of_procedure"], units = "days")
    })
    #save(list = ls(), file = "recist_env.RData", envir = environment())
    w2_re$Months_From_Baseline <- w2_re$Days_From_Baseline/30.4375
    w2_re$Change_From_Baseline <- (w2_re$percent_change_from_baseline/100) + 1
    w2_re

  })

  recist_plot_react <- shiny::reactive({

    req(input$recist_plot_patient)
    subject <- input$recist_plot_patient
    w2_re <- recist_plot_data()
    da_data_upload <- da_data_upload()
    fu_data_upload <- fu_data_upload()
    AEandDemoData <- AEandDemoData()

    # Subset Columns of interest
    name1 <- c("date_of_procedure","percent_change_from_baseline","percent_change_from_smallest_sum","response",
               "Days_From_Baseline","Months_From_Baseline","Change_From_Baseline")
    w2_re <- w2_re %>% dplyr::select(dplyr::any_of(c(name1))) %>% unique()
    my_theme = ggplot2::theme(axis.title = ggplot2::element_text(size = 20), axis.text = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15), strip.text = ggplot2::element_text(size = 15))

    k=1.1
    k1=2

    # Drug administration Data
    if("start_date_of_drug" %in% names( da_data_upload)) {
      da_data_subject <- da_data_upload %>% dplyr::filter(sequence_no == subject)
    }else{
      da_data_subject <- da_data_upload %>% dplyr::mutate(start_date_of_drug = start_date) %>% dplyr::filter(sequence_no == subject)
    }
    if("drug" %in% names( da_data_subject)) {
      da_data_subject <- da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)
    }else{
      da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)
    }
    FirstPR <- round(min(w2_re[which(w2_re$response == "Partial Response (PR)"),"Months_From_Baseline"]),2)
    FirstPR <- ifelse(is.infinite(FirstPR),NA,FirstPR)
    LastR <- round(max(w2_re[,"Months_From_Baseline"]),2)

    if ("last_visit_date" %in% colnames(AEandDemoData)) {
      AEandDemoData_subject <- AEandDemoData[which(AEandDemoData[,1] == subject),]
      LastVisit <- unique(difftime(AEandDemoData_subject[,"last_visit_date"],AEandDemoData_subject[,"on_treatment_date"], units = "days"))
      LastVisit <- round(LastVisit/30.4375,2)
    } else {
      LastVisit <- NULL
    }


    da_data_subject$Days_From_Baseline <- sapply(da_data_subject[,"start_date_of_drug"],function(x) {
      difftime(x,w2_re[1,"date_of_procedure"], units = "days")
    })
    da_data_subject$Months_From_Baseline <- da_data_subject$Days_From_Baseline/30.4375

    # Follow up data
    fu_data_subject <- fu_data_upload %>% dplyr::filter(sequence_no == subject)
    fu_Days_From_Baseline <- as.numeric(difftime(fu_data_subject$off_study_date,w2_re[1,"date_of_procedure"], units = "days"))
    fu_Months_From_Baseline <- round(fu_Days_From_Baseline/30.4375,2)


    if (min(fu_data_subject$date_of_progression,na.rm = T) != Inf) {
      SubjectPFS <- as.numeric(difftime(min(fu_data_subject$date_of_progression, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
      SubjectPFS_months <- round(SubjectPFS/30.4375,2)
      SubjectPFS_months_text <- paste("- PFS:",SubjectPFS_months,"Months")
    } else {
      SubjectPFS <- as.numeric(difftime(min(AEandDemoData_subject[,"last_visit_date"], na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
      SubjectPFS_months <- round(SubjectPFS/30.4375,2)
      SubjectPFS_months_text <- paste("- PFS:",SubjectPFS_months,"Months")
      #SubjectPFS_months <- NULL
      #SubjectPFS_months_text <- NULL
    }

    if (min(fu_data_subject$last_followup_date,na.rm = T) != Inf) {
      SubjectOS <- as.numeric(difftime(min(fu_data_subject$last_followup_date, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
      SubjectOS_months <- round(SubjectOS/30.4375,2)
      SubjectOS_months_text <- paste("- OS:",SubjectOS_months,"Months")
    } else {
      SubjectOS_months <- NULL
      SubjectOS_months_text <- NULL
    }
    shapes <- c(`Baseline (BL)` = 1, `Partial Response (PR)` = 2, `Progressive Disease (PD)` = 0, `Stable Disease (SD)` = 3)

    # Base plot
    p1 <- ggplot2::ggplot(w2_re, ggplot2::aes(x = Months_From_Baseline, y = Change_From_Baseline)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(ggplot2::aes(shape = response), size = 7) +
      my_theme +
      ggplot2::scale_shape(solid = FALSE) +
      ggplot2::scale_shape_manual(values=shapes) +
      ggplot2::labs(shape = "RECIST",
           x = "Months",
           y = "% Change from baseline (RECIST)") +
      ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", color = "blue", size = 1) +
      ggplot2::geom_hline(yintercept = 1.20, linetype = "dashed", color = "orange", size = 1) +
      ggplot2::theme(plot.title = ggplot2::element_text(size=18*k,face="bold"),
            legend.position="top",
            axis.text=ggplot2::element_text(size=14*k),
            axis.title=ggplot2::element_text(size=16*k,face="bold"),
            legend.text = ggplot2::element_text(size=12*k),
            legend.title = ggplot2::element_text(size=14*k),
            legend.box.margin=ggplot2::margin(0,0,8,0)) +
      ggplot2::scale_y_continuous(sec.axis = sec_axis(~., breaks = c(1.20,0.70), labels = c("PD","PR"))) +
      ggplot2::ggtitle(paste("RECIST data for ID:", as.character(subject),SubjectPFS_months_text,SubjectOS_months_text))

    # Add drug administration data
    p2_v2 <- p1 +
      ggplot2::geom_vline(xintercept = LastVisit,
                 size = 1, linetype = "dashed", color = "darkred") +
      ggplot2::geom_vline(xintercept = c(min(da_data_subject$Months_From_Baseline),max(da_data_subject$Months_From_Baseline)),
                 size = 1, linetype = "dashed", color = "skyblue") +
      ggplot2::geom_vline(xintercept = c(FirstPR,LastR),
                 size = 1, linetype = "dashed", color = "blue") +
      ggrepel::geom_text_repel(data = data.frame(x = c(min(da_data_subject$Months_From_Baseline),max(da_data_subject$Months_From_Baseline),FirstPR,LastR,LastVisit),
                                        label = c("Drug Start","Drug End",FirstPR,LastR,"Last Visit")),
                      x = c(min(da_data_subject$Months_From_Baseline),max(da_data_subject$Months_From_Baseline),FirstPR,LastR,LastVisit),
                      y = (layer_scales(p1)$y$range$range[2]*1.01),
                      ggplot2::aes(label = c("Drug Start","Drug End",FirstPR,LastR,"Last Visit")), size = 5, show.legend = FALSE, na.rm = T,
                      vjust = -1,
                      hjust = 0.5,
                      direction = "x",
                      min.segment.length = unit(1, 'lines'),
                      #min.segment.length = 5,
                      ylim = c(0,(layer_scales(p1)$y$range$range[2]*1.1))) +
      ggplot2::coord_cartesian(clip = 'off')

    # Add Followup data
    p3 <- p2_v2 +
      ggplot2::geom_segment(x = LastR,y = 1,xend = fu_Months_From_Baseline, yend = 1, color = "darkorange",
                   arrow = arrow(length = unit(0.03, "npc")), size = 1.5) +
      annotate(geom = "text", x = fu_Months_From_Baseline, y = 1, label = fu_data_subject$last_known_survival_status,
               color = ifelse(fu_data_subject$last_known_survival_status == "Dead","red","forestgreen"), hjust = 0.75, size = 6,vjust = 1.75)
    #color = ifelse(fu_data_subject$last_known_survival_status == "Dead","red","forestgreen"), hjust = -0.25, size = 5)
    #save(list = ls(), file = "shiny_env_recist.Rdata", envir = environment())
    p3

  })

  output$recist_plot <- shiny::renderPlot({

    req(input$recist_plot_patient)
    p <- recist_plot_react()
    p

  })

  output$download_recist_plot <- shiny::downloadHandler(
    filename = function() {
      #IDnum<-IDnum()
      paste0("RECIST_plot_of_ID_", as.character(input$recist_plot_patient), "_", Sys.Date(), ".png")

    },
    content = function(file) { ggplot2::ggsave(file, plot = recist_plot_react(),  width = 14,
                                      height = 8,
                                      units = c("in"),#, "cm", "mm", "px"),
                                      dpi = 300 )   }
  )

  output$re_dataind <- DT::renderDataTable({

    DT::datatable(recist_plot_data() ,
              rownames = FALSE, options = list(pageLength = 100))

  })

  output$RECISTtableinddownload <- shiny::downloadHandler(
    filename = function(){"RECISTtableindv.csv"},
    content = function(fname){
      write.csv(recist_plot_data(), fname, row.names = FALSE)
    }
  )

  output$renddownload_all_recist_plots <- shiny::renderUI({

    req(draw_REplots())
    downloadButton('download_all_recist_plots',"Save all plots")

  })

  draw_REplots <- shiny::eventReactive(input$runAllREplots, {

    plot_RE <- function(subjectID,AEandDemoData_subject,recist_data_subject,da_data_upload_subject,fu_data_upload_subject) {

      subject <- subjectID
      AEandDemoData <- AEandDemoData_subject
      recist_data <- recist_data_subject
      da_data_upload <- da_data_upload_subject
      fu_data_upload <- fu_data_upload_subject

      # Subset to patient of interest
      w1_re <- recist_data %>% dplyr::filter(is.na(date_of_procedure) == FALSE)
      w1_re <- w1_re %>% dplyr::filter(sequence_no == subject)
      w2_re <- w1_re

      # Calculate days from baseline
      w2_re$Days_From_Baseline <- sapply(w2_re[,"date_of_procedure"],function(x) {
        difftime(x,w2_re[1,"date_of_procedure"], units = "days")
      })
      w2_re$Months_From_Baseline <- w2_re$Days_From_Baseline/30.4375
      w2_re$Change_From_Baseline <- (w2_re$percent_change_from_baseline/100) + 1

      # Subset Columns of interest
      name1 <- c("date_of_procedure","percent_change_from_baseline","percent_change_from_smallest_sum","response",
                 "Days_From_Baseline","Months_From_Baseline","Change_From_Baseline")
      w2_re <- w2_re %>% dplyr::select(dplyr::any_of(c(name1))) %>% unique()
      my_theme = ggplot2::theme(axis.title = ggplot2::element_text(size = 20), axis.text = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15), strip.text = ggplot2::element_text(size = 15))

      k=1.1
      k1=2
      # Drug administration Data
      if("start_date_of_drug" %in% names( da_data_upload)) {
        da_data_subject <- da_data_upload %>% dplyr::filter(sequence_no == subject)
      }else{
        da_data_subject <- da_data_upload %>% dplyr::mutate(start_date_of_drug = start_date) %>% dplyr::filter(sequence_no == subject)
      }
      if("drug" %in% names( da_data_subject)) {
        da_data_subject <- da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }else{
        da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }
      FirstPR <- suppressWarnings(round(min(w2_re[which(w2_re$response == "Partial Response (PR)"),"Months_From_Baseline"]),2))
      FirstPR <- ifelse(is.infinite(FirstPR),NA,FirstPR)
      LastR <- round(max(w2_re[,"Months_From_Baseline"]),2)

      if ("last_visit_date" %in% colnames(AEandDemoData)) {
        AEandDemoData_subject <- AEandDemoData[which(AEandDemoData[,1] == subject),]
        LastVisit <- unique(difftime(AEandDemoData_subject[,"last_visit_date"],AEandDemoData_subject[,"on_treatment_date"], units = "days"))
        LastVisit <- round(LastVisit/30.4375,2)
      } else {
        LastVisit <- NULL
      }

      da_data_subject$Days_From_Baseline <- sapply(da_data_subject[,"start_date_of_drug"],function(x) {
        difftime(x,w2_re[1,"date_of_procedure"], units = "days")
      })
      da_data_subject$Months_From_Baseline <- da_data_subject$Days_From_Baseline/30.4375

      # Follow up data
      fu_data_subject <- fu_data_upload %>% dplyr::filter(sequence_no == subject)
      fu_Days_From_Baseline <- as.numeric(difftime(fu_data_subject$off_study_date,w2_re[1,"date_of_procedure"], units = "days"))
      fu_Months_From_Baseline <- round(fu_Days_From_Baseline/30.4375,2)


      if (min(fu_data_subject$date_of_progression,na.rm = T) != Inf) {
        SubjectPFS <- as.numeric(difftime(min(fu_data_subject$date_of_progression, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
        SubjectPFS_months <- round(SubjectPFS/30.4375,2)
        SubjectPFS_months_text <- paste("- PFS:",SubjectPFS_months,"Months")
      } else {
        SubjectPFS <- as.numeric(difftime(min(AEandDemoData_subject[,"last_visit_date"], na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
        SubjectPFS_months <- round(SubjectPFS/30.4375,2)
        SubjectPFS_months_text <- paste("- PFS:",SubjectPFS_months,"Months")
        #SubjectPFS_months <- NULL
        #SubjectPFS_months_text <- NULL
      }

      if (min(fu_data_subject$last_followup_date,na.rm = T) != Inf) {
        SubjectOS <- as.numeric(difftime(min(fu_data_subject$last_followup_date, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
        SubjectOS_months <- round(SubjectOS/30.4375,2)
        SubjectOS_months_text <- paste("- OS:",SubjectOS_months,"Months")
      } else {
        SubjectOS_months <- NULL
        SubjectOS_months_text <- NULL
      }
      shapes <- c(`Baseline (BL)` = 1, `Partial Response (PR)` = 2, `Progressive Disease (PD)` = 0, `Stable Disease (SD)` = 3)


      # Base plot
      p1 <- ggplot2::ggplot(w2_re, ggplot2::aes(x = Months_From_Baseline, y = Change_From_Baseline)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(ggplot2::aes(shape = response), size = 5) +
        #my_theme +
        ggplot2::scale_shape(solid = FALSE) +
        ggplot2::scale_shape_manual(values=shapes) +
        ggplot2::labs(shape = "RECIST",
                      x = "Months",
                      y = "% Change from baseline (RECIST)") +
        ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", color = "blue", linewidth = 1) +
        ggplot2::geom_hline(yintercept = 1.20, linetype = "dashed", color = "orange", linewidth = 1) +
        ggplot2::theme(plot.title = ggplot2::element_text(size=16*k,face="bold"),
                       legend.position="top",
                       axis.text=ggplot2::element_text(size=12*k),
                       axis.title=ggplot2::element_text(size=14*k,face="bold"),
                       legend.text = ggplot2::element_text(size=10*k),
                       legend.title = ggplot2::element_text(size=12*k),
                       legend.box.margin=ggplot2::margin(0,0,8,0)) +
        ggplot2::scale_y_continuous(sec.axis = sec_axis(~., breaks = c(1.20,0.70), labels = c("PD","PR"))) +
        ggplot2::ggtitle(paste0("RECIST data for ID: ", as.character(subject),SubjectPFS_months_text,SubjectOS_months_text))

      # Add drug administration data
      p2_v2 <- p1 +
        ggplot2::geom_vline(xintercept = LastVisit,
                            linewidth = 1, linetype = "dashed", color = "darkred") +
        ggplot2::geom_vline(xintercept = c(min(da_data_subject$Months_From_Baseline),max(da_data_subject$Months_From_Baseline)),
                            linewidth = 1, linetype = "dashed", color = "skyblue") +
        ggplot2::geom_vline(xintercept = c(FirstPR,LastR),
                            linewidth = 1, linetype = "dashed", color = "blue") +
        ggrepel::geom_text_repel(data = data.frame(x = c(min(da_data_subject$Months_From_Baseline),max(da_data_subject$Months_From_Baseline),FirstPR,LastR,LastVisit),
                                                   label = c("Drug Start","Drug End",FirstPR,LastR,"Last Visit")),
                                 x = c(min(da_data_subject$Months_From_Baseline),max(da_data_subject$Months_From_Baseline),FirstPR,LastR,LastVisit),
                                 y = (layer_scales(p1)$y$range$range[2]*1.01),
                                 ggplot2::aes(label = c("Drug Start","Drug End",FirstPR,LastR,"Last Visit")), size = 3, show.legend = FALSE, na.rm = T,
                                 vjust = -1,
                                 hjust = 0.5,
                                 direction = "x",
                                 min.segment.length = unit(0, 'lines'),
                                 #min.segment.length = 1,
                                 ylim = c(0,(layer_scales(p1)$y$range$range[2]*1.1))) +
        ggplot2::coord_cartesian(clip = 'off')

      # Add Followup data
      p3 <- p2_v2 +
        ggplot2::geom_segment(x = LastR,y = 1,xend = fu_Months_From_Baseline, yend = 1, color = "darkorange",
                              arrow = arrow(length = unit(0.03, "npc")), size = 1.5) +
        annotate(geom = "text", x = fu_Months_From_Baseline, y = 1, label = fu_data_subject$last_known_survival_status,
                 color = ifelse(fu_data_subject$last_known_survival_status == "Dead","red","forestgreen"), hjust = 0.75, size = 6,vjust = 1.75)
      return(p3)
    }

    AEandDemoData <- AEandDemoData()
    recist_data <- recist_data()
    da_data_upload <- da_data_upload()
    fu_data_upload <- fu_data_upload()

    plot_list <- list()
    for (subject in unique(AEandDemoData[,1])) {
      if("start_date_of_drug" %in% names(da_data_upload)) {
        da_data_subject <- da_data_upload %>% dplyr::filter(sequence_no == subject)
      }else{
        da_data_subject <- da_data_upload %>% dplyr::mutate(start_date_of_drug = start_date) %>% dplyr::filter(sequence_no == subject)
      }
      if("drug" %in% names( da_data_subject)) {
        da_data_subject <- da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }else{
        da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }

      w1 <- AEandDemoData %>% dplyr::filter(is.na(resolved_date ) == FALSE  & is.na(on_treatment_date ) == FALSE)
      AEandDemoData_subject <- w1 %>% dplyr::filter(sequence_no == subject)

      w1_re <- recist_data %>% dplyr::filter(is.na(date_of_procedure) == FALSE)
      recist_data_subject <- w1_re %>% dplyr::filter(sequence_no == subject)

      fu_data_subject <- fu_data_upload %>% dplyr::filter(sequence_no == subject)

      p <- plot_RE(subject,AEandDemoData_subject,recist_data_subject,da_data_subject,fu_data_subject)
      plot_list[[subject]] <- p
    }
    plot_list

  })

  # AE and boxplots of responses####
  draw_REplot <- function() {  draw_REplots()  }

  # Download AE plots pdf ###

  # hopefully for either or PC OR biostools
  # Download AE plots pdf ####
  output$download_all_recist_plots <- shiny::downloadHandler(
    filename = "RECIST_plots_report.pdf",
    content = function(file) {

      filetorender <-  "template_REplots.Rmd"

      res <- rmarkdown::render(
        input = filetorender,
        params = list(
          draw_REplot = draw_REplot
        )
      )

      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }


    }
  )




  # end: RECIST Data ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^####
  # start: AE RECIST Data vvvvvvvvvvvvvvvvvvvvvvvvvvvv####

  output$rendAErecist_plot_patient <- shiny::renderUI({
    Patient_select <- unique(AEandDemoData()$sequence_no)[1]
    if (isTruthy(input$recist_plot_patient)) {
      if (input$recist_plot_patient %in% unique(AEandDemoData()$sequence_no)) {
        Patient_select <- input$recist_plot_patient
      }
    }
    shiny::selectInput("AErecist_plot_patient","Patient number:",
                choices = unique(AEandDemoData()$sequence_no), selected = Patient_select)
  })

  observe({
    shiny::updateSelectizeInput(session,"single_var_input", selected = input$AErecist_plot_patient)
  })


  AErecist_plot_react <- shiny::reactive({

    req(input$AErecist_plot_patient)
    AEearly_Cut <- input$AEREplot_EarlyAECut
    TimeDisplay <- input$AEREplot_ShowTime
    AEearly_Cut_opt <- TRUE
    if (is.na(AEearly_Cut)) {
      AEearly_Cut_opt <- FALSE
    } else {}
    AEandDemoData <- AEandDemoData()

    recist_data <- recist_data()


    w1<-AEandDemoData() %>% dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE  & is.na(on_treatment_date) == FALSE)

    subject<-input$AErecist_plot_patient
    w1 <- w1 %>% dplyr::filter(sequence_no == subject)
    name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date' ,'grade')
    name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
    w2<-w1%>%dplyr::select(dplyr::any_of(c(name1,name2)))
    w2[,name2][is.na(w2[,name2])] <- 'Not  Applicable'
    w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x=='Not  Applicable'))
    w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
    w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                   t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                   t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days')),
                   code=cdus_ctcae_toxicity_type_code)
    w2$index<-as.numeric(factor(w2$code))
    w2$grade<-as.factor(w2$grade)



    if("start_date_of_drug" %in% names( da_data_upload())) {
      da_data_subject <- da_data_upload() %>% dplyr::filter(sequence_no == subject)
    }else{
      da_data_subject <- da_data_upload() %>% dplyr::mutate(start_date_of_drug = start_date) %>% dplyr::filter(sequence_no == subject)
    }
    if("drug" %in% names( da_data_subject)) {
      da_data_subject <-da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)
    }else{
      da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
        dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
        dplyr::filter(!drug %in% c("Not  Applicable")) %>%
        dplyr::filter(is.na(drug) == FALSE)
    }
    w2.2early <- w2 %>% dplyr::distinct(on_treatment_date, .keep_all = TRUE)
    w3 <- merge(  da_data_subject, w2.2early %>% dplyr::select(on_treatment_date)) %>% dplyr::mutate(dadaynumber = as.numeric(difftime(start_date_of_drug ,on_treatment_date,units='days')) )
    #UL <- max(w2$t2)+10
    #lastAEdaynumber <- max(w2$t2)

    AE.data= w2
    RE.data = recist_data
    subjectID = input$AErecist_plot_patient
    early.time=AEearly_Cut
    k=1.1
    k1=2
    early.AE.status=AEearly_Cut_opt
    #early_AERE_plot_manuscript.fun<-function(AE.data= w2,RE.data = recist_data,subjectID = input$AErecist_plot_patient,
    #                                         early.time=AEearly_Cut,k=1.1,k1=2,early.AE.status=AEearly_Cut_opt) {

    ## AE Data

    if (is.na(early.time)) {
      early.time <- 30
    }
    vline.fun<-function(early.AE.status.tmp,early.time.tmp) if(early.AE.status.tmp)    ggplot2::geom_vline(xintercept = early.time.tmp,linetype="dotted")
    geom_label.no_early_AE.fun<-function(data,early.AE.status.tmp) if(!early.AE.status.tmp) geom_label(data = data,ggplot2::aes(x=t2-(t12/2),y=index,label = t12), inherit.aes = F,hjust=0.5,size = 7)
    geom_label.early_AE.fun<-function(data,early.AE.status.tmp) if(early.AE.status.tmp) geom_label(data = data,ggplot2::aes(x=t2-(t12/2),y=index,label = t12.early), inherit.aes = F,hjust=0.5,size = 7)
    g.cols=c('lightgreen','yellow','orange','red',"purple",'blue','green','gray25','gray75' )
    w1<-AE.data %>% dplyr::filter(is.na(resolved_date ) == FALSE  & is.na(on_treatment_date ) == FALSE)
    subject<-unique(w1$sequence_no)
    name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date',#'AE.time',
             'grade')
    name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
    w2<-w1%>%dplyr::select(dplyr::any_of(c(name1,name2)))
    w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x%in%c('Not  Applicable',NA)))
    w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
    w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                   t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                   t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days'))+1,
                   code=cdus_ctcae_toxicity_type_code)
    w2$index<-as.numeric(factor(w2$code))
    w2$t1 <- round(w2$t1)
    w2$t2 <- round(w2$t2)
    w2$t12 <- round(w2$t12)
    if(early.AE.status){
      w2=w2%>%dplyr::mutate(AE.early.indicator=(t1<=early.time)*(t2>=early.time))%>%
        dplyr::mutate(t12.early=ifelse(AE.early.indicator%in%1,paste(early.time-t1+ifelse(early.time==0,1,0),'+',t2-early.time+1,sep=''),t12))
    }

    ## RE Data

    w1_re <- RE.data %>% dplyr::filter(is.na(date_of_procedure) == FALSE)
    w1_re <- w1_re %>% dplyr::filter(sequence_no == subjectID)
    name1_re <- c("date_of_procedure","percent_change_from_baseline","percent_change_from_smallest_sum","response")
    w2_re <- w1_re %>% dplyr::select(dplyr::any_of(c(name1_re))) %>% unique()
    w2_re$Days_From_Baseline <- sapply(w2_re[,"date_of_procedure"],function(x) {
      difftime(x,w2_re[1,"date_of_procedure"], units = "days")
    })
    w2_re$Months_From_Baseline <- w2_re$Days_From_Baseline/30.4375
    w2_re$Change_From_Baseline <- (w2_re$percent_change_from_baseline/100) + 1

    FirstPR <- round(min(w2_re[which(w2_re$response == "Partial Response (PR)"),"Days_From_Baseline"]),2)
    FirstPR <- ifelse(is.infinite(FirstPR),NA,FirstPR)
    LastR <- round(max(w2_re[,"Days_From_Baseline"]),2)

    fu_data_subject <- fu_data_upload() %>% dplyr::filter(sequence_no == subjectID)
    fu_from <- max(c(w2_re$date_of_procedure,w2$resolved_date), na.rm = T)
    fu_Days_From_Baseline <- as.numeric(difftime(fu_data_subject$off_study_date,fu_from, units = "days"))
    fu_y <- ifelse(max(w2_re$date_of_procedure, na.rm = T)>=max(w2$resolved_date, na.rm = T),
                   (w2_re[which(w2_re$date_of_procedure == max(w2_re$date_of_procedure, na.rm = T)),"Change_From_Baseline"])*max(w2$index),
                   w2[which(w2$resolved_date == max(w2$resolved_date, na.rm = T)),"index"])

    if (fu_Days_From_Baseline <= 0 | is.na(fu_Days_From_Baseline)) {
      fu_Days_From_Baseline <- max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T)+1
    }

    if ("last_visit_date" %in% colnames(AEandDemoData)) {
      AEandDemoData_subject <- AEandDemoData[which(AEandDemoData[,1] == subjectID),]
      LastVisit <- unique(difftime(AEandDemoData_subject[,"last_visit_date"],AEandDemoData_subject[,"on_treatment_date"], units = "days"))
    } else {
      LastVisit <- NULL
    }


    if (min(fu_data_subject$date_of_progression,na.rm = T) != Inf) {
      SubjectPFS <- as.numeric(difftime(min(fu_data_subject$date_of_progression, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
      SubjectPFS_months <- round(SubjectPFS/30.4375,2)
      SubjectPFS_months_text <- paste("- PFS:",SubjectPFS_months,"Months")
    } else {
      SubjectPFS <- as.numeric(difftime(min(AEandDemoData_subject[,"last_visit_date"], na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
      SubjectPFS_months <- round(SubjectPFS/30.4375,2)
      SubjectPFS_months_text <- paste("- PFS:",SubjectPFS_months,"Months")
      #SubjectPFS_months <- NULL
      #SubjectPFS_months_text <- NULL
    }

    if (min(fu_data_subject$last_followup_date,na.rm = T) != Inf) {
      SubjectOS <- as.numeric(difftime(min(fu_data_subject$last_followup_date, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
      SubjectOS_months <- round(SubjectOS/30.4375,2)
      SubjectOS_months_text <- paste("- OS:",SubjectOS_months,"Months")
    } else {
      SubjectOS_months <- NULL
      SubjectOS_months_text <- NULL
    }

    shapes <- c(`Baseline (BL)` = 1, `Partial Response (PR)` = 2, `Progressive Disease (PD)` = 0, `Stable Disease (SD)` = 3)

    if (max(w2_re$Change_From_Baseline, na.rm = T) >= 1.4) {
      altBreaks <- c(0,0.2,0.4,0.6,0.7,0.8,1.0,1.2,1.4)
      altLabs <- c("0","0.2","0.4","0.6","0.7 - PR","0.8","1.0","1.2 - PD","1.4")
    } else {
      altBreaks <- c(0,0.2,0.4,0.6,0.7,0.8,1.0,1.2)
      altLabs <- c("0","0.2","0.4","0.6","0.7 - PR","0.8","1.0","1.2 - PD")
    }

    #save(list = ls(), file = "AE_RE_Plot_env.RData", envir = environment())

    plot1 <- ggplot2::ggplot() +
      ggplot2::geom_point(data = w2, ggplot2::aes(x=t2, y=index,color=grade,shape=treatment_related),size=2*k1)+
      ggplot2::geom_segment(data = w2,ggplot2::aes(x = t1, xend = t2, y=index,yend = index,color=grade), linewidth = 1*k1, lineend = "butt") +
      ggplot2::scale_color_manual(values=g.cols, drop=FALSE) +
      ggnewscale::new_scale_color() +
      ggnewscale::new_scale("shape") +
      ggplot2::scale_shape(solid = FALSE) +
      ggplot2::geom_vline(data = w3, ggplot2::aes(xintercept = dadaynumber, color = drug)) +
      ggplot2::geom_line(data = w2_re, ggplot2::aes(x = Days_From_Baseline, y = (Change_From_Baseline*(max(w2$index)))), size = 1) +
      ggplot2::geom_point(data = w2_re,ggplot2::aes(x = Days_From_Baseline, y = (Change_From_Baseline*(max(w2$index))), shape = response), size = 4) +
      ggplot2::scale_shape_manual(values=shapes)
      if (as.numeric(tools::file_path_sans_ext(packageVersion("ggplot2"))) >= 3.5) {
        plot1 <- plot1 +
          ggplot2::scale_y_continuous(breaks=w2$index, labels=w2$code, sec.axis = ggplot2::sec_axis(transform = ~./max(w2$index),
                                                                                                    name = "% Change from baseline (RECIST)",
                                                                                                    breaks = altBreaks,
                                                                                                    labels = altLabs))
      } else {
        plot1 <- plot1 +
          ggplot2::scale_y_continuous(breaks=w2$index, labels=w2$code, sec.axis = ggplot2::sec_axis(trans = ~./max(w2$index),
                                                                                                    name = "% Change from baseline (RECIST)",
                                                                                                    breaks = altBreaks,
                                                                                                    labels = altLabs))
      }
    plot1 <- plot1 +
      ggplot2::xlab('Days') + ggplot2::ylab('') +
      ggplot2::geom_hline(yintercept = (0.70*(max(w2$index))), linetype= "dashed", color = "blue", size = 1) +
      ggplot2::geom_hline(yintercept = (1.20*(max(w2$index))), linetype= "dashed", color = "orange", size = 1) +
      vline.fun(early.AE.status.tmp=early.AE.status,early.time.tmp=early.time)+
      ggplot2::theme(plot.title = ggplot2::element_text(size=18*k,face="bold"),
            legend.position="top",
            axis.text=ggplot2::element_text(size=14*k),
            axis.title=ggplot2::element_text(size=18*k,face="bold"),
            legend.text = ggplot2::element_text(size=16*k),
            legend.title = ggplot2::element_text(size=18*k),
            legend.box.margin=ggplot2::margin(0,0,8,0),
            legend.direction = "vertical", legend.box = "horizontal")+
      ggplot2::geom_vline(xintercept = LastVisit,
                 size = 1, linetype = "dashed", color = "darkred") +
      ggplot2::geom_vline(xintercept = c(FirstPR,LastR),
                 size = 1, linetype = "dashed", color = "blue") +
      ggplot2::geom_segment(ggplot2::aes(x = max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T),
                       y = fu_y,
                       xend = max(c((max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T) + fu_Days_From_Baseline),LastVisit), na.rm = T),
                       yend = fu_y),
                   color = "blue",
                   arrow = arrow(length = unit(0.03, "npc")), size = 1.5) +
      annotate(geom = "text",
               x = max(c((max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T) + fu_Days_From_Baseline),LastVisit), na.rm = T),
               y = fu_y,
               label = fu_data_subject$last_known_survival_status,
               color = ifelse(fu_data_subject$last_known_survival_status == "Dead","red","forestgreen"), hjust = 0.75, size = 6,vjust = 1.75)

    plot2 <- plot1 +
      #  ggplot2::geom_text(ggplot2::aes(x = LastVisit, y = (layer_scales(plot1)$y$range$range[2]*1.01), label = "Last Visit"), color = "red") +
      ggrepel::geom_text_repel(data = data.frame(x = LastVisit,
                                        label = "Last Visit"),
                      x = LastVisit,
                      y = (layer_scales(plot1)$y$range$range[2]*1.02),
                      ggplot2::aes(label = "Last Visit"), size = 5, show.legend = FALSE, na.rm = T,
                      vjust = -1,
                      hjust = 0.5,
                      direction = "x",
                      min.segment.length = unit(0, 'lines'),
                      #min.segment.length = 1,
                      ylim = c(0,(layer_scales(plot1)$y$range$range[2]*1.1))) +
      ggplot2::coord_cartesian(clip = 'off') +
      ggplot2::scale_x_continuous(limits=c(0, max(c((max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T) + fu_Days_From_Baseline),LastVisit), na.rm = T))) +
      ggplot2::ggtitle(paste("Adverse events + RECIST for ID:", as.character(subjectID),SubjectPFS_months_text,SubjectOS_months_text))

    if (TimeDisplay == TRUE) {
      plot2 <- plot2 +
        geom_label.no_early_AE.fun(data = w2,early.AE.status.tmp=early.AE.status)+  ## Display full numbers
        geom_label.early_AE.fun(data = w2,early.AE.status.tmp=early.AE.status)      ## Display Cut numbers
    }

    #save(list = ls(), file = "AE_resist_env.RData",envir = environment())
    #return(plot1)

    plot2
    #}

    #early_AERE_plot_manuscript.fun()


  })

  output$AErecist_plot <- shiny::renderPlot({

    pl <- AErecist_plot_react()
    pl

  })

  output$download_AErecist_plot <- shiny::downloadHandler(
    filename = function() {
      #IDnum<-IDnum()
      paste0("AE_RECIST_plot_of_ID_", as.character(input$AErecist_plot_patient), "_", Sys.Date(), ".png")

    },
    content = function(file) { ggplot2::ggsave(file, plot = AErecist_plot_react(),  width = 14,
                                      height = 8,
                                      units = c("in"),#, "cm", "mm", "px"),
                                      dpi = 300 )   }
  )

  AEcounttableindRE <- shiny::reactive({


    req(input$AErecist_plot_patient)
    subject<-input$AErecist_plot_patient
    #adverse_events0<- AE_data() %>% dplyr::filter(sequence_no == subject)
    adverse_events0<-AEandDemoData() %>% dplyr::filter(sequence_no == subject)  %>%
      dplyr::filter(is.na(onset_date_of_ae) == FALSE  & is.na(resolved_date) == FALSE  & is.na(on_treatment_date) == FALSE)

    if (!"cdus_toxicity_type_code" %in% names(adverse_events0)){
      adverse_events0$cdus_toxicity_type_code<- adverse_events0$toxicity_category}

    if (!"cdus_ctcae_toxicity_type_code" %in% names(adverse_events0)){
      adverse_events0$cdus_ctcae_toxicity_type_code<- adverse_events0$cdus_toxicity_type_code}

    if (!class(adverse_events0$grade) == "numeric"){
      adverse_events0$grade<- as.numeric(adverse_events0$grade)
    }

    # Toxicity table ####
    # This is a AE frequency not the max AE----

    adverse_events2 <- adverse_events0 %>% dplyr::filter(is.na(grade)==FALSE) %>%
      dplyr::group_by(sequence_no, cdus_ctcae_toxicity_type_code) %>%
      #dplyr::filter(grade == max(grade, na.rm = TRUE)) %>%
      dplyr::ungroup() #%>%
    #dplyr::distinct(sequence_no, cdus_ctcae_toxicity_type_code, grade, .keep_all = TRUE)
    #dplyr::distinct(sequence_no, cdus_ctcae_toxicity_type_code, .keep_all = TRUE)

    summary_ae_drug =adverse_events2%>%
      dplyr::mutate(grade = recode_factor(grade, `1` = "Grade 1", `2` = "Grade 2", `3` = "Grade 3",`4` = "Grade 4",`5` = "Grade 5")) %>%
      dplyr::count( `Adverse event` = cdus_ctcae_toxicity_type_code, grade) %>%
      tidyr::pivot_wider(names_from = grade, values_from = n, names_sort = TRUE)  %>%
      dplyr::filter(is.na(`Adverse event`)==FALSE)

    summary_ae_drug # ->summary_ae_drugMCC18494 #->summary_ae_drugMCC18494
    summary_ae_drug[is.na(summary_ae_drug)] <- 0

    return(summary_ae_drug)
  })

  output$AEcounttableindRE <- DT::renderDataTable({
    DT::datatable(AEcounttableindRE() ,
              rownames = FALSE, options = list(pageLength = 100))})

  AErecist_plot_data <- shiny::reactive({

    req(input$AErecist_plot_patient)
    subject <- input$AErecist_plot_patient
    recist_data <- recist_data()

    # Subset to patient of interest
    w1_re <- recist_data %>% dplyr::filter(is.na(date_of_procedure) == FALSE)
    w1_re <- w1_re %>% dplyr::filter(sequence_no == subject)
    w2_re <- w1_re

    # Calculate days from baseline
    w2_re$Days_From_Baseline <- sapply(w2_re[,"date_of_procedure"],function(x) {
      difftime(x,w2_re[1,"date_of_procedure"], units = "days")
    })
    w2_re$Months_From_Baseline <- w2_re$Days_From_Baseline/30.4375
    w2_re$Change_From_Baseline <- (w2_re$percent_change_from_baseline/100) + 1
    w2_re

  })

  output$AEre_dataind <- DT::renderDataTable({

    DT::datatable(AErecist_plot_data() ,
              rownames = FALSE, options = list(pageLength = 100))

  })

  output$AEcounttableinddownloadRE <- shiny::downloadHandler(
    filename = function(){"AEtableindv.csv"},
    content = function(fname){
      write.csv(AEcounttableindRE(), fname, row.names = FALSE)
    }
  )

  output$AERECISTtableinddownload <- shiny::downloadHandler(
    filename = function(){"AE_RECISTtableindv.csv"},
    content = function(fname){
      write.csv(AErecist_plot_data(), fname, row.names = FALSE)
    }
  )

  output$renddownload_all_AErecist_plots <- shiny::renderUI({

    req(draw_AEREplots())
    downloadButton('download_all_AErecist_plots',"Save all plots")

  })

  draw_AEREplots <- shiny::eventReactive(input$runAllAEREplots, {

    plot_AERE <- function(subjectID,AEandDemoData_subject,recist_data_subject,da_data_upload_subject,fu_data_upload_subject,AEearly_Cut,TimeDisplay,AEearly_Cut_opt) {

      subject <- subjectID
      AE.data = AEandDemoData_subject
      RE.data = recist_data_subject
      early.time=AEearly_Cut
      da_data_subject <- da_data_upload_subject
      fu_data_subject <- fu_data_upload_subject
      k=1.1
      k1=2
      early.AE.status=AEearly_Cut_opt
      if (is.na(early.time)) {
        early.time <- 30
      }
      vline.fun<-function(early.AE.status.tmp,early.time.tmp) if(early.AE.status.tmp)    ggplot2::geom_vline(xintercept = early.time.tmp,linetype="dotted")
      geom_label.no_early_AE.fun<-function(data,early.AE.status.tmp) if(!early.AE.status.tmp) ggplot2::geom_label(data = data,ggplot2::aes(x=t2-(t12/2),y=index,label = t12), inherit.aes = F,hjust=0.5,size = 7)
      geom_label.early_AE.fun<-function(data,early.AE.status.tmp) if(early.AE.status.tmp) ggplot2::geom_label(data = data,ggplot2::aes(x=t2-(t12/2),y=index,label = t12.early), inherit.aes = F,hjust=0.5,size = 7)
      g.cols=c('lightgreen','yellow','orange','red',"purple",'blue','green','gray25','gray75' )

      # AE
      w1 <- AE.data
      name1<-c('onset_date_of_ae','cdus_ctcae_toxicity_type_code', 'resolved_date','on_treatment_date',#'AE.time',
               'grade')
      name2<-c('attribution_possible','attribution_probable', 'attribution_definite')
      w2<-w1%>%dplyr::select(dplyr::any_of(c(name1,name2)))
      w2$treatment_related<-!apply(as.matrix(w2[,name2]),1,function(x) all(x%in%c('Not  Applicable',NA)))
      w2$treatment_related<-factor(w2$treatment_related,level=c(F,T),label=c('No','Yes'))
      w2=w2%>%dplyr::mutate(t1=as.numeric(difftime(onset_date_of_ae,on_treatment_date,units='days')),
                     t2=as.numeric(difftime(resolved_date,on_treatment_date,units='days')),
                     t12=as.numeric(difftime(resolved_date,onset_date_of_ae,units='days'))+1,
                     code=cdus_ctcae_toxicity_type_code)
      w2$index<-as.numeric(factor(w2$code))
      w2$t1 <- round(w2$t1)
      w2$t2 <- round(w2$t2)
      w2$t12 <- round(w2$t12)
      if(early.AE.status){
        w2=w2%>%dplyr::mutate(AE.early.indicator=(t1<=early.time)*(t2>=early.time))%>%
          dplyr::mutate(t12.early=ifelse(AE.early.indicator%in%1,paste(early.time-t1+ifelse(early.time==0,1,0),'+',t2-early.time+1,sep=''),t12))
      }
      w2.2early <- w2 %>% dplyr::distinct(on_treatment_date, .keep_all = TRUE)
      w3 <- merge(  da_data_subject, w2.2early %>% dplyr::select(on_treatment_date)) %>%
        dplyr::mutate(dadaynumber = as.numeric(difftime(start_date_of_drug ,on_treatment_date,units='days')) )

      # RECIST
      w1_re <- RE.data
      name1_re <- c("date_of_procedure","percent_change_from_baseline","percent_change_from_smallest_sum","response")
      w2_re <- w1_re %>% dplyr::select(dplyr::any_of(c(name1_re))) %>% unique()
      w2_re$Days_From_Baseline <- sapply(w2_re[,"date_of_procedure"],function(x) {
        difftime(x,w2_re[1,"date_of_procedure"], units = "days")
      })
      w2_re$Months_From_Baseline <- w2_re$Days_From_Baseline/30.4375
      w2_re$Change_From_Baseline <- (w2_re$percent_change_from_baseline/100) + 1
      w2_re$Change_From_Baseline[which(is.na(w2_re$Change_From_Baseline))] <- 1

      FirstPR <- suppressWarnings(round(min(w2_re[which(w2_re$response == "Partial Response (PR)"),"Days_From_Baseline"]),2))
      FirstPR <- ifelse(is.infinite(FirstPR),NA,FirstPR)
      LastR <- round(max(w2_re[,"Days_From_Baseline"]),2)

      # Follow Up
      fu_from <- max(c(w2_re$date_of_procedure,w2$resolved_date), na.rm = T)
      fu_Days_From_Baseline <- as.numeric(difftime(fu_data_subject$off_study_date,fu_from, units = "days"))
      fu_y <- ifelse(max(w2_re$date_of_procedure, na.rm = T)>=max(w2$resolved_date, na.rm = T),
                     (w2_re[which(w2_re$date_of_procedure == max(w2_re$date_of_procedure, na.rm = T)),"Change_From_Baseline"])*max(w2$index),
                     w2[which(w2$resolved_date == max(w2$resolved_date, na.rm = T)),"index"])
      if (fu_Days_From_Baseline <= 0 | is.na(fu_Days_From_Baseline)) {
        fu_Days_From_Baseline <- max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T)+1
      }

      if ("last_visit_date" %in% colnames(AEandDemoData_subject)) {
        LastVisit <- unique(difftime(AEandDemoData_subject[,"last_visit_date"],AEandDemoData_subject[,"on_treatment_date"], units = "days"))
      } else {
        LastVisit <- NULL
      }

      if (suppressWarnings(min(fu_data_subject$date_of_progression,na.rm = T)) != Inf) {
        SubjectPFS <- as.numeric(difftime(min(fu_data_subject$date_of_progression, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
        SubjectPFS_months <- round(SubjectPFS/30.4375,2)
        SubjectPFS_months_text <- paste("PFS:",SubjectPFS_months,"Months")
      } else {
        SubjectPFS <- as.numeric(difftime(min(AEandDemoData_subject[,"last_visit_date"], na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
        SubjectPFS_months <- round(SubjectPFS/30.4375,2)
        SubjectPFS_months_text <- paste("PFS:",SubjectPFS_months,"Months")
        #SubjectPFS_months <- NULL
        #SubjectPFS_months_text <- NULL
      }

      if (suppressWarnings(min(fu_data_subject$last_followup_date,na.rm = T)) != Inf) {
        SubjectOS <- as.numeric(difftime(min(fu_data_subject$last_followup_date, na.rm = T),min(da_data_subject$start_date_of_drug, na.rm = T), units = "days"))
        SubjectOS_months <- round(SubjectOS/30.4375,2)
        SubjectOS_months_text <- paste("-- OS:",SubjectOS_months,"Months")
      } else {
        SubjectOS_months <- NULL
        SubjectOS_months_text <- NULL
      }
      shapes <- c(`Baseline (BL)` = 1, `Partial Response (PR)` = 2, `Progressive Disease (PD)` = 0, `Stable Disease (SD)` = 3)

      w2$codewrap <- str_wrap(w2$code, width = 50)


      if (max(w2_re$Change_From_Baseline, na.rm = T) >= 1.4) {
        altBreaks <- c(0,0.2,0.4,0.6,0.7,0.8,1.0,1.2,1.4)
        altLabs <- c("0","0.2","0.4","0.6","0.7 - PR","0.8","1.0","1.2 - PD","1.4")
      } else {
        altBreaks <- c(0,0.2,0.4,0.6,0.7,0.8,1.0,1.2)
        altLabs <- c("0","0.2","0.4","0.6","0.7 - PR","0.8","1.0","1.2 - PD")
      }

      w2$grade <- as.character(factor(w2$grade))
      plot1 <- ggplot2::ggplot() +
        ggplot2::geom_point(data = w2, ggplot2::aes(x=t2, y=index,color=grade,shape=treatment_related),size=2*k1)+
        ggplot2::geom_segment(data = w2,ggplot2::aes(x = t1, xend = t2, y=index,yend = index,color=grade), linewidth = 1*k1, lineend = "butt") +
        ggplot2::scale_color_manual(values=g.cols, drop=FALSE) +
        ggnewscale::new_scale_color() +
        ggnewscale::new_scale("shape") +
        ggplot2::scale_shape(solid = FALSE) +
        ggplot2::geom_vline(data = w3, ggplot2::aes(xintercept = dadaynumber, color = drug)) +
        ggplot2::geom_line(data = w2_re, ggplot2::aes(x = Days_From_Baseline, y = (Change_From_Baseline*max(w2$index))), linewidth = 1) +
        ggplot2::geom_point(data = w2_re,ggplot2::aes(x = Days_From_Baseline, y = (Change_From_Baseline*max(w2$index)), shape = response), size = 4) +
        ggplot2::scale_shape_manual(values=shapes)
        if (as.numeric(tools::file_path_sans_ext(packageVersion("ggplot2"))) >= 3.5) {
          plot1 <- plot1 +
            ggplot2::scale_y_continuous(breaks=w2$index, labels=w2$code, sec.axis = ggplot2::sec_axis(transform = ~./max(w2$index),
                                                                                                      name = "% Change from baseline (RECIST)",
                                                                                                      breaks = altBreaks,
                                                                                                      labels = altLabs))
        } else {
          plot1 <- plot1 +
            ggplot2::scale_y_continuous(breaks=w2$index, labels=w2$code, sec.axis = ggplot2::sec_axis(trans = ~./max(w2$index),
                                                                                                      name = "% Change from baseline (RECIST)",
                                                                                                      breaks = altBreaks,
                                                                                                      labels = altLabs))
        }
      plot1 <- plot1 +
        ggplot2::xlab('Days') + ggplot2::ylab('') +
        ggplot2::geom_hline(yintercept = (0.70*max(w2$index)), linetype= "dashed", color = "blue", linewidth = 1) +
        ggplot2::geom_hline(yintercept = (1.20*max(w2$index)), linetype= "dashed", color = "orange", linewidth = 1) +
        vline.fun(early.AE.status.tmp=early.AE.status,early.time.tmp=early.time)+
        ggplot2::theme(plot.title = ggplot2::element_text(size=14*k,face="bold"),
              legend.position="top",
              axis.text=ggplot2::element_text(size=10*k),
              axis.title=ggplot2::element_text(size=12*k,face="bold"),
              legend.text = ggplot2::element_text(size=10*k),
              legend.title = ggplot2::element_text(size=12*k),
              legend.box.margin=ggplot2::margin(0,0,8,0),
              legend.direction = "vertical", legend.box = "horizontal")+
        ggplot2::geom_vline(xintercept = LastVisit,
                            linewidth = 1, linetype = "dashed", color = "darkred") +
        ggplot2::geom_vline(xintercept = c(FirstPR,LastR),
                            linewidth = 1, linetype = "dashed", color = "blue") +
        ggplot2::geom_segment(ggplot2::aes(x = max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T),
                         y = fu_y,
                         xend = max(c((max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T) + fu_Days_From_Baseline),LastVisit), na.rm = T),
                         yend = fu_y),
                     color = "blue",
                     arrow = arrow(length = unit(0.03, "npc")), linewidth = 1.5) +
        ggplot2::annotate(geom = "text",
                 x = max(c((max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T) + fu_Days_From_Baseline),LastVisit), na.rm = T),
                 y = fu_y,
                 label = fu_data_subject$last_known_survival_status,
                 color = ifelse(fu_data_subject$last_known_survival_status == "Dead","red","forestgreen"), hjust = 0.75, size = 6,vjust = 1.75)

      plot2 <- plot1 +
        ggrepel::geom_text_repel(data = data.frame(x = LastVisit,
                                          label = "Last Visit"),
                        x = LastVisit,
                        y = (layer_scales(plot1)$y$range$range[2]*1.02),
                        ggplot2::aes(label = "Last Visit"), size = 5, show.legend = FALSE, na.rm = T,
                        vjust = -1,
                        hjust = 0.5,
                        direction = "x",
                        min.segment.length = unit(0, 'lines'),
                        ylim = c(0,(layer_scales(plot1)$y$range$range[2]*1.1))) +
        ggplot2::coord_cartesian(clip = 'off') +
        ggplot2::scale_x_continuous(limits=c(0, max(c((max(c(w2_re$Days_From_Baseline,w2$t2), na.rm = T) + fu_Days_From_Baseline),LastVisit), na.rm = T))) +
        ggplot2::ggtitle(paste("Adverse events + RECIST for ID:", as.character(subjectID),"\n",SubjectPFS_months_text,SubjectOS_months_text))

      if (TimeDisplay == TRUE) {
        plot2 <- plot2 +
          geom_label.no_early_AE.fun(data = w2,early.AE.status.tmp=early.AE.status)+  ## Display full numbers
          geom_label.early_AE.fun(data = w2,early.AE.status.tmp=early.AE.status)      ## Display Cut numbers
      }
      return(plot2)

    }



    #plot_list <- list()
    #i <- 1
    AEandDemoData <- AEandDemoData()
    recist_data <- recist_data()
    da_data_upload <- da_data_upload()
    fu_data_upload <- fu_data_upload()
    AEearly_Cut <- input$AEREplot_EarlyAECut
    TimeDisplay <- input$AEREplot_ShowTime
    AEearly_Cut_opt <- TRUE
    if (is.na(AEearly_Cut)) {
      AEearly_Cut_opt <- FALSE
    }


    plot_list <- list()
    for (subject in unique(AEandDemoData[,1])) {
      if("start_date_of_drug" %in% names(da_data_upload)) {
        da_data_subject <- da_data_upload %>% dplyr::filter(sequence_no == subject)
      }else{
        da_data_subject <- da_data_upload %>% dplyr::mutate(start_date_of_drug = start_date) %>% dplyr::filter(sequence_no == subject)
      }
      if("drug" %in% names( da_data_subject)) {
        da_data_subject <- da_data_subject %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }else{
        da_data_subject <- da_data_subject %>% dplyr::mutate(drug = level) %>% dplyr::filter(sequence_no == subject) %>%
          dplyr::select(sequence_no, start_date_of_drug, cycle, drug) %>%
          dplyr::filter(!drug %in% c("Not  Applicable")) %>%
          dplyr::filter(is.na(drug) == FALSE)
      }
      w1 <- AEandDemoData %>% dplyr::filter(is.na(resolved_date ) == FALSE  & is.na(on_treatment_date ) == FALSE)
      AEandDemoData_subject <- w1 %>% dplyr::filter(sequence_no == subject)
      w1_re <- recist_data %>% dplyr::filter(is.na(date_of_procedure) == FALSE)
      recist_data_subject <- w1_re %>% dplyr::filter(sequence_no == subject)
      fu_data_subject <- fu_data_upload %>% dplyr::filter(sequence_no == subject)
      p <- plot_AERE(subject,AEandDemoData_subject,recist_data_subject,da_data_subject,fu_data_subject,AEearly_Cut,TimeDisplay,AEearly_Cut_opt)
      plot_list[[subject]] <- p
    }
    plot_list

  

  })

  observe({

    AEandDemoData <- AEandDemoData()
    recist_data <- recist_data()
    da_data_upload <- da_data_upload()
    fu_data_upload <- fu_data_upload()
    AEearly_Cut <- input$AEREplot_EarlyAECut
    TimeDisplay <- input$AEREplot_ShowTime
    AEearly_Cut_opt <- TRUE
    if (is.na(AEearly_Cut)) {
      AEearly_Cut_opt <- FALSE
    }

    subject<-input$AErecist_plot_patient


    #save(list = ls(), file = "AE_RE_Plot_env.RData", envir = environment())

  })


  # AE and boxplots of responses####
  draw_AEREplot <- function() {  draw_AEREplots()  }

  # Download AE plots pdf ###

  # hopefully for either or PC OR biostools
  # Download AE plots pdf ####
  output$download_all_AErecist_plots <- shiny::downloadHandler(
    filename = "AE_RECIST_plots_report.pdf",
    content = function(file) {

      filetorender <-  "template_AEREplots.Rmd"

      res <- rmarkdown::render(
        input = filetorender,
        params = list(
          draw_AEREplot = draw_AEREplot
        )
      )

      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }


    }
  )


  # end: AE RECIST Data ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^####

  # start: survival::coxph AE measures tab VVVVVVVVVVVVVVVVVVVVVVV####
  # KM plots and survival::coxph models for AE measures ###

  kmandcoxphinfofromtoxdata <- shiny::eventReactive(input$gogocoxmodels,{

    kmandboxplotsfunction <- function(df){
      png.status<-F
      AE.km.and.boxplot.list<-coef.list<-list()
      k<-0
      # 2626 HERE ADD VARS HERE ####
      plotthesevars <-   c("all.grade.duration","all.grade.fre",  "all.grade.occurrence","all.grade.trt.duration",
                           "all.grade.trt.fre","all.grade.trt.occurrence",  "LG.duration","LG.fre",
                           "LG.occurrence","LG.trt.duration",  "LG.trt.fre","LG.trt.occurrence",
                           "HG.duration","HG.fre",  "HG.occurrence","HG.trt.duration",  "HG.trt.fre","HG.trt.occurrence",
                           
                           "LG.ntr.fre","LG.ntr.occurrence","LG.ntr.duration",
                           "HG.ntr.fre","HG.ntr.occurrence","HG.ntr.duration",
                            "g1.fre","g2.fre","g3.fre","g4.fre","g5.fre",
                           "all.grade.ntr.fre",
                           "all.grade.ntr.occurrence",
                           "all.grade.ntr.duration",
                           "g1.trt.fre","g2.trt.fre","g3.trt.fre","g4.trt.fre","g5.trt.fre",
                           "g1.nontrt.fre","g2.nontrt.fre","g3.nontrt.fre","g4.nontrt.fre","g5.nontrt.fre",
                           "g1.occurrence","g2.occurrence","g3.occurrence","g4.occurrence","g5.occurrence",  
                           "g1.trt.occurrence","g2.trt.occurrence","g3.trt.occurrence","g4.trt.occurrence","g5.trt.occurrence",
                           "g1.duration","g2.duration","g3.duration","g4.duration","g5.duration",
                           "g1.trt.duration","g2.trt.duration","g3.trt.duration","g4.trt.duration","g5.trt.duration",
                           "g1.ntr.duration","g2.ntr.duration","g3.ntr.duration","g4.ntr.duration","g5.ntr.duration"
      )
       
      for (h in match(plotthesevars,names(df)))#[howmany])
      { #print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ next !!!')
        a4<-df;
        k<-k+1

        # Make outcomes ###
        a4$y<-a4[,h] # original continuous
        a4$AE.bin<-I(a4[,h]>0) # indicator TRUE if >0

        # best responses ###

        name.BOR<-c("Complete Response", "Partial Response","Stable Disease","Progressive Disease")
        a40<-a4%>%dplyr::filter(best_response%in%name.BOR)
        name.BOR1<-names(table(a40$best_response)[table(a40$best_response)>0])

        # Make a factor

        a40$best_response<-factor(a40$best_response,level=name.BOR[name.BOR%in%name.BOR1])

        # Summarize N by best response.
        nn<-a40%>%dplyr::group_by(best_response)%>%dplyr::summarise(n=n())

        # Plot boxplots ###
        plot1<-ggplot2::ggplot(a40, ggplot2::aes(y=y,x=best_response,fill=best_response)) +
          ggplot2::geom_boxplot(outlier.shape=NA) +
          ggplot2::geom_jitter(position = ggplot2::position_jitter(w = 0.2, h = 0))+
          ggplot2::theme(legend.position="none")+
          ggplot2::labs(title=names(a40)[h])+ggplot2::ylab(names(a40)[h])+
          ggpubr::stat_compare_means( ggplot2::aes(group=best_response),label = "p.signif", method="t.test", comparisons = combn(1:length(name.BOR1), 2, FUN = list))+
          ggplot2::geom_text(data=nn, ggplot2::aes(best_response,Inf,label = paste('(n=',n,')',sep='')), vjust = 1)+
          ggplot2::stat_summary(fun=mean, geom="point", shape=8, size=7, color="red", fill="red")


        a40<-a40%>%dplyr::mutate(disease_control=as.factor(dplyr::case_when(best_response %in% c("Complete Response","Partial Response","Stable Disease") ~ "DC",
                                                              best_response %in% c("Progressive Disease") ~ "PD")))

        name.BOR2<-names(table(a40$disease_control))
        nn<-a40%>%dplyr::group_by(disease_control)%>%dplyr::summarise(n=n())
        plot11<-ggplot2::ggplot(a40, ggplot2::aes(y=y,x=disease_control,fill=disease_control)) +
          ggplot2::geom_boxplot(outlier.shape=NA) +
          ggplot2::geom_jitter(position = ggplot2::position_jitter(w = 0.2, h = 0))+
          ggplot2::theme(legend.position="none")+
          ggplot2::labs(title=names(a40)[h])+ggplot2::ylab(names(a40)[h])+
          ggpubr::stat_compare_means( ggplot2::aes(group=disease_control),method="t.test", comparisons = combn(1:length(name.BOR2), 2, FUN = list))+
          ggplot2::geom_text(data=nn, ggplot2::aes(disease_control,Inf,label = paste('(n=',n,')',sep='')), vjust = 1)+
          ggplot2::stat_summary(fun=mean, geom="point", shape=8, size=7, color="red", fill="red")+ggplot2::xlab('')


        if(length(table(a4$AE.bin))>1)
        {
          cox1<-suppressWarnings(survival::coxph(survival::Surv(os_time,os_censor)~y,data=a4))

          tmp99.os<-coef(summary(cox1))

          if(exists("cox1")==TRUE){rm(cox1)}
          cox1<-suppressWarnings(survival::coxph(survival::Surv(os_time,os_censor)~AE.bin,data=a4))

          tmp99.os<-rbind(tmp99.os,coef(summary(cox1)))

          if(exists("cox1")==TRUE){rm(cox1)}
        } else {

          cox1<-suppressWarnings(survival::coxph(survival::Surv(os_time,os_censor)~y,data=a4))

          tmp99.os<-coef(summary(cox1))

          tmp.os <- coef(summary(cox1))

          tmp.os<-rep(NA,5)
          tmp99.os<-rbind(tmp99.os,tmp.os)

          if(exists("cox1")==TRUE){rm(cox1)}
          # print("END OS ------ END OS -----------------END OS ______")
        }
        #---PFS--
        if(length(table(a4$AE.bin))>1)
        {

          cox1<-suppressWarnings(survival::coxph(survival::Surv(pfs_time,pfs_censor)~y,data=a4))
          tmp99.pfs<-coef(summary(cox1))
          cox1<-suppressWarnings(survival::coxph(survival::Surv(pfs_time,pfs_censor)~AE.bin,data=a4))
          tmp99.pfs<-rbind(tmp99.pfs,coef(summary(cox1)))
          fit3 <- survival::survfit( survival::Surv(os_time,os_censor) ~ AE.bin, data = a4 )
          ggsurv <- survminer::ggsurvplot(fit3, data = a4, pval = TRUE,surv.median.line = 'hv',break.time.by = 4,risk.table = TRUE,
                               xlab='months',
                               tables.height = 0.3,
                               legend.title = "AE",
                               legend.labs = c("No", "Yes"),
                               palette = c("cyan","red"),
                               title=paste('OS: ',names(a4)[h]))
          plot2<-ggsurv

          fit3 <- survival::survfit( survival::Surv(pfs_time,pfs_censor)~ AE.bin,data = a4 )
          ggsurv <- survminer::ggsurvplot(fit3,data = a4, pval = TRUE,surv.median.line ='hv',break.time.by = 4,risk.table = TRUE,
                               xlab='months',
                               tables.height = 0.3,
                               legend.title = "AE",
                               legend.labs = c("No", "Yes"),
                               palette = c("cyan","red"),
                               title=paste('PFS: ',names(a4)[h]))
          plot3<-ggsurv


          if(exists("cox1")==TRUE){rm(cox1)}
          # print("END PFS ------ END PFS -----------------END PFS ______")
        } else {

          cox1<-suppressWarnings(survival::coxph(survival::Surv(pfs_time,pfs_censor)~y,data=a4))
          tmp99.pfs<-coef(summary(cox1))

          #cox1<-survival::coxph(survival::Surv(pfs_time,pfs_censor)~AE.bin,data=a4)
          # print('PFS.bin')
          tmp.pfs <- coef(summary(cox1))

          if(exists("cox1")==TRUE){rm(cox1)}
          tmp.pfs<-rep(NA,5)
          tmp99.pfs<-rbind(tmp99.pfs,tmp.pfs)

          if(exists("cox1")==TRUE){rm(cox1)}

          plot2<-NA

          plot3<-NA

          if(exists("cox1")==TRUE){rm(cox1)}
          #print("END PFS ------ END PFS -----------------END PFS ______")
        }
        AE.km.and.boxplot.list[[k]]<-list(bp1=plot1, bp2=plot11, kmos = plot2, kmpfs=plot3)
        coef.list[[k]]<-list(os=tmp99.os,pfs=tmp99.pfs)

      }

      names(coef.list)<-plotthesevars

      names(AE.km.and.boxplot.list)<-plotthesevars

      # make the coef.list into a data frame maybe for output?

      step1<-lapply(coef.list, function(x){   as.data.frame(do.call(rbind,x), row.names = NULL) %>%
          dplyr::mutate(outcome =c("OS","OS","PFS","PFS"),
                 predictor = c("y","AE.bin","y","AE.bin")) })
      step2<-do.call(rbind,step1)
      
      # ADD HERE TOO !? 2929 ####
      coef.data <- tibble::as_tibble(as.data.frame(step2) %>% 
                                       dplyr::mutate(
                                         AEmeasure  = rep( c("all.grade.duration","all.grade.fre",  "all.grade.occurrence","all.grade.trt.duration",
                                                                                 "all.grade.trt.fre","all.grade.trt.occurrence",  "LG.duration","LG.fre",
                                                                                 "LG.occurrence","LG.trt.duration",  "LG.trt.fre","LG.trt.occurrence",
                                                                                 "HG.duration","HG.fre",  "HG.occurrence","HG.trt.duration",  "HG.trt.fre","HG.trt.occurrence",
                                                             "LG.ntr.fre","LG.ntr.occurrence","LG.ntr.duration",
                                                             "HG.ntr.fre","HG.ntr.occurrence","HG.ntr.duration",
                                                                                 "g1.fre","g2.fre","g3.fre","g4.fre","g5.fre",
                                                                                 "all.grade.ntr.fre",
                                                                         "all.grade.ntr.occurrence" ,
                                                                         "all.grade.ntr.duration",
                                                                         "g1.trt.fre","g2.trt.fre","g3.trt.fre","g4.trt.fre","g5.trt.fre",
                                                                         "g1.nontrt.fre","g2.nontrt.fre","g3.nontrt.fre","g4.nontrt.fre","g5.nontrt.fre",
                                                                          "g1.occurrence","g2.occurrence","g3.occurrence","g4.occurrence","g5.occurrence",
                                                             
                                                             "g1.trt.occurrence","g2.trt.occurrence","g3.trt.occurrence","g4.trt.occurrence","g5.trt.occurrence",
                                                                         "g1.duration","g2.duration","g3.duration","g4.duration","g5.duration",
                                                                         "g1.trt.duration","g2.trt.duration","g3.trt.duration","g4.trt.duration","g5.trt.duration",
                                                                         "g1.ntr.duration","g2.ntr.duration","g3.ntr.duration","g4.ntr.duration","g5.ntr.duration"
      )
                                                                               ,each = 4)
    
      ))   %>% dplyr::mutate(AEmeasure = paste(AEmeasure,".",outcome,".",predictor, sep='')) %>%
        dplyr::select(AEmeasure, coef, HR=`exp(coef)`, `se(coef)`, z, `Pr(>|z|)`)

      # end o'Loop de loop
      return(list(coef.data = coef.data, plots = AE.km.and.boxplot.list))
    }

    testing <- kmandboxplotsfunction( toxicity.whole.summary.data())
    testing

  })

  output$rendBPKPplotSelection <- shiny::renderUI({

    BPKPplot_list <- kmandcoxphinfofromtoxdata()
    plot_grade_names <- names(BPKPplot_list$plots)
    all_plot_names <- c()
    for (i in plot_grade_names) {
      plot_grade_names_sub <- names(BPKPplot_list[["plots"]][[i]])
      for (j in plot_grade_names_sub) {
        if (is.list(BPKPplot_list[["plots"]][[i]][[j]])) {
          plot_name <- paste(i,"__",j,sep = "")
          all_plot_names <- c(all_plot_names,plot_name)
        }
      }
    }
    shiny::selectInput("BPKPplotSelection","Select Plot to View:", choices = all_plot_names)


  })
  # >*>*>*>*>*>*>*>*>*>* BP OR KM PLOT HERE ####
  output$BPKP_PlotOutput <- shiny::renderPlot({

    req(input$BPKPplotSelection)
    BPKPplot_list <- kmandcoxphinfofromtoxdata()
    BPKP_Select <- input$BPKPplotSelection
    BPKP_name1 <- strsplit(BPKP_Select,"__")[[1]][1]
    BPKP_name2 <- strsplit(BPKP_Select,"__")[[1]][2]
    plot <- BPKPplot_list[["plots"]][[BPKP_name1]][[BPKP_name2]]
    plot

  })


  BPKP_PlotOutput <- shiny::reactive({

    req(input$BPKPplotSelection)
    BPKPplot_list <- kmandcoxphinfofromtoxdata()
    BPKP_Select <- input$BPKPplotSelection
    BPKP_name1 <- strsplit(BPKP_Select,"__")[[1]][1]
    #print("WHICH KM OR BOX PLOT")
    #print(BPKP_name1)
    BPKP_name2 <- strsplit(BPKP_Select,"__")[[1]][2]
    #print(BPKP_name2)
    plot <- BPKPplot_list[["plots"]][[BPKP_name1]][[BPKP_name2]]
    plot

  })



  output$BMSKMpc1drug1 <- shiny::renderPlot({   print(BPKP_PlotOutput())    })
  #allow users to download the  KM OR BOXPLOT ####
  { output$download_BPKP_PlotOutput <- shiny::downloadHandler(
    filename = function() {
      #IDnum<-IDnum()
      BPKP_Select <- input$BPKPplotSelection
      BPKP_name1 <- strsplit(BPKP_Select,"__")[[1]][1]
      #print("WHICH KM OR BOX PLOT")
      #print(BPKP_name1)
      BPKP_name2 <- strsplit(BPKP_Select,"__")[[1]][2]
      #print(BPKP_name2)
      paste0("plot_of_",BPKP_name1, "_",BPKP_name2,"_", Sys.Date(), ".png")

    },
    content = function(file) {

      BPKP_Select <- input$BPKPplotSelection
      BPKP_name1 <- strsplit(BPKP_Select,"__")[[1]][1]
      #print("WHICH KM OR BOX PLOT")
      #print(BPKP_name1)
      BPKP_name2 <- strsplit(BPKP_Select,"__")[[1]][2]
      #print(BPKP_name2)
      #print("KMLPlOT?")
      #print(any(BPKP_name2 %in% c("bp1","bp2")))
      if (any(BPKP_name2 %in% c("bp1","bp2")) ){
        ggplot2::ggsave(file, plot = BPKP_PlotOutput(),  width = 14,
               height = 8,
               units = c("in"),#, "cm", "mm", "px"),
               dpi = 300 )
      } else {
        #print("GOING TO ELSE")
        png(file ,
            width = 7, height = 8, units = "in",  res = 300
            # pointsize = 12, bg = "white",
            #, family = "", restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), antialias, symbolfamily="default"
        )
        print(BPKP_PlotOutput(), newpage = FALSE)
        dev.off()
      }



    }
  )
  } ####
  # { output$download_BPKP_PlotOutput <- shiny::downloadHandler(
  #   filename = function() {
  #     #IDnum<-IDnum()
  #     BPKP_Select <- input$BPKPplotSelection
  #     BPKP_name1 <- strsplit(BPKP_Select,"__")[[1]][1]
  #     print("WHICH KM OR BOX PLOT")
  #     print(BPKP_name1)
  #     BPKP_name2 <- strsplit(BPKP_Select,"__")[[1]][2]
  #     print(BPKP_name2)
  #     paste0("plot_of_",BPKP_name1, "_",BPKP_name2,"_", Sys.Date(), ".png")
  #
  #   },
  #   content = function(filename) {
  #
  #     png(filename ,
  #         width = 7, height = 8, units = "in",  res = 300
  #         # pointsize = 12, bg = "white",
  #         #, family = "", restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), antialias, symbolfamily="default"
  #     )
  #     BPKP_PlotOutput()
  #     #print(BPKP_PlotOutput(), newpage = FALSE)
  #     dev.off()
  #
  #
  #     }
  # )
  # }


  # Display survival::coxph output for AE measures ####
  output$kmcophinfo <- DT::renderDataTable({
    todisplay <-  kmandcoxphinfofromtoxdata()$coef.data   %>%
       dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 4)) %>%
     # dplyr::mutate(dplyr::across(dplyr::where(is.numeric),\(x) round(x, 4))) %>%
      dplyr::filter(is.na(coef)==FALSE)
    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 72))

  })



  kmandcoxphinfofromtoxdatadisplay<- shiny::reactive({
    kmandcoxphinfofromtoxdata()$coef.data   %>%
       dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 4)) %>%
      #dplyr::mutate(dplyr::across(dplyr::where(is.numeric),\(x) round(x, 4))) %>%
      dplyr::filter(is.na(coef)==FALSE)
  })

  # Download handler for kmandcoxphinfofromtoxdatadisplay data #####
  {
    output$kmcoxphinfodownload <- shiny::downloadHandler(
      filename = function(){"Overall_coxph_info_download.csv"},
      content = function(fname){
        write.csv(kmandcoxphinfofromtoxdatadisplay(), fname, row.names = FALSE)
      }
    )
  }







  # KM and boxplots of responses####
  draw_plot <- function() {  kmandcoxphinfofromtoxdata()$plots  }

  observe({
    obj <- kmandcoxphinfofromtoxdata()
    #save(list = ls(),file = "KM_Plots.RData", envir = environment())
  })

  # Download km and box plots ####
  output$plotall <- shiny::renderPlot({ draw_plot() })

  # Download KM plots pdf ###

  # hopefully for either or PC OR biostools
  # Download KM plots pdf ####
  output$downloadplotspdf <- shiny::downloadHandler(
    filename = "Boxplots_and_KM_plots_report.pdf",
    content = function(file) {

      # sysname <- Sys.info()[1]
      # if (sysname == "Windows") {
      #   filetorender <-  "template.Rmd"
      # } else {
      #   filetorender <- "/var/shiny-server/data/ThompsZJ/newAPPtesting/template.Rmd"
      # }


      filetorender <-  "template.Rmd"

      res <- rmarkdown::render(
        input = filetorender , # "template.Rmd",
        params = list(
          draw_plot = draw_plot
        )
      )

      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }


    }
  )
  # end: survival::coxph AE measures tab ^^^^^^^^^^^^^^^^^^^^^^^####


  # start: Forest plots OS and PF tab VVVVVVVVVVVVVVVV####
  # FOREST PLOTS FOR P-values ###
  responsePvaluelistforestplots <- shiny::eventReactive(input$goforstplotstests,{

    toxdataNOW <- toxicity.whole.summary.data()

    a400<-list(whole=alldataoutput()$toxicity.whole.summary.data)

    a400<-c(a400,
            alldataoutput()$toxicity.category.summary.data,
            alldataoutput()$toxicity.type.summary.data)
    a41now<-a400

    forestplots<-function(a41=a41now,a4.whole.summary=toxdataNOW){ # input will be a a list of 3 data sets and a data set with survival information,
      N <-  length(a41)
      #--calcuate cox coef and p value for OS and PFS----
      coef.list.group<-list()
      for(j in 1:N)
      {
     AETYPEorCAT <-names(a41)[j]
     print("AETYPEorCAT---------------------")
     print(AETYPEorCAT)
        # a4<-dplyr::left_join(a41[[j]],a4.whole.summary[,c(2,21:dim(a4.whole.summary)[2])])
        # a4<-dplyr::left_join(a41[[j]],a4.whole.summary[,c(2,26:dim(a4.whole.summary)[2])],by="pid")
        
     # CHANGE COL NUMBERS WHEN ADDING BIOMARKERS ####
     # print("--line 2978---------------names(a4.whole.summary)-----------------")
     # print(names(a4.whole.summary))
          #a4<-dplyr::left_join(a41[[j]],a4.whole.summary[,c(2,30:dim(a4.whole.summary)[2])],by="pid")
          #a4<-dplyr::left_join(a41[[j]],a4.whole.summary[,c(2,34:dim(a4.whole.summary)[2])],by="pid")
          #a4<-dplyr::left_join(a41[[j]],a4.whole.summary[,c(2,39:dim(a4.whole.summary)[2])],by="pid")
          a4<-dplyr::left_join(a41[[j]],a4.whole.summary[,c(2,match("followup_start_date",names(a4.whole.summary)):dim(a4.whole.summary)[2])],by="pid")
     
        # print("names(a4)")
        # print(names(a4))
        png.status<-F
        # print("png.status")
        # print(png.status)
        coef.list<-list()
        k<-0
  # 3114 ADD VARS HERE ####
        measures<-c("all.grade.duration","all.grade.fre",
                    "all.grade.occurrence","all.grade.trt.duration","all.grade.trt.fre",
                    "all.grade.trt.occurrence", "LG.duration","LG.fre",
                    "LG.occurrence","LG.trt.duration","LG.trt.fre",
                    "LG.trt.occurrence","HG.duration","HG.fre",
                    "HG.occurrence","HG.trt.duration","HG.trt.fre",
                    "HG.trt.occurrence",
                    
                    "LG.ntr.fre","LG.ntr.occurrence","LG.ntr.duration",
                    "HG.ntr.fre","HG.ntr.occurrence","HG.ntr.duration",
                    "g1.fre","g2.fre","g3.fre","g4.fre","g5.fre",
                    "all.grade.ntr.fre","all.grade.ntr.occurrence","all.grade.ntr.duration",
                    "g1.trt.fre","g2.trt.fre","g3.trt.fre","g4.trt.fre","g5.trt.fre",
                    "g1.nontrt.fre","g2.nontrt.fre","g3.nontrt.fre","g4.nontrt.fre","g5.nontrt.fre", 
                   "g1.occurrence","g2.occurrence","g3.occurrence","g4.occurrence","g5.occurrence"  ,   
                   "g1.trt.occurrence","g2.trt.occurrence","g3.trt.occurrence","g4.trt.occurrence","g5.trt.occurrence",
                    "g1.duration","g2.duration","g3.duration","g4.duration","g5.duration",
                    "g1.trt.duration","g2.trt.duration","g3.trt.duration","g4.trt.duration","g5.trt.duration",
                    "g1.ntr.duration","g2.ntr.duration","g3.ntr.duration","g4.ntr.duration","g5.ntr.duration"
        )
         

        for(h in  match(measures,names(a4)) )
        {
          #print("h-------------")
          # print(h)
          #print(names(a4)[h])
          k<-k+1

          a4=a4%>%dplyr::mutate(y=.data[[names(a4)[h]]], AE.bin=I(y>0))
          name.BOR<-c("Complete Response",   "Partial Response","Stable Disease","Progressive Disease")
          a40<-a4%>%dplyr::filter(best_response%in%name.BOR)
          name.BOR1<-names(table(a40$best_response)[table(a40$best_response)>0])
          a40$best_response<-factor(a40$best_response,level=name.BOR[name.BOR%in%name.BOR1])
          if(length(table(a4$AE.bin))>1)
          {
            if(length(table(a4$AE.bin))>1)
            {
              cox1<-suppressWarnings(survival::coxph(survival::Surv(os_time,os_censor)~y,data=a4))
              temp<-coef(summary(cox1))
              tmp99.os<- c(coef(summary(cox1)),
                           LCL= round(exp(temp[1]  - 1.96*temp[3]),3),
                           UCL= round(exp(temp[1]  + 1.96*temp[3]),3))
              names(tmp99.os)[1:5] <- c( "coef", "exp(coef)" , "se(coef)", "z", " Pr(>|z|)" ) #names(coef(summary(cox1)))

              if(exists("cox1")==TRUE){rm(cox1)}
              cox1<-suppressWarnings(survival::coxph(survival::Surv(os_time,os_censor)~AE.bin,data=a4))
              temp<-coef(summary(cox1))
              tmp99.os.bin<- c(coef(summary(cox1)),
                               LCL= round(exp(temp[1] - 1.96*temp[3]),3),
                               UCL= round(exp(temp[1] + 1.96*temp[3]),3))
              names(tmp99.os.bin)[1:5] <- c( "coef", "exp(coef)" , "se(coef)", "z", " Pr(>|z|)" )
              tmp99.os<-rbind(tmp99.os,tmp99.os.bin)
              rownames(tmp99.os)<- c("y","AE.binTRUE")

              if(exists("cox1")==TRUE){rm(cox1)}
            }
            #---PFS-----
            if(length(table(a4$AE.bin))>1)
            {
              cox1<-suppressWarnings(survival::coxph(survival::Surv(pfs_time,pfs_censor)~y,data=a4))
              temp<-coef(summary(cox1))
              tmp99.pfs<- c(coef(summary(cox1)),
                            LCL= round(exp(temp[1] - 1.96*temp[3]),3),
                            UCL= round(exp(temp[1] + 1.96*temp[3]),3))
              names(tmp99.pfs)[1:5] <- c( "coef", "exp(coef)" , "se(coef)", "z", " Pr(>|z|)" )

              if(exists("cox1")==TRUE){rm(cox1)}
              cox1<-suppressWarnings(survival::coxph(survival::Surv(pfs_time,pfs_censor)~AE.bin,data=a4))
              temp<-coef(summary(cox1))
              tmp99.pfs.bin<- c(coef(summary(cox1)),
                                LCL= round(exp(temp[1] - 1.96*temp[3]),3),
                                UCL= round(exp(temp[1] + 1.96*temp[3]),3))
              names(tmp99.pfs.bin)[1:5] <- c( "coef", "exp(coef)" , "se(coef)", "z", " Pr(>|z|)" )

              if(exists("cox1")==TRUE){rm(cox1)}

              tmp99.pfs<-rbind(tmp99.pfs,tmp99.pfs.bin)
              rownames(tmp99.pfs)<- c("y","AE.binTRUE")

              if(exists("cox1")==TRUE){rm(cox1)}

            }
            coef.list<-c(coef.list,list(list(os=tmp99.os,pfs=tmp99.pfs)))
            names(coef.list)[length(coef.list)]<-names(a4)[h]
          }

        }  #--for h loop--
        # print("~~~~~~~do it again !!!!!!!!!!!!!!!!!!")

        coef.list.group[[j]]<-coef.list
      }  #--for j loop--

      names(coef.list.group)<-names(a41)[1:N]#length(a41)


      # NOW TAKE THE OUTPUT OF THAT LOOP DE LOOP and use it to plot the p-values and HRs....---

      #---get occurrence (yes/no) based on dichotomized AE----
      fun1<-function(my.data.list=coef.list) # Input is coef.list.group a list of survival::coxph results... coeffiences and pvalues etc...
      {
        surv.name<-c('os','pfs')
        e2.comb<-numeric()
        for(i in 1:length(surv.name)) #loop over os and pfs vars
        {
          e1=sapply(my.data.list,function(x) x[[surv.name[i]]][,c(2,5:7)],simplify=F) # get the HR and pvalue
          # print("e1")
          # print(e1)
          e2<-data.frame(do.call("rbind",e1))    #combine them into a data frame
          # print("e2")
          # print(e2)
          dim.index<-dim(e2[!is.na(e2[,2]),])[1] #--ensure not all NA--
          if(dim.index>0)
          {
            #What the heck is going on here... dplyr::mutate making a var to indicate OS or PFS and a var for AE type and measure type either continuous or Occurrence
            e2<-e2 %>% tibble::rownames_to_column(var='data.type') %>% #dplyr::add_rownames(var='data.type') %>%
              dplyr::mutate(survival=surv.name[i],
                     AE.type=rep(names(my.data.list),each=2),
                     data.type=sub('y.*','Continuous',sub('AE.*','Occurrence',data.type))) %>%
              dplyr::relocate(AE.type)
            names(e2)[3:4]<-c('HR','p') # dplyr::rename
             #  print("e2 again ....")
             # print(e2)
             #  print(dim(e2))
            e21<-e2%>%dplyr::filter(data.type %in%'Occurrence')%>%
              dplyr::slice(dplyr::ends_with('occurrence',vars=AE.type))
              # print("e21 ")
              #print(head(e21,7))
              # print(e21)
              # print(dim(e21))
            e22<-e2%>%dplyr::filter(data.type %in%'Continuous')%>%
              dplyr::slice(dplyr::ends_with('occurrence',vars=AE.type))%>%
              dplyr::mutate(AE.type=sub('occurrence','sum.unique',AE.type))
              #print(head(e22,7))
            # print("e22 ")
            # print(e22)
            #   print(dim(e22))
            e23<-e2%>%dplyr::filter(data.type %in%'Continuous')%>%
              dplyr::slice(-dplyr::ends_with('occurrence',vars=AE.type))
              # print(head(e23,12)) 
            # print("e23 ")
            # print(e23)
            #   print(dim(e23))
            e2.comb<-rbind(e2.comb,rbind(e21,e22,e23))
              # print("ecombined")
              # print(e2.comb)
              # print(dim(e2.comb))
              
             
          }
        }

        e2.comb
      }

      #---generate AE survival p value plot for each AE----
      AE.survival.p.plot.list<-list()
      AE.survival.p.forestplot.list.test <- list()
      AE.survival.p.forestplot.list<-plot.data.AE.survival.p.forestplot.list<-list()
      k<-0
      e3<-numeric()
      for(i in 1:length(coef.list.group))
        # for(i in 1:N)
      {
        name.tmp<-names(coef.list.group)[i] # AE type ( or all = whole)
        # print("name.tmp----------------")
        # print(name.tmp)
        e2<-fun1(my.data.list=coef.list.group[[i]]) # HERE WE call the function above.
        
        #print(str(e2))
        #print("WHAT IS WRONG !!!!!   e2")
        #print(head(e2))

        #if(length(e2)>0)
        if(NROW(e2)>0)
        {
          k<-k+1
          e2<-e2%>%dplyr::mutate(AE=name.tmp) # make a variable for the TYPE

          e3<-rbind(e3,e2) #rowbind something with e2

          e3<-e3%>%dplyr::relocate(AE)
          # print("e3")
          # print(head(e3))
          # e4 is the plot data

          e4<- as.data.frame(e2 %>% dplyr::group_by(survival) %>% 
                               arrange(survival, AE.type) %>%
                               dplyr::mutate(index =  n():1))  %>%

            dplyr::mutate(
              HR0=HR,
              UCL0 = UCL,
              LCL0 = LCL,
              HR = dplyr::case_when(HR0 > 10 ~ NA_real_,
                             HR0 >= 0.01 & HR0 <= 10 ~ HR0,
                             HR0 < 0.01 ~ NA_real_,
                             !is.finite(HR0)~ NA_real_) ,
              UCL = dplyr::case_when(UCL0 > 10 ~ NA_real_,
                              UCL0 >= 0.01 & UCL0 <= 10 ~ UCL0,
                              UCL0 < 0.01 ~ NA_real_,
                              !is.finite(UCL0)~ NA_real_ ),
              LCL = dplyr::case_when(LCL0 > 10 ~ NA_real_,
                              LCL0 >= 0.01 & LCL0 <= 10 ~ LCL0,
                              LCL0 < 0.01 ~ NA_real_,
                              !is.finite(LCL0)~ NA_real_ ) ,
              UCL = dplyr::case_when(!is.infinite(UCL)  ~ UCL,
                              is.infinite(UCL) ~ NA_real_ ),
              LCL = dplyr::case_when(!is.infinite(LCL)  ~ LCL,
                              is.infinite(LCL) ~ NA_real_  )

            )

          e5<- as.data.frame(e3   %>% dplyr::group_by(survival) %>% arrange(survival, AE.type) %>%
                               dplyr::mutate(index = n():1)) %>%
            dplyr::mutate(
              HR0=HR,
              UCL0 = UCL,
              LCL0 = LCL,
              HR = dplyr::case_when(HR0 > 10 ~ NA_real_,
                             HR0 >= 0.01 & HR0 <= 10 ~ HR0,
                             HR0 < 0.01 ~ NA_real_,
                             !is.finite(HR0)~ NA_real_) ,
              UCL = dplyr::case_when(UCL0 > 10 ~ NA_real_,
                              UCL0 >= 0.01 & UCL0 <= 10 ~ UCL0,
                              UCL0 < 0.01 ~ NA_real_,
                              !is.finite(UCL0)~ NA_real_ ),
              LCL = dplyr::case_when(LCL0 > 10 ~ NA_real_,
                              LCL0 >= 0.01 & LCL0 <= 10 ~ LCL0,
                              LCL0 < 0.01 ~ NA_real_,
                              !is.finite(LCL0)~ NA_real_ ) ,
              UCL = dplyr::case_when(!is.infinite(UCL)  ~ UCL,
                              is.infinite(UCL) ~ NA_real_ ),
              LCL = dplyr::case_when(!is.infinite(LCL)  ~ LCL,
                              is.infinite(LCL) ~ NA_real_  )

            )

          #VVVVVVVVVVVVVVVVGGPLOT VVVVVVVVVVV~
    # print("-----------------e4-------------------")
    # print(names(e4))
    # print(head(e4,30))
    #gogogadget::gogogadgetexport(e4)
    
    # Split the dataset by AE.Type using filtering
    duration_data <- e4 %>% filter(grepl("duration", AE.type)) %>% mutate(cate="duration")
    fre_data <- e4 %>% filter(grepl("fre", AE.type))%>% mutate(cate="fre")
    occurrence_data <- e4 %>% filter(grepl("occurrence", AE.type))%>% mutate(cate="occurrence")
    sum_unique_AE_data <- e4 %>% filter(grepl("sum.unique", AE.type))%>% mutate(cate="sum.unique")
    
    # print("THE DIMENSIONS !@#$%^&$@#!@#^&#%$!@%#^$@#!@%#^#$!@%#^$&^%@#$%^$#@")
    # print(dim(duration_data))
    # print(dim(fre_data))
    # print(dim(occurrence_data))
    # print(dim(sum_unique_AE_data))
    e4o<-e4
    # print("dim(e4o")
    # print(dim(e4o))
    e4v2<- rbind(duration_data,fre_data,occurrence_data,sum_unique_AE_data)
    # print("dim(e4)")
    # print(dim(e4v2))
    # Initialize an empty list to store plots
    #AE.survival.p.forestplot.list <- list()
    AE.survival.p.forestplot.list.cat <- list()
    AE.survival.p.forestplot.list.cat.NROWS <-c(NA,NA,NA,NA)
    # Define the AE.type categories
    categories <- c("duration", "fre", "occurrence", "sum.unique")
    
    # Loop through each AE.type category and generate a forest plot
    for (cat in categories) {
      # Filter the data for the current AE.Type
      e4_filtered <- e4v2 %>% filter(cate == cat)  
      AE.survival.p.forestplot.list.cat.NROWS[cat]<-NROW(e4_filtered)
      catlabel <- ifelse(cat=="fre","frequency",cat)
      # print("names(e4_filtered)----------------------------------")
      # print(names(e4_filtered))
      # print(dim(e4_filtered))
      # print(cat)
      # Create the forest plot for this category
      AE.survival.p.forestplot.list.cat[[cat]] <- ggplot2::ggplot(e4_filtered, ggplot2::aes(y = index, x = HR, col = p < 0.05)) +
        scale_colour_manual(values = c("red", "cyan"), breaks = c(TRUE, FALSE), labels = c('<0.05', '>0.05')) +
        facet_wrap(vars(survival)) +
        ggplot2::geom_point(shape = 18, size = 3) +
        geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
        ggplot2::geom_vline(xintercept = 1, color = "black", linetype = "dashed", cex = 0.8, alpha = 0.5) +
        ggplot2::scale_y_continuous(name = "", breaks = e4_filtered$index, labels = e4_filtered$AE.type) +
        ggplot2::xlim(c(-1, max(e4_filtered$UCL) + 1)) +
        ggplot2::xlab("Hazard Ratio (95% CI)") +
        ggplot2::ylab("") +
        ggplot2::labs(title = paste("Forest Plot for", catlabel)) +
        theme_minimal()
    }
    
    library(patchwork)
    
    # Combine the plots into a 4x1 grid
    # grid_plot <- (
    #     AE.survival.p.forestplot.list.cat[["fre"]] +
    #     AE.survival.p.forestplot.list.cat[["occurrence"]] +
    #     AE.survival.p.forestplot.list.cat[["sum.unique"]] +
    #     AE.survival.p.forestplot.list.cat[["duration"]]
    # ) + plot_layout(ncol = 1, nrow = 4)
    
    grid_plot <- (
      AE.survival.p.forestplot.list.cat[["fre"]] /
        AE.survival.p.forestplot.list.cat[["occurrence"]] /
        AE.survival.p.forestplot.list.cat[["sum.unique"]] /
        AE.survival.p.forestplot.list.cat[["duration"]]
    ) + plot_layout(heights = max(c(AE.survival.p.forestplot.list.cat.NROWS["fre"], AE.survival.p.forestplot.list.cat.NROWS["occurrence"],
                                AE.survival.p.forestplot.list.cat.NROWS["sum.unique"], AE.survival.p.forestplot.list.cat.NROWS["duration"])
    )
                    )
    
    
    
    
    
    
    # Save the grid plot in the list
    AE.survival.p.forestplot.list.test[[k]] <- grid_plot
    names(AE.survival.p.forestplot.list.test)[k]<-name.tmp
 
     #FOREST PLOT PVAL####
         plot.data.AE.survival.p.forestplot.list[[k]] <- e4
          AE.survival.p.forestplot.list[[k]]<- ggplot2::ggplot(e4, ggplot2::aes(y = index, x = HR, col = p<0.05)) + #
            scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
            facet_wrap(vars(survival)) +
            ggplot2::geom_point(shape = 18, size = 3) +
            geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
            ggplot2::geom_vline(xintercept = 1, color = "black", linetype = "dashed", cex = .8, alpha = 0.5) +
            ggplot2::scale_y_continuous(name = "", breaks=e4$index, labels = e4$AE.type)+#, trans = "reverse") +
            xlim(  c( -1,max(e4$UCL)+1)) +
            ggplot2::xlab("Hazard Ratio (95% CI)") +
            ggplot2::ylab(" ")+
            ggplot2::labs(title=name.tmp)
           names(AE.survival.p.forestplot.list)[k]<-name.tmp
           names(plot.data.AE.survival.p.forestplot.list)[k]<-name.tmp

        }


      }
      
   #   sink()
      #---save survival p value data----
    
      responsePvaluelist <-list(coef=coef.list.group,
                                coef.long=e3,
                                coef.long4=plot.data.AE.survival.p.forestplot.list,
                                filtered.coef.long=e5
                                ,plot=AE.survival.p.forestplot.list
                                ,gridplots=AE.survival.p.forestplot.list.test
                                 )
      
      
    }


    forestplots(a41=a41now,a4.whole.summary=toxdataNOW)
    
    })

  output$rendForestplotSelection <- shiny::renderUI({
    Forestplot_list <- responsePvaluelistforestplots()
    #plot_grade_names <- names(Forestplot_list$plot)
    plot_grade_names <- names(Forestplot_list$gridplots)
    
    shiny::selectInput("ForestplotSelection","Select Plot to View:", choices = plot_grade_names)

  })

  output$Forest_PlotOutput <- shiny::renderPlot({

    req(input$ForestplotSelection)
    Forestplot_list <- responsePvaluelistforestplots()
    Forest_Select <- input$ForestplotSelection
    #plot <- Forestplot_list[["plot"]][[Forest_Select]]
    plot <- Forestplot_list[["gridplots"]][[Forest_Select]]
    plot

  })

  output$forestplotcophinfo <- DT::renderDataTable({
    todisplay <-  responsePvaluelistforestplots()$coef.long %>% 
       dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 4))
     #     dplyr::mutate(dplyr::across(dplyr::where(is.numeric),\(x) round(x, 4)))  
    #todisplay
    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 100))
  })


  forestplotcophinfodatadisplay <- shiny::reactive({
    todisplay <-  responsePvaluelistforestplots()$coef.long %>% 
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 4))
      #dplyr::mutate(dplyr::across(dplyr::where(is.numeric),\(x) round(x, 4)))  
    todisplay
  })

  # Download handler for kmandcoxphinfofromtoxdatadisplay data #####
  {
    output$forestplotcophinfodownload <- shiny::downloadHandler(
      filename = function(){"Forest_plot_coxph_results_download.csv"},
      content = function(fname){
        write.csv(forestplotcophinfodatadisplay(), fname, row.names = FALSE)
      }
    )
  }


  # draw_forestplot of responses ###
 # draw_forestplot <- function() {  responsePvaluelistforestplots()$plot   }
  draw_forestplot <- function() {  responsePvaluelistforestplots()$gridplots   }
  
  # render draw_forestplot ###
  output$plotallforest <- shiny::renderPlot({   draw_forestplot() })

  # Download FOREST PLOTS pdf ####
  output$downloadforestplotspdf <- shiny::downloadHandler(
    filename = "rendered_forest_plots_report.pdf",

    # content = function(file) {
    #  sysname <- Sys.info()[1]
    #  if (sysname == "Windows") {
    #    filetorender <-  "template_forestplots.Rmd"
    #  } else {
    #    filetorender <- "/var/shiny-server/data/ThompsZJ/newAPPtesting/template_forestplots.Rmd"
    #  }
    #
    #  res <- rmarkdown::render(
    #    input = filetorender , # "template_forestplots.Rmd",
    #    params = list(
    #      draw_plot = draw_forestplot
    #    )
    #  )
    #
    #
    #  if (sysname == "Windows") {
    #    file.rename(res, file)
    #  } else {
    #    file.copy(res, file)
    #  }
    #
    #
    # }

    content = function(file) {
      res <- rmarkdown::render(
        input = "template_forestplots.Rmd",
        params = list(
          draw_forestplot = draw_forestplot
        )
      )

      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }


    }
  )

  # end: Forest plots OS and PFS tab   ^^^^^^^^^^^^^^^^####
  # start: RESPONSE tests tab VVVVVVVVVVVVVVVV####
  # testing_BOR_ans() shiny::reactive data set plots and t.test results... ####

  #testing_BOR_ans   <- shiny::reactive(input$goresponsetests,{
  testing_BOR_ans   <- shiny::eventReactive(input$goresponsetests,{
    toxdataNOW <- toxicity.whole.summary.data()
    #print("RUN RESPONSE TESTS....")
    a400<-list(whole=alldataoutput()$toxicity.whole.summary.data)


    a400<-c(a400,
            alldataoutput()$toxicity.category.summary.data,
            alldataoutput()$toxicity.type.summary.data)
    a41now<-a400
    
    
     responseplots<- function(a41=a41,adf = a1$toxicity.ans.summary_sub){
      
      a4.whole.summary<-adf;
      
      coef.list.group<-AE.BOR.p.plot.list<-list()
      for(j in 1:length(a41))
      {
        #if(j%%10 == 0 ){#print("working on it: #");
        #print(j)
        #}
        a41.tmp<-dplyr::ungroup(a41[[j]])
        var.drop<-c('AE.category','AE')[c('AE.category','AE')%in%names(a41.tmp)]
        #---get AE occurrence from sum.unique.AE
        a41.tmp<-dplyr::select(a41.tmp,-all_of(var.drop))
        a41.tmp.sum.unique.AE<-a41.tmp%>%dplyr::select(c(pid,dplyr::ends_with('occurrence')))
        a41.tmp.occurrence<-as.list(a41.tmp.sum.unique.AE)[-1]%>%map_dfr(function(x) as.numeric(x>0))
        names(a41.tmp.sum.unique.AE)<-sub('occurrence','sum.unique',names(a41.tmp.sum.unique.AE))
        a41.tmp.fre.duraiton<-a41.tmp%>%dplyr::select(c(pid,dplyr::ends_with(c('fre','duration'))))
        a41.tmp.new<-cbind(a41.tmp.fre.duraiton,a41.tmp.sum.unique.AE%>%dplyr::select(-pid),a41.tmp.occurrence)
        #a4<-suppressMessages(dplyr::left_join(a41.tmp.new,a4.whole.summary[,c(2,21:dim(a4.whole.summary)[2])]))
        #a4<-suppressMessages(dplyr::left_join(a41.tmp.new,a4.whole.summary[,c(2,26:dim(a4.whole.summary)[2])],by="pid"))
        
        
        # print(names(a4.whole.summary))
        # print("-----names(a41.tmp.new)-----")
        # print(names(a41.tmp.new))
       # a4<-suppressMessages(dplyr::left_join(a41.tmp.new,a4.whole.summary[,c(2,30:dim(a4.whole.summary)[2])],by="pid"))
        # a4<-suppressMessages(dplyr::left_join(a41.tmp.new,a4.whole.summary[,c(2,34:dim(a4.whole.summary)[2])],by="pid"))
        # a4<-suppressMessages(dplyr::left_join(a41.tmp.new,a4.whole.summary[,c(2,44:dim(a4.whole.summary)[2])],by="pid"))
        # print("gooooooooooooooooo->match(names(a4.whole.summary), followup_start_date )")
        # print(match("followup_start_date",names(a4.whole.summary)))
        # 
          a4<-suppressMessages(dplyr::left_join(a41.tmp.new,a4.whole.summary[,c(2,match("followup_start_date",names(a4.whole.summary)):dim(a4.whole.summary)[2])],by="pid"))
          
          # print("-----names(a4)-----")
          # print(names(a4))
        name.BOR<-c("Complete Response",   "Partial Response","Stable Disease","Progressive Disease")
        name.BOR.short<-c("CR","PR","SD","PD")
        a40<-a4%>%dplyr::filter(best_response%in%name.BOR)
        name.BOR1<-names(table(a40$best_response)[table(a40$best_response)>0])
        a40$best_response<-factor(a40$best_response,level=name.BOR[name.BOR%in%name.BOR1],label=name.BOR.short[name.BOR%in%name.BOR1])
        
        
        # print("-----names(a40)-----")
        # print(dim(a40))
        # print(names(a40))
        
        
        #AE.var<-names(a40)[2:25] # Just the freq 
        # all AE.vars
        AE.var<-names(a40)[2:87] # all  
        
        data.tmp<-a40%>%dplyr::select(c(pid,best_response,all_of(AE.var)))%>%tidyr::pivot_longer(cols=-(1:2),names_to ='type',values_to = 'value' )
        #---comparison of DC (CR/PR/SD) vs PD---
        tmp.com<-list('DC_vs_PD'=name.BOR.short,'PR_vs_PD'=name.BOR.short[c(1,2,4)],'SD_vs_PD'=name.BOR.short[c(3,4)])
        var1<-expand.grid(tmp.com,AE.var)
        var1$BOR<-rep(names(tmp.com),length(AE.var))
        # print("this is the var 1 df to test the rows")
        # print(var1)
        if("AE.category" %in% names(a41[[j]])) {var1$AEtype <- unique(a41[[j]]$AE.category)} else {
          if("AE" %in% names(a41[[j]])) {var1$AEtype <- unique(a41[[j]]$AE)} else {var1$AEtype <- "whole"
          }
        }#end if statements
        
        #options(warn=-1)
        tissue.test.ans<-  pmap(var1,~data.tmp%>%dplyr::filter((best_response%in%..1)&(type%in%..2)))%>%
          map(function(x)
          {
            
            df <- x %>% 
              # dplyr::mutate(g1 = as.character(factor(x$best_response=='PD',level=c(F,T),label=c('PD','DC')))) %>%
                dplyr::mutate(g1 = factor(x$best_response=='PD',level=c(F,T),label=c('PD','DC')))  %>%
              dplyr::select(value,g1)
            
            # print("the df to test:")
            # print(df)
            #
            if( inherits(
              try(tmp1<-suppressMessages(t.test(df$value~df$g1))
                  , silent = T)
              , "try-error",T)
              
            ){
              ans<-c(NA, NA) 
              ans<-c(NA, NA, NA,NA,NA,NA ) 
              
            } else{
              tmp1<-suppressMessages(t.test(df$value~df$g1))
              # print("tmp1 ")
              # print(tmp1 ) 
              #print("(tmp1$estimate")
              #print(tmp1$estimate)
              #print("diff(tmp1$estimate)")
              #print(diff(tmp1$estimate))
              
              xbarDC<-round(unname(tmp1$estimate[1]),2)
              xbarPD<-round(unname(tmp1$estimate[2]),2)
              diff2<-xbarDC-xbarPD
              ans<-c(tmp1$p.value,diff(tmp1$estimate[2:1]))
              ans<-c(tmp1$p.value,round(diff(tmp1$estimate[2:1]),2),
                     round(tmp1$conf.int[1],3),round(tmp1$conf.int[2],3),
                     xbarDC,xbarPD)
              
              
            }
          })
        
        
        #tissue.test.ans.long<-cbind(var1,t(sapply(tissue.test.ans,c)))
        # print("*****************************tissue.test.ans.long*****************************")
        # print(head(tissue.test.ans.long))
        
        
        tissue.test.ans.long<-cbind(var1,t(sapply(tissue.test.ans,c)))%>%
          rename_all(~c('Best_response','Measurement.type','BOR','AE','p.value','difference',"LCL","UCL",
                        "meanDC","meanPD" ))%>%
          dplyr::mutate(Measurement.type=factor(Measurement.type,level=sort(names(table(Measurement.type)))))
        
        #THE PLOTS ####
        #  plot1<-tissue.test.ans.long%>%
        #    ggplot2::ggplot(ggplot2::aes(y=Measurement.type,x=difference,fill=p.value<0.05))+
        #    ggplot2::geom_bar(stat = 'identity')+
        #    #ggplot2::labs(title=paste(trial.id,names(a41)[j],sep='_'),fill='P value')+
        #    ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
        #    ggplot2::scale_fill_manual(values=c("cyan","red"),breaks=c(F,T),labels=c('>0.05', '<0.05'))+
        #    ggplot2::ylab('')+ggplot2::xlab('Difference (PD as reference)')+
        #    facet_wrap(vars(BOR))
        #  plot2<-tissue.test.ans.long%>%dplyr::filter(BOR%in%'DC_vs_PD')%>%
        #    ggplot2::ggplot(ggplot2::aes(y=Measurement.type,x=difference,fill=p.value<0.05))+
        #    ggplot2::geom_bar(stat = 'identity')+
        #    #ggplot2::labs(title=paste(trial.id,names(a41)[j],sep='_'),fill='P value')+
        #    ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
        #    ggplot2::scale_fill_manual(values=c("cyan","red"),breaks=c(F,T),labels=c('>0.05', '<0.05'))+
        #    ggplot2::ylab('')+ggplot2::xlab('Difference ( PD-CD )')
        #  
        df1<-tissue.test.ans.long
        
        # print("NAMES(tissue.test.ans.long)")
        # print(head(tissue.test.ans.long))
        # print(names(tissue.test.ans.long))
        # print(table(tissue.test.ans.long$Measurement.type))
        # 
        duration_data <- tissue.test.ans.long %>% filter(grepl("duration", Measurement.type)) #%>% mutate(cate="duration")
        fre_data <- tissue.test.ans.long %>% filter(grepl("fre", Measurement.type))#%>% mutate(cate="fre")
        occurrence_data <- tissue.test.ans.long %>% filter(grepl("occurrence", Measurement.type))#%>% mutate(cate="occurrence")
        sum_unique_AE_data <- tissue.test.ans.long %>% filter(grepl("sum.unique", Measurement.type))#%>% mutate(cate="sum.unique")
        
        # print("Split the dataset by AE.Type using filtering")
        # print(head(duration_data))
        # print(head(fre_data)) 
        # print(head(occurrence_data))
        # print(head(sum_unique_AE_data)) 
        
        
 if ( 1 == 1 )  {     # Split the dataset by AE.Type using filtering

         plot1duration <- duration_data %>% filter(!is.nan(p.value)) %>%
           ggplot2::ggplot( ggplot2::aes(y = Measurement.type, x = difference, col = p.value<0.05)) + #
           ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
           scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
           facet_wrap(vars(BOR)) +
           ggplot2::geom_point(shape = 18, size = 3) +
           geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
           ggplot2::xlab("Difference (PD as reference)") +
           ggplot2::ylab(" ") + 
           coord_cartesian(xlim = c(-10, 10) ) +
           ggplot2::labs(title =  "Forest Plot for duration" )
         
         plot1freq <- fre_data %>% filter(!is.nan(p.value)) %>%
           ggplot2::ggplot( ggplot2::aes(y = Measurement.type, x = difference, col = p.value<0.05)) + #
           ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
           scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
           facet_wrap(vars(BOR)) +
           ggplot2::geom_point(shape = 18, size = 3) +
           geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
           ggplot2::xlab("Difference (PD as reference)") +
           ggplot2::ylab(" ") + 
           coord_cartesian(xlim = c(-10, 10) )  +
           ggplot2::labs(title =  "Forest Plot for frequency" )
         
         plot1occurrence <- occurrence_data %>% filter(!is.nan(p.value)) %>%
           ggplot2::ggplot( ggplot2::aes(y = Measurement.type, x = difference, col = p.value<0.05)) + #
           ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
           scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
           facet_wrap(vars(BOR)) +
           ggplot2::geom_point(shape = 18, size = 3) +
           geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
           ggplot2::xlab("Difference (PD as reference)") +
           ggplot2::ylab(" ") + 
           coord_cartesian(xlim = c(-10, 10) )  +
           ggplot2::labs(title =  "Forest Plot for occurrence" )
         
         plot1sumunique <- sum_unique_AE_data %>% filter(!is.nan(p.value)) %>%
           ggplot2::ggplot( ggplot2::aes(y = Measurement.type, x = difference, col = p.value<0.05)) + #
           ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
           scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
           facet_wrap(vars(BOR)) +
           ggplot2::geom_point(shape = 18, size = 3) +
           geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
           
           ggplot2::xlab("Difference (PD as reference)") +
           ggplot2::ylab(" ") + 
           coord_cartesian(xlim = c(-10, 10) ) +
           ggplot2::labs(title =  "Forest Plot for sum of unique AEs" )
         
         
         
         library(patchwork)
         
         grid_plot <- (
                   plot1freq /
             plot1occurrence /
               plot1sumunique/
               plot1duration
         ) + plot_layout(heights = max(c(NROW(duration_data), NROW(fre_data),
                                         NROW(occurrence_data),NROW(sum_unique_AE_data))
         )
         )
         
         
         }
         
         
       #Original  
        plot1 <- tissue.test.ans.long %>% filter(!is.nan(p.value)) %>%
          ggplot2::ggplot( ggplot2::aes(y = Measurement.type, x = difference, col = p.value<0.05)) + #
          ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
          scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
          facet_wrap(vars(BOR)) +
          ggplot2::geom_point(shape = 18, size = 3) +
          geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
          
          ggplot2::xlab("Difference (PD as reference)") +
          ggplot2::ylab(" ") + 
          coord_cartesian(xlim = c(-10, 10) ) 
       
        # NEW 
        plot1 <- grid_plot
        
         
        plot2<-tissue.test.ans.long%>%dplyr::filter(BOR%in%'DC_vs_PD')%>% 
          filter(!is.nan(p.value)) %>%
          ggplot2::ggplot( ggplot2::aes(y = Measurement.type, x = difference, col = p.value<0.05)) + #
          ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
          scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
          ggplot2::geom_point(shape = 18, size = 3) +
          geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
          ggplot2::labs(title=paste("",names(a41)[j],sep=''),fill='P value')+
          ggplot2::scale_fill_manual(values=c("cyan","red"),breaks=c(F,T),labels=c('>0.05', '<0.05'))+
          ggplot2::ylab('')+ggplot2::xlab('Difference ( CD-PD )')+ 
          coord_cartesian(xlim = c(-20, 20) )
        
        
        AE.BOR.p.plot.list[[j]]<-list(all_comparison=plot1,DC_PD=plot2)
        coef.list.group[[j]]<-tissue.test.ans.long
      }
      
      names(AE.BOR.p.plot.list)<-names(coef.list.group)<-names(a41)
      
      twolists<-list(coef=coef.list.group,plot=AE.BOR.p.plot.list)
    }
    
     
    testing_BOR_ans<- responseplots(a41=a41now,adf=toxdataNOW)
    # print("testing_BOR_ans")
    # print(head(testing_BOR_ans))
    # print(tail(testing_BOR_ans))
    #save(testing_BOR_ans,file="testing_BOR_ans.RData")
    testing_BOR_ans

  })

  # Display t-test pvalues and differences ####
  output$responsettestoutput <- DT::renderDataTable({
    todisplay <-  tibble::as_tibble(do.call(rbind,testing_BOR_ans() $coef) %>%
                              dplyr::select(AE,everything(),-Best_response)) %>%
      dplyr::mutate(comparison = dplyr::case_when(BOR == "DC_vs_PD" ~ "(CR, PR, SD) vs PD",
                                    BOR == "PR_vs_PD" ~ "(CR, PR) vs PD",
                                    BOR == "SD_vs_PD" ~ "SD vs PD"),
             p.value=round(p.value,6),
             difference = round(difference,2))

    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 72))
  })

  responsetabledownloaddata <- shiny::reactive({

    todisplay <-  tibble::as_tibble(do.call(rbind,testing_BOR_ans() $coef) %>%
                              dplyr::select(AE,everything(),-Best_response)) %>%
      dplyr::mutate(comparison = dplyr::case_when(BOR == "DC_vs_PD" ~ "(CR, PR, SD) vs PD",
                                    BOR == "PR_vs_PD" ~ "(CR, PR) vs PD",
                                    BOR == "SD_vs_PD" ~ "SD vs PD"),
             p.value=round(p.value,6),
             difference = round(difference,2))

    todisplay
  })


  # Download handler for responsetabledownloaddata data #####
  {
    output$responsetabledownload <- shiny::downloadHandler(
      filename = function(){"Response_tests_results_download.csv"},
      content = function(fname){
        write.csv(responsetabledownloaddata(), fname, row.names = FALSE)
      }
    )
  }

  # TO CHO0SE PLOT TO DISPLAY
  output$rendRESPplotSelection <- shiny::renderUI({
    BPKPplot_list <-  testing_BOR_ans()$plot


    overallplots<-sapply(BPKPplot_list,"[[",1)
    DCPCplots<-sapply(BPKPplot_list,"[[",2)
    names(overallplots)<-names(testing_BOR_ans()$plot) #paste(names(testing_BOR_ans()$plot),"_allcomparison",sep="")
    names(DCPCplots) <- names(testing_BOR_ans()$plot)#paste(names(testing_BOR_ans()$plot),"_DC_PD",sep="")

    #print(getwd())
    # save(BPKPplot_list,file="BPKPplot_list.RData")

    plot_names <- names(overallplots)
    all_plot_names <- c()
    for (i in 1:length(plot_names) ) {
      all_plot_names[i] <- names(overallplots)[i]# c(all_plot_names,plot_names_sub)

    }

    # print("all_plot_names")
    # print(all_plot_names)
    # print(length(all_plot_names))
    shiny::selectInput("RESPplotSelection","Select Plot to View:", choices = all_plot_names)

  })
  # TO CH0OSE PLOT TO DISPLAY
  output$RESP_PlotOutput_all <- shiny::renderPlot({

    req(input$RESPplotSelection)
    RESPplot_list <- testing_BOR_ans()$plot
    RESP_Select <- input$RESPplotSelection
    # RESP_name1 <- strsplit(RESP_Select,"__")[[1]][1]
    # RESP_name2 <- strsplit(RESP_Select,"__")[[1]][2]
    plot <- RESPplot_list[[RESP_Select]][["all_comparison"]]
    plot

  })

  output$RESP_PlotOutput_DC_PD <- shiny::renderPlot({

    req(input$RESPplotSelection)
    RESPplot_list <- testing_BOR_ans()$plot
    RESP_Select <- input$RESPplotSelection
    # RESP_name1 <- strsplit(RESP_Select,"__")[[1]][1]
    # RESP_name2 <- strsplit(RESP_Select,"__")[[1]][2]
    plot <- RESPplot_list[[RESP_Select]][["DC_PD"]]
    plot

  })



  #  plots of responses ###
  draw_responseplots <- function() {  testing_BOR_ans()$plot   }

  # render draw_responseplots ###
  output$plotall_responseplots <- shiny::renderPlot({   draw_responseplots() })

  # Download response PLOTS pdf ####
  output$download_responseplots <- shiny::downloadHandler(
    filename = "Response_plots_report.pdf",

    # content = function(file) {
    #
    #
    #   sysname <- Sys.info()[1]
    #   if (sysname == "Windows") {
    #     filetorender <-  "template_responseplots.Rmd"
    #   } else {
    #     filetorender <- "/var/shiny-server/data/ThompsZJ/newAPPtesting/template_responseplots.Rmd"
    #   }
    #
    #   res <- rmarkdown::render(
    #     input = filetorender , # "template_responseplots.Rmd",
    #     params = list(
    #       draw_plot = draw_responseplots
    #     )
    #   )
    #
    #
    #   if (sysname == "Windows") {
    #     file.rename(res, file)
    #   } else {
    #     file.copy(res, file)
    #   }
    #
    #
    # }


    content = function(file) {
      res <- rmarkdown::render(
        input = "template_responseplots.Rmd",
        params = list(
          draw_responseplots = draw_responseplots
        )
      )

      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }

    }
  )
  # end: Response testz plots       ^^^^^^^^^^^^^^^^####

  # start: CORRELATION tab VVVVVVVVVVVVVVVV####
  # TABLE OF SIGNIFICANT FINDINGS  ? or DURATION? ###
  durationanalysis   <- shiny::eventReactive(input$goCorrelationtests,{

    toxdataNOW <- toxicity.whole.summary.data()
    a0<-toxdataNOW
    cor.AE.treatment.time.ans<-cor.data<-list()

    a1<-alldataoutput()
    a0<-a0%>%dplyr::mutate(treatment.time=as.numeric(treatment.time))
    # 3694 DO I CHANGE  THIS the AE.VARS??####
    # sink(file="C:\\Users\\thompszj\\Desktop\\sinktest.txt")
    # print("----line 3696----COR TAB------------------>names(a0)[3:20]... no ALL OF THEM")
    # print(names(a0))
    # sink()
    # AE.var=names(a0)[3:20]
    # a0=a0%>%dplyr::select(-c(3:20))
    
    #dplyr::left_join(a41[[j]],a4.whole.summary[,c(2,match("followup_start_date",names(a4.whole.summary)):dim(a4.whole.summary)[2])],by="pid")
    # print("--------------matching for the correlation data--------------------")
    # print(match("followup_start_date",names(a0)))
    # 
    AE.var=names(a0)[3:(match("followup_start_date",names(a0))-1)]
    # print("----------------AE.var")
    # print(AE.var)
    a0=a0%>%dplyr::select(-c(3:(match("followup_start_date",names(a0))-1)))
    # print("----line 3710----COR TAB------------------>names(a0) .. no ALL OF THEM")
    # print(names(a0))
    a400<-list(whole=alldataoutput()$toxicity.whole.summary.data) #FROM 616

    a400<-c(a400,
            alldataoutput()$toxicity.category.summary.data,
            alldataoutput()$toxicity.type.summary.data)
    a41<-a400
    
    # sink("C:\\Users\\thompszj\\Desktop\\names41.txt")
    # print(names(a41))
    # print(AE.var)
    # sink()
    
    cor.plot.AE.treatment.time.test<-cor.plot.AE.treatment.time.cat <-cor.plot.AE.treatment.time<-list()
    for(i in 1:length(a41))
    { #print("-----------------------names(a41[[i]])");print(names(a41[[i]]));
      #tmp1<-dplyr::left_join(a0,a41[[i]])%>%dplyr::select(pid,treatment.time,all_of(AE.var))
      # print("names(dplyr::left_join(a0,a41[[i]],by= pid ))")
      # print(names(dplyr::left_join(a0,a41[[i]],by="pid")))
      # WHAT ABOUT HERE???
      tmp1<-dplyr::left_join(a0,a41[[i]],by="pid")%>%
        dplyr::select(pid,treatment.time,all_of(AE.var))
      #cor1<-cor(tmp1[,c('treatment.time',AE.var)],method = 'pearson', use = "complete.obs")[1,]
      
      # if ( i == 1){
      #   
      #  # save(tmp1,file="F:\\myGitRepo\\aeshinyapp\\tmp1.RData")
      #   
      #   sink("C:\\Users\\thompszj\\Desktop\\WHOLEtmp1_20241231.txt")
      #   print(names(a41[[i]]))
      #   print(names(a41[i]))
      #   print(str(tmp1)) 
      #   print(tmp1[1:17])
      #   print(tmp1[18:33])
      # sink()}
      # if ( i == 6){tmp1eye<-tmp1
      # #  save(tmp1eye,file="F:\\myGitRepo\\aeshinyapp\\tmp1eye.RData")
      #   
      #   sink("C:\\Users\\thompszj\\Desktop\\EYEtmp1_20241231.txt")
      #   print(names(a41[[i]]))
      #   print(names(a41[i]))
      #   print(str(tmp1))
      #   
      #   print(tmp1[1:17])
      #   print(tmp1[18:33])
      #   sink()}
      
     #  print("NCOL tmp1 for cor:::")
     # print(NCOL(tmp1))
      
      suppressWarnings(cor1<-cor(tmp1[,c('treatment.time',AE.var)],method = 'pearson', 
                                 use = "complete.obs")[1,]
      )
       cor1<-data.frame(AE.measurement.type=names(cor1)[-1],r=round(cor1[-1],3))
       
       #print(cor1)
      #print(head(cor1))
      treatment.time<-tmp1$treatment.time
      cor1$pvalue<-apply(tmp1[,AE.var],2,function(x){
        
        # Extract the variable name
        # Compare x to each column in tmp1[, AE.var] and generate a logical vector
        match_vector <-    unname(apply(tmp1,2, function(col){
          # print(unname(unlist(as.vector(as.numeric(col)))));
          # print(x);
          # print(all(unname(unlist(as.vector(as.numeric(col))))==x))
          return(all(unname(unlist(as.vector(as.numeric(col))))==x))
        }))
         
        # Extract the column name corresponding to the TRUE value
        variable_name <- names(tmp1 )[match_vector]
         
        # print(names(x))
        # print("NCOL tmp1 for cor:::")
        # print(NCOL(tmp1))
        if( inherits(
          try(suppressWarnings(cor.test(x,treatment.time,method='pearson')$p.value)
              , silent = T)
          , "try-error",T)
          
        ){ p<-NA } else{
          
          cortestdata<-suppressWarnings(cor.test(x,treatment.time,method='pearson'))
          # print(names(cortestdata))
          #print(data.frame(x,treatment.time))
          
          #print(cortestdata)
          # print(cortestdata$conf.int)
         p<- suppressWarnings(round(cor.test(x,treatment.time,method='pearson')$p.value,5))
        }
      
        # if ( i == 1){sink("C:\\Users\\thompszj\\Desktop\\WHOLEloop_20241231.txt", append = TRUE)
        #   cat("\nProcessing variable:", variable_name, "\n")
        #   print(data.frame(x,treatment.time))
        #   
        #   print(cortestdata)
        #   sink()}
        # 
        # if ( i == 6){sink("C:\\Users\\thompszj\\Desktop\\EYEloop_20241231.txt", append = TRUE)
        #   cat("\nProcessing variable:", variable_name, "\n")
        #   print(data.frame(x,treatment.time))
        #   
        #   print(cortestdata)
        #   sink()}
        return(p)
        }

      )#end apply
      
      
      # if ( i == 1){sink("C:\\Users\\thompszj\\Desktop\\WHOLEcor1pvalue_20241231.txt")
      #   print(cor1)
      #   sink()}
      # 
      # if ( i == 6){sink("C:\\Users\\thompszj\\Desktop\\EYEcor1pvalue_20241231.txt")
      #   print(cor1)
      #   sink()}
      # 
      
      
      cor1$LCL<-apply(tmp1[,AE.var],2,function(x){
        if( inherits(
          try(suppressWarnings(cor.test(x,treatment.time,method='pearson')$p.value)
              , silent = T)
          , "try-error",T)
          
        ){ NA } else{
          
          cortestdata<-suppressWarnings(cor.test(x,treatment.time,method='pearson'))
         
          round(cortestdata$conf.int[1],3)
        }
      }
      )#end apply 
      
      cor1$UCL<-apply(tmp1[,AE.var],2,function(x){
        
        if( inherits(
          try(suppressWarnings(cor.test(x,treatment.time,method='pearson')$p.value)
              , silent = T)
          , "try-error",T)
          
        ){ NA } else{
          
          cortestdata<-suppressWarnings(cor.test(x,treatment.time,method='pearson'))
          
          round(cortestdata$conf.int[2],3)
          #round(cor.test(x,treatment.time,method='pearson')$p.value,5)
        }
      }
      )#end apply
      
      
      plot1<-cor1%>%ggplot2::ggplot(ggplot2::aes(y=AE.measurement.type,x=r,col=pvalue<0.05))+
        ggplot2::scale_fill_manual(values=c("cyan","red"),breaks=c(F,T),labels=c('>0.05', '<0.05'))+
        ggplot2::geom_point(shape = 18, size = 3) +
        geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
        coord_cartesian(xlim = c(-1.1, 1.1) ) +
        ggplot2::xlab('correlation coefficient (r)')+
        ggplot2::ylab('')+
        ggplot2::labs(title=names(a41)[i],fill='P value')
   
      
      cor.plot.AE.treatment.time[[i]]<-plot1
      cor.data[[i]]<-cor1
      
     # save(cor1,file=paste(names(a41)[i],"cor1dataset.RData",sep="-"))
      
      # Split the cor1 dataset by AE.Type using filtering
      duration_data <- cor1 %>% filter(grepl("duration", AE.measurement.type)) %>% mutate(cate="duration")
      fre_data <- cor1 %>% filter(grepl("fre", AE.measurement.type))%>% mutate(cate="fre")
      occurrence_data <- cor1 %>% filter(grepl("occurrence", AE.measurement.type))%>% mutate(cate="occurrence")
     
      # print("THE DIMENSIONS !@#$%^&$@#!@#^&#%$!@%#^$@#!@%#^#$!@%#^$&^%@#$%^$#@")
      # print(dim(duration_data))
      # print(dim(fre_data))
      # print(dim(occurrence_data)) 
      cor1o<-cor1
      # print("dim(cor1o")
      # print(dim(cor1o))
      cor1v2<- rbind(duration_data,fre_data,occurrence_data)
      # print("dim(cor1v2)")
      # print(dim(cor1v2))
      # Initialize an empty list to store plots
      #AE.survival.p.forestplot.list <- list()
      #cor.plot.AE.treatment.time.test<-cor.plot.AE.treatment.time.cat <- list()
      
      # Define the AE.type categories
      categories <- c("duration", "fre", "occurrence")# "sum_unique_AE")
      # GGPLOT CORR !!!!!!!!####
      # Loop through each AE.type category and generate a forest plot
      for (cat in categories) {
        # Filter the data for the current AE.Type
        cor1v2_filtered <- cor1v2 %>% filter(cate == cat)
        
        catlabel <- ifelse(cat=="fre","frequency",cat)
        # print("names(cor1v2_filtered)----------------------------------")
        # print(names(cor1v2_filtered))
        # print(dim(cor1v2_filtered)) 
        # print(cat)
        # Create the forest plot for this category
        cor.plot.AE.treatment.time.cat[[cat]] <- cor1v2_filtered%>%
          ggplot2::ggplot(ggplot2::aes(y=AE.measurement.type,x=r,col=pvalue<0.05))+
          #ggplot2::scale_fill_manual(values=c("cyan","red"),breaks=c(F,T),labels=c('>0.05', '<0.05'))+
          scale_colour_manual(values=c("red","cyan"),breaks=c(T,F),labels=c('<0.05', '>0.05')) +
          
          ggplot2::geom_point(shape = 18, size = 3) +
          geom_errorbarh(ggplot2::aes(xmin = LCL, xmax = UCL), height = 0.25) +
          coord_cartesian(xlim = c(-1.1, 1.1) ) +
          ggplot2::xlab('correlation coefficient (r)')+
          ggplot2::ylab('')+
          ggplot2::labs(title= paste("Forest Plot for", catlabel ,"AEs: ",names(a41)[i]),fill='P value')+                     # Scatter plot
          geom_vline(xintercept = 0,         # Add vertical line at x = 0
                     color = "black",          # Line color
                     linetype = "dashed",    # Line type
                     size = 1.2)
        
        
   #     cor.plot.AE.treatment.time.cat[[i]]<-plot1
         
        
        
      }
      
      library(patchwork)
      
      # Combine the plots into a 2x2 grid
      grid_plot <- (
          cor.plot.AE.treatment.time.cat[["duration"]] +
          cor.plot.AE.treatment.time.cat[["fre"]] +
          cor.plot.AE.treatment.time.cat[["occurrence"]]  
      ) + plot_layout(ncol = 1, nrow = 3)
      
      
      
        
      
      # Save the grid plot in the list
      cor.plot.AE.treatment.time.test[[i]] <- grid_plot
    }
    #sink()
    
    
    
    names(cor.plot.AE.treatment.time.test) <-names(cor.plot.AE.treatment.time)<-names(cor.data)<-names(a41)
    cor.AE.treatment.time.ans<-list(cor=cor.data,plot=cor.plot.AE.treatment.time)
   # sink("C:\\Users\\thompszj\\Desktop\\corrleationtesting2_20241230.txt")
   # print(cor.AE.treatment.time.ans)
   # sink()
    a2=cor.AE.treatment.time.ans$cor
    a3=sapply(a2[sapply(a2,length)>0],function(x) x%>%
                dplyr::rename(p=pvalue, AE.type=AE.measurement.type)%>%
                dplyr::filter((p<0.05)),simplify = F)

    a4=sapply(a3,dim)[1,]
    a31=a3[a4!=0]

    a5=sapply(a31,function(x)
    {
      x=x%>%dplyr::mutate(group=paste(factor(p<0.05,level=c(T,F),label=c('Sig','NS')),
                               factor(r>0,level=c(T,F),label=c('Pos_Cor','Neg_Cor')),sep='_'))
      tapply(as.vector(x$AE.type),x$group,sort)
    },simplify = F
    )

    fun.AE.summary<-function(x)
    {
      paste(gsub('NA','',gsub('HG','High-Grade',gsub('LG','Low-Grade',gsub('treatment\\.related','Trt',x)))),collapse='/')
    }

    fun.AE.paste<-function(x)
    {
      index1<-grep('treatment',x);
      if(length(index1)>0) x<-x[-index1]
      x
    }

    a6<-sapply(a5,function(y) as.vector(sapply(y,function(x) names(table(sub('\\.sum\\.unique','',sub('\\.sum.unique','',sub('\\.occurrence','',sub('\\.fre','',sub('\\.duration','',x))))))),simplify = F)),simplify = F)

    a6.trt<-sapply(a6,function(y) unlist(sapply(y,function(x) {index1<-grep('treatment',x); if(length(index1)==0) NULL else
    {
      x<-x[index1];paste(x,collapse='/')
    }})))
    a6.trt<-a6.trt[!sapply(a6.trt,is.null)]
    a6.trt<-sapply(a6.trt,function(y) sapply(y,fun.AE.summary,simplify = F))

    a6.no_trt<-sapply(a6,function(y) unlist(sapply(y,function(x) {index1<-grep('treatment',x); if(length(index1)>0) x<-x[-index1];paste(x,collapse='/')})))
    a6.no_trt<-sapply(a6.no_trt,function(y) sapply(y,fun.AE.summary,simplify = F))

    a7<-sapply(a6,function(y) sapply(y,function(x) paste(sub('HG','High-Grade',sub('LG','Low-Grade',sub('treatment\\.related','Trt',x))),collapse='/')),simplify = F)

    name1<-c("Sig_Neg_Cor", "Sig_Pos_Cor")
    name2<-c("Negative Correlation","Positive Correlation")

    fun.group<-function(x,name.ref1=name1,name.ref2=name2)
    {
      name.tmp1<-names(x)
      index1<-name.ref1%in%name.tmp1
      if(sum(index1)<length(name.ref1)) {
        name.null<-rep('',length(name.ref1[!index1]))
        names(name.null)<-name.ref1[!index1]
        x<-c(x,name.null)
      }
      x<-x[match(name.ref1,names(x))]
      names(x)<-name.ref2
      x
    }

    table.AE.Response.sig.summary<-t(sapply(a7,function(x) fun.group(unlist(x))))

    q1=table.AE.Response.sig.summary
    q11=data.frame(AE=rownames(q1),q1)

    #---table of ind AE by AE category

    q2=table(AE_data()$cdus_toxicity_type_code,AE_data()$toxicity_category)# problem for MCC 18597
    AE_data<-AE_data()
     
    #---get ind AE under AE category
    q3=q2[rownames(q2)%in%rownames(q1)[!rownames(q1)%in%c('whole',colnames(q2))],,drop=F]
    name1=rownames(q3)
    q31<-q3[,apply(q3,2,sum)!=0,drop=F]

    name2=colnames(q31)
    q5=apply(q31,1,function(x) name2[x>0])
    q6=data.frame(cate.AE=q5,AE=names(q5))
    q7=full_join(q6,q11)
    q7$cate.AE[(1:dim(q7)[1])[is.na(q7$cate.AE)]]<-q7$AE[(1:dim(q7)[1])[is.na(q7$cate.AE)]]
    q8=q7[order(q7$cate.AE),]
    q8$AE[q8$AE==q8$cate.AE]<-NA
    q8=q8[order(q8$cate.AE,q8$AE,na.last = F),]
    index1<-grep('whole',q8$cate.AE)
    if(length(index1)>0) q8<-q8[c(index1,(1:dim(q8)[1])[-index1]),]
    q8$AE[is.na(q8$AE)]<-''

    list(q8=q8,plot=cor.AE.treatment.time.ans,gridplots=cor.plot.AE.treatment.time.test)

  })
  # Displaydurationanalysis table ####
  output$durationanalysistableoutput <- DT::renderDataTable({
    todisplay <- durationanalysis()$q8
    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 100))

  })
  correlationtabledownloaddata <- shiny::reactive({

    todisplay <- durationanalysis()$q8

    todisplay
  })
  # Download handler for correlationtabledownloaddata data #####
  {
    output$correlationtabledownload <- shiny::downloadHandler(
      filename = function(){"Correlation_tests_results_download.csv"},
      content = function(fname){
        write.csv(correlationtabledownloaddata(), fname, row.names = FALSE)
      }
    )
  }
  # Display numerical correlation results ###
  output$cornumericalresults <- DT::renderDataTable({
    CORplot_list <-  durationanalysis()$plot
    todisplay <-  do.call("rbind",CORplot_list$cor)
    todisplay <- todisplay %>% mutate(AEcategory =  sub("\\..*", "", rownames(todisplay))) %>%
      select(AEcategory, AE.measurement.type,r , pvalue,LCL,UCL)
    
    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 72))

  })
  numericcorrelationtabledownloaddata <- shiny::reactive({

    CORplot_list <-  durationanalysis()$plot
    todisplay <-  do.call("rbind",CORplot_list$cor)

    todisplay
  })
  # Download handler for correlationtabledownloaddata data #####
  {
    output$numericcorrelationtabledownload <- shiny::downloadHandler(
      filename = function(){"Numerical_Correlation_tests_results_download.csv"},
      content = function(fname){
        write.csv(numericcorrelationtabledownloaddata(), fname, row.names = FALSE)
      }
    )
  }

   # TO CHO0SE PLOT TO DISPLAY
  output$rendCORplotSelection <- shiny::renderUI({
    # CORplot_list <-  durationanalysis()$plot
     CORplot_list <-  durationanalysis()$gridplots
    #save(CORplot_list,file="CORplot_list_all.RData")
    # CORplot_list <- CORplot_list$plot
     #CORplot_list <- CORplot_list$gridplots
    # save(CORplot_list,file="CORplot_list.RData")
    # print("str(CORplot_list,1,0)")
    # print(str(CORplot_list,1,0))
    # print(length(CORplot_list))
    # print(names(CORplot_list))
    #overallplots<-sapply(CORplot_list,"[[",1)
    # DCPCplots<-sapply(BPKPplot_list,"[[",2)
    # names(overallplots)<-names(testing_BOR_ans()$plot) #paste(names(testing_BOR_ans()$plot),"_allcomparison",sep="")
    # names(DCPCplots) <- names(testing_BOR_ans()$plot)#paste(names(testing_BOR_ans()$plot),"_DC_PD",sep="")


    plot_names <- names(CORplot_list)#names(overallplots)
    print(plot_names)
    all_plot_names <- c()
    for (i in 1:length(plot_names) ) {
      all_plot_names[i] <- names(CORplot_list)[i]# c(all_plot_names,plot_names_sub)

    }

    # print("all_plot_names")
    # print(all_plot_names)
    # print(length(all_plot_names))
    shiny::selectInput("CORplotSelection","Select Plot to View:", choices = all_plot_names)

  })
  # TO CH0OSE PLOT TO DISPLAY
  output$COR_PlotOutput_all <- shiny::renderPlot({

    req(input$CORplotSelection)
    # CORplot_list <-  durationanalysis()$plot
     CORplot_list <-  durationanalysis()$gridplots
     
   # CORplot_list <- CORplot_list$plot
    COR_Select <- input$CORplotSelection
    # RESP_name1 <- strsplit(RESP_Select,"__")[[1]][1]
    # RESP_name2 <- strsplit(RESP_Select,"__")[[1]][2]
    plot <- CORplot_list[[COR_Select]]#[["all_comparison"]]
    plot

  })


  # Cor plots ###
  # draw_plot_duration <- function() {  durationanalysis()$plot  }
  draw_plot_duration <- function() {  durationanalysis()$gridplots  }
  
  # render correlation plots ###
  output$plotduration <- shiny::renderPlot({  draw_plot_duration() })

  # Download correlation plots pdf ###
  output$downloaddurationplotspdf <- shiny::downloadHandler(
    filename = "duration_rendered_report.pdf",

    # content = function(file) {
    #
    #
    #   sysname <- Sys.info()[1]
    #   if (sysname == "Windows") {
    #     filetorender <-  "templateduration.Rmd"
    #   } else {
    #     filetorender <- "/var/shiny-server/data/ThompsZJ/newAPPtesting/templateduration.Rmd"
    #   }
    #
    #   res <- rmarkdown::render(
    #     input = filetorender , # "template.Rmd",
    #     params = list(
    #       draw_plot = draw_plot_duration
    #     )
    #   )
    #
    #
    #   if (sysname == "Windows") {
    #     file.rename(res, file)
    #   } else {
    #     file.copy(res, file)
    #   }
    #
    #
    # }



    content = function(file) {
      res <- rmarkdown::render(
        input = "templateduration.Rmd",
        params = list(
          draw_plot_duration = draw_plot_duration
        )
      )


      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }




    }
  )
  # end: correlation DURATION plots        ^^^^^^^^^####



  # start: survival summary tab VVVVVVVVVVVVVVVV####
  survivialtempout <- shiny::reactive({

    #sink("C:\\Users\\thompszj\\Desktop\\debugging_20221228.txt")
    fun1<-function(my.data.list=coef.list)
    {
      surv.name<-c('os','pfs')
      e2.comb<-numeric()
      #print("my.data.list")
      #print(my.data.list)
      for(i in 1:length(surv.name))
      {

        e1=sapply(my.data.list,function(x) x[[surv.name[i]]][,c(2,5)],simplify=F)
        #print("e1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        #print(e1)
        e2<-data.frame(do.call("rbind",e1))
        dim.index<-dim(e2[!is.na(e2[,2]),])[1] #--ensure not all NA--
        if(dim.index>0)
        {
          e2<-e2 %>%  tibble::rownames_to_column(var='data.type') %>% 
            #dplyr::add_rownames(var='data.type')%>%
            dplyr::mutate(survival=surv.name[i],
                   AE.type=rep(names(my.data.list),each=2),
                   data.type=sub('y.*','Continuous',sub('AE.*','Occurrence',data.type)))%>%
            dplyr::relocate(AE.type)
          names(e2)[3:4]<-c('HR','p')
          e21<-e2%>%dplyr::filter(data.type %in%'Occurrence')%>%dplyr::slice(dplyr::ends_with('occurrence',vars=AE.type))
          e22<-e2%>%dplyr::filter(data.type %in%'Continuous')%>%dplyr::slice(dplyr::ends_with('occurrence',vars=AE.type))%>%
            dplyr::mutate(AE.type=sub('occurrence','sum.unique',AE.type))
          e23<-e2%>%dplyr::filter(data.type %in%'Continuous')%>%dplyr::slice(-dplyr::ends_with('occurrence',vars=AE.type))
          e2.comb<-rbind(e2.comb,rbind(e21,e22,e23))
        }
      }
      e2.comb
    }

    a1=responsePvaluelistforestplots()$coef

    #save(a1,file="C:\\Users\\thompszj\\Desktop\\a1.RData")

    a2<-sapply(a1,fun1,simplify = F)


    a3=sapply(a2[sapply(a2,length)>0],function(x) x%>%dplyr::filter(p<0.05),simplify = F)

    a4=sapply(a3,dim
              ,simplify=TRUE)[1,]# ,simplify=TRUE on 20221212


    a31=a3[a4!=0]


    a5=sapply(a31,function(x)
    {
      x=x%>%dplyr::mutate(group=paste(survival,factor(p<0.05,level=c(T,F),label=c('Sig','NS')),factor(HR>1,level=c(T,F),label=c('HR>1','HR<1')),sep='_'))
      tapply(as.vector(x$AE.type),x$group,sort)
    },simplify=F)



    fun.AE.summary<-function(x)
    {
      paste(gsub('NA','',gsub('HG','High-Grade',
                              gsub('LG','Low-Grade',
                                   gsub('treatment\\.related','Trt',x)))),collapse='/')
    }

    fun.AE.paste<-function(x)
    {
      index1<-grep('treatment',x);
      if(length(index1)>0) x<-x[-index1]
      x
    }




    a6<-sapply(a5,function(y) as.vector(sapply(y,function(x) names(table(sub('\\.sum.unique','',sub('\\.occurrence','',sub('\\.fre','',sub('\\.duration','',x)))))),simplify = F)),simplify=F)


    a6.trt<-sapply(a6,function(y) unlist(sapply(y,function(x) {index1<-grep('treatment',x); if(length(index1)==0) NULL else
    {
      x<-x[index1];paste(x,collapse='/')
    }},simplify=F)),simplify=F)


    a6.trt<-a6.trt[!sapply(a6.trt,is.null)]
    a6.trt<-sapply(a6.trt,function(y) sapply(y,fun.AE.summary,simplify = F),simplify=F)



    a6.no_trt<-sapply(a6,function(y) unlist(sapply(y,function(x) {index1<-grep('treatment',x); if(length(index1)>0) x<-x[-index1];paste(x,collapse='/')},simplify=F)),simplify = F)
    a6.no_trt<-sapply(a6.no_trt,function(y) sapply(y,fun.AE.summary,simplify = F),simplify = F)


    a7<-sapply(a6,function(y) sapply(y,function(x) paste(sub('HG','High-Grade',sub('LG','Low-Grade',sub('treatment\\.related','Trt',x))),collapse='/'),simplify = F),simplify = F)
    name1<-c("os_Sig_HR<1","os_Sig_HR>1", "pfs_Sig_HR<1", "pfs_Sig_HR>1")
    name2<-c("Improved OS","Poorer OS", "Improved PFS",
             "Poorer PFS")

    fun.group<-function(x,name.ref1=name1,name.ref2=name2)
    {
      name.tmp1<-names(x)
      index1<-name.ref1%in%name.tmp1
      if(sum(index1)<length(name.ref1)) {
        name.null<-rep('',length(name.ref1[!index1]))
        names(name.null)<-name.ref1[!index1]
        x<-c(x,name.null)
      }
      x<-x[match(name.ref1,names(x))]
      names(x)<-name.ref2
      x
    }



    table.AE.survival.All.sig.summary<-t(sapply(a7,function(x) fun.group(unlist(x)),simplify=TRUE))

    q1=table.AE.survival.All.sig.summary
    q11=data.frame(AE=rownames(q1),q1)

    #---table of ind AE by AE category

    #q2=table(toxicity.raw.data$cdus_ctcae_toxicity_type_code,toxicity.raw.data$toxicity_category)
    # if (( !"cdus_toxicity_type_code" %in% names(AE_data())) & ("toxicity" %in% names(AE_data()))){
    #   AE_data()$cdus_toxicity_type_code <-  AE_data()$toxicity
    # }
    #

    q2=table(AE_data()$cdus_toxicity_type_code,AE_data()$toxicity_category) # STill some proble,s MCC18597


    #---get ind AE under AE category
    q3=q2[rownames(q2)%in%rownames(q1)[!rownames(q1)%in%c('whole',colnames(q2))],]
    name1=rownames(q3)
    q31<-q3[,apply(q3,2,sum)!=0]

    name2=colnames(q31)
    #print("q31~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    #print(q31)
    q5=apply(q31,1,function(x) name2[x>0])
    #print("q5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    #print(q5)
    q6=data.frame(cate.AE=q5,AE=names(q5))
    #getwd()
    #save(a1,a31,a5,a6,a7,q1,q2,q3,q31,q5,q6,q11,file="datasets.RData")
    q7=full_join(q6,q11) #HERE NO VARIABLES ARE SPECIFIED THIS IS PART OF THE PROBLEM
    q7$cate.AE[(1:dim(q7)[1])[is.na(q7$cate.AE)]]<-q7$AE[(1:dim(q7)[1])[is.na(q7$cate.AE)]]
    q8=q7[order(q7$cate.AE),]
    q8$AE[q8$AE==q8$cate.AE]<-NA
    q8=q8[order(q8$cate.AE,q8$AE,na.last = F),]
    index1<-grep('whole',q8$cate.AE)
    if(length(index1)>0)q8<-q8[c(index1,(1:dim(q8)[1])[-index1]),]
    q8$AE[is.na(q8$AE)]<-''

    #sink()

    # save(a1,a31,a5,a6,a7,q1,q2,q3,q31,q5,q6,q7,q11,q8,
    #      file="F:\\myGitRepo\\datasetswithq7andq8.RData")
    q8

  })


  # Display t-test pvalues and differences ####
  output$survivalsummarytable <- DT::renderDataTable({
    todisplay <-  survivialtempout()
    showN<- NROW(todisplay)
    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 100))

  })

  # Down load toxicity.whole.summary.data()
  output$survival_summarydownload <- shiny::downloadHandler(
    filename = function(){"survival_summary.csv"},
    content = function(fname){
      write.csv(survivialtempout(), fname, row.names = FALSE)
    }
  )
  
  # FOR THE PLOT ####
  
  
  
  summaryplot <- shiny::reactive({
    
    
    heatmap_long <-data<-survivialtempout()%>%pivot_longer(cols = -(1:3),names_to = 'Outcome', values_to ='Association')%>%
      mutate(Association= na_if(Association, ""),
             outcome_short=sub('\\..*','',Outcome),
             var.new=ifelse(is.na(Association),NA,outcome_short),
             AE2=if_else(is.na(AE),"",AE),
             AE_Category=paste(cate.AE,"-",AE2,sep=" "),
             AE_Category=if_else(is.na(AE),gsub("-"," ",AE_Category),AE_Category))
    
    
    plot1 <- ggplot(heatmap_long, aes(x = Outcome, y = AE_Category, fill = var.new)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Association), size = 3, na.rm = TRUE) +
      scale_fill_manual(values = c("Improved" = "cornflowerblue",'Poorer'='brown1' ),na.value =  "white", guide = FALSE) +
      #  scale_color_manual(values = c("Improved.OS"='blue',  "Improved.PFS"='blue', "Poorer.OS"='red',    "Poorer.PFS"='red'), guide = FALSE) +
      theme_minimal() +
      labs(title = "AE Associations with OS and PFS", x = "Outcome", y = "AE Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    
    
  })
  
  
  
  
  
  output$summaryplot_out <- shiny::renderPlot({   print(summaryplot())    })
  
  
  
  
  
  
  # end: survival summary          ^^^^^^^^^####

  # start: response summary tab VVVVVVVVVVVVVVVV####
  responsetempout <- shiny::reactive({

    a2=testing_BOR_ans()$coef
    a3=sapply(a2[sapply(a2,length)>0],function(x) x%>%dplyr::rename(p=p.value, AE.type=Measurement.type)%>%dplyr::filter((p<0.05)&(BOR%in%'DC_vs_PD'))%>%dplyr::select(-Best_response),simplify = F)

    a4=sapply(a3,dim)[1,]
    a31=a3[a4!=0]

    a5=sapply(a31,function(x)
    {
      x=x%>%dplyr::mutate(group=paste(factor(p<0.05,level=c(T,F),label=c('Sig','NS')),factor(difference>0,level=c(T,F),label=c('DC>PD','DC<PD')),sep='_'))
      tapply(as.vector(x$AE.type),x$group,sort)
    },simplify = F
    )



    fun.AE.summary<-function(x)
    {
      paste(gsub('NA','',gsub('HG','High-Grade',gsub('LG','Low-Grade',gsub('treatment\\.related','Trt',x)))),collapse='/')
    }

    fun.AE.paste<-function(x)
    {
      index1<-grep('treatment',x);
      if(length(index1)>0) x<-x[-index1]
      x
    }


    a6<-sapply(a5,function(y) as.vector(sapply(y,function(x) names(table(sub('\\.sum\\.unique','',sub('\\.sum.unique','',sub('\\.occurrence','',sub('\\.fre','',sub('\\.duration','',x))))))),simplify = F)),simplify = F)

    a6.trt<-sapply(a6,function(y) unlist(sapply(y,function(x) {index1<-grep('treatment',x); if(length(index1)==0) NULL else
    {
      x<-x[index1];paste(x,collapse='/')
    }})))
    a6.trt<-a6.trt[!sapply(a6.trt,is.null)]
    a6.trt<-sapply(a6.trt,function(y) sapply(y,fun.AE.summary,simplify = F))

    a6.no_trt<-sapply(a6,function(y) unlist(sapply(y,function(x) {index1<-grep('treatment',x); if(length(index1)>0) x<-x[-index1];paste(x,collapse='/')})))
    a6.no_trt<-sapply(a6.no_trt,function(y) sapply(y,fun.AE.summary,simplify = F))

    a7<-sapply(a6,function(y) sapply(y,function(x) paste(sub('HG','High-Grade',sub('LG','Low-Grade',sub('treatment\\.related','Trt',x))),collapse='/')),simplify = F)

    name1<-c("Sig_DC<PD", "Sig_DC>PD")
    name2<-c("Associated with PD","Associated with DC")

    fun.group<-function(x,name.ref1=name1,name.ref2=name2)
    {
      name.tmp1<-names(x)
      index1<-name.ref1%in%name.tmp1
      if(sum(index1)<length(name.ref1)) {
        name.null<-rep('',length(name.ref1[!index1]))
        names(name.null)<-name.ref1[!index1]
        x<-c(x,name.null)
      }
      x<-x[match(name.ref1,names(x))]
      names(x)<-name.ref2
      x
    }

    table.AE.Response.sig.summary<-t(sapply(a7,function(x) fun.group(unlist(x))))

    q1=table.AE.Response.sig.summary
    q11=data.frame(AE=rownames(q1),q1)

    #---table of ind AE by AE category
    #q2=table(toxicity.raw.data$cdus_ctcae_toxicity_type_code,toxicity.raw.data$toxicity_category)
    # if (( !"cdus_toxicity_type_code" %in% names(AE_data())) & ("toxicity" %in% names(AE_data()))){
    #   AE_data()$cdus_toxicity_type_code <-  AE_data()$toxicity
    # }

    q2=table(AE_data()$cdus_toxicity_type_code,AE_data()$toxicity_category)

    #---get ind AE under AE category
    q3=q2[rownames(q2)%in%rownames(q1)[!rownames(q1)%in%c('whole',colnames(q2))],,drop=F]
    name1=rownames(q3)
    q31<-q3[,apply(q3,2,sum)!=0,drop=F]

    name2=colnames(q31)
    q5=apply(q31,1,function(x) name2[x>0])
    q6=data.frame(cate.AE=q5,AE=names(q5))
    q7=full_join(q6,q11)
    q7$cate.AE[(1:dim(q7)[1])[is.na(q7$cate.AE)]]<-q7$AE[(1:dim(q7)[1])[is.na(q7$cate.AE)]]
    q8=q7[order(q7$cate.AE),]
    q8$AE[q8$AE==q8$cate.AE]<-NA
    q8=q8[order(q8$cate.AE,q8$AE,na.last = F),]
    index1<-grep('whole',q8$cate.AE)
    q8<-q8[c(index1,(1:dim(q8)[1])[-index1]),]
    q8$AE[is.na(q8$AE)]<-''
    q8
  })


  # Display t-test pvalues and differences ####
  output$responsesummarytable <- DT::renderDataTable({
    todisplay <-  responsetempout()
    showN<- NROW(todisplay)
    DT::datatable(todisplay, rownames = FALSE, options = list(pageLength = 100))

  })

  # Down load response summary
  output$response_summarydownload <- shiny::downloadHandler(
    filename = function(){"response_summary.csv"},
    content = function(fname){
      write.csv(responsetempout(), fname, row.names = FALSE)
    }
  )

  # end: response summary          ^^^^^^^^^####

  # start: summary report VVVVVVVVVVVVV#####

  output$downloadsumaryReport <- shiny::downloadHandler(

    filename = function() {
      filename = "summary_report.pdf"
    },


    content = function(file) {

      res <- rmarkdown::render(input = 'summaryreport.Rmd',

                               output_format = rmarkdown::pdf_document(),
 )


      sysname <- Sys.info()[1]
      if (sysname == "Windows") {
        file.rename(res, file)
      } else {
        file.copy(res, file)
      }


    }

  )

  # end: summary report tab^^^^^^^^^^^^^####

}) # end server ####
