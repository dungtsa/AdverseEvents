

# Increase uploaded file size to 30MB
options(shiny.maxRequestSize = 50*1024^2)

function(request){
  shiny::fluidPage(
    title = "AE plotting and calculation of days with AEs",
    # theme = shinytheme("spacelab"),
    # shinythemes::themeSelector(),
    shinyjs::useShinyjs(),
    htmltools::tags$head(
      htmltools::tags$head(htmltools::includeCSS("www/style.css")),
      #htmltools::tags$link(href = "www/style.css", rel = "stylesheet")
      #htmltools::tags$link(href = here::here("inst","app","www","style.css"),
      #          rel = "stylesheet")
    ),
    htmltools::div(id = "header",
        htmltools::div(id = "title",
            "Analysis of Adverse Events"
        )
        # ,
        # htmltools::div(id = "subtitle",
        #     "Adverse Events"),
        # htmltools::div(id = "subsubtitle",
        #     "By:",
        #     htmltools::tags$a(href = "mailto:zacha@moffitt.org", "Ram Thapa")
        # )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 2,
        class = "col-settings",

        shiny::fileInput("data_file", "Select AE file"),
        shiny::helpText("File size limited to 50 MB"),
        shiny::fileInput("demographics_file", "Select demographics file"),
        shiny::helpText("File size limited to 50 MB"),
        shiny::fileInput("fu_file", "Select followup file"),
        shiny::helpText("File size limited to 50 MB"),
        shiny::fileInput("da_file", "Select Drug administration file"),
        shiny::helpText("File size limited to 50 MB"),
        shiny::fileInput("re_file", "Select RECIST file (Optional)"),
        shiny::helpText("File size limited to 50 MB"),

        shiny::bookmarkButton()
      ),

      # tabs to show raw datasets ####
      shiny::column(
        width = 9,
        class = "col-tabs",


        shiny::tabsetPanel( id = "panel", type = "tabs",

                            shiny::tabPanel("Data",
                              #
                              # htmltools::br(),
                              htmltools::p("This tab panel shows the raw input data and the toxicity data which is the demographics
               and AE data merged with AE time calculated. "),
               htmltools::br(),
               shiny::tabsetPanel(id = "data_panel",
                                  shiny::tabPanel(
                             htmltools::div(shiny::icon("database"), "AE data"),
                             DT::dataTableOutput("selected_data")  
                           ),  # tab to show raw data
                           shiny::tabPanel(
                             htmltools::div(shiny::icon("database"), "Demographic data"),
                             DT::dataTableOutput("demographics_data")
                           ),  # tab to show raw data
                           shiny::tabPanel(
                             htmltools::div(shiny::icon("database"), "Follow up data"),
                             DT::dataTableOutput("followup_data")
                           ), # tab to show raw data

                           shiny::tabPanel(
                             htmltools::div(shiny::icon("database"), "Drug administration data"),
                             DT::dataTableOutput("drug_data")
                           ),
                           shiny::tabPanel(
                             htmltools::div(shiny::icon("database"), "Toxicity data"),
                             DT::dataTableOutput("tox_data")
                           ),
                           shiny::tabPanel(htmltools::div(shiny::icon("database"), "RECIST data"),
                             span(textOutput("RECIST_Data_Message1"), style="color:red"),
                             DT::dataTableOutput("re_data"))

               )) #END tabPanel("Data", tabsetPanel
               ,

               #  ,



               # AE measures and plots
               # AE Plots and Measures ####
               shiny::tabPanel("AE Plots and Measures",
                        # htmltools::br(),
                        htmltools::p("This tab panel contains the AE measures sub-tab which is used to calculate
                                     AE metrics used in all other tabs for analysis. Please click Compute AE measures button before any analysis.")
      ,    
      
      htmltools::p("This tab panel also contains swimmers plots and RECIST plots of adverse events and tables of AEs."),
      htmltools::p("It also provides download buttons to save the plots and data. ")
      ,
      htmltools::br(),
      # AE measures tab ####
    
      
      shiny::tabsetPanel(id = "plot_panel",
                         shiny::tabPanel(
                           htmltools::div(shiny::icon("database"), "AE measures"),
                           # htmltools::br(),
                           # htmltools::br(),
                           htmltools::p("This sub-tab contains the AE data with the calculated metrics used in all other tabs for analysis.
               Leave 'Early AE time point box' blank for all the data. Category can also be chosen, but may lead to sparse data and
               the inability to do any analysis. Please click the 'Compute AE measures' button.")
               ,
               shiny::fluidRow(
                 shiny::column(3,
                               htmltools::div( shiny::actionButton("goAEmeasures", "Compute AE measures" ) )
                 ),
                 shiny::column(2,
                               shiny::numericInput("AEmeasuresEarlyAEcut","Early AE Time Point:", value = NULL, step = 1, min = 1)
                 ),
                 shiny::column(3,
                               shiny::uiOutput("rendAEmeasuresAEcatselect")
                 ),
                 shiny::column(3,
                               shiny::uiOutput("rendAEmeasuresAEtypeselect")
                 ),
                 
                 shiny::column(2,
                               htmltools::div(shiny::downloadButton('toxicitymeasuresdownload',"Download the data"))
                 )
                 
               ),
               #htmltools::div(downloadButton('toxicitymeasuresdownload',"Download the data"),
               htmltools::br(),
               htmltools::br(),
               htmltools::br(),
               shinycssloaders::withSpinner(DT::dataTableOutput("toxicitytableoutput"))
               #)
                         ),
               
               

               shiny::tabPanel(
                    htmltools::div(shiny::icon("chart-line"), "AE individual swimmers plot and table"),
                    htmltools::p("This sub-tab displays individual swimmers plot and AE table by grade.   ")
                    ,
                    htmltools::div(
                      style = "position:relative",
                      shiny::fluidRow(
                        shiny::column(2,
                                      shiny::selectizeInput("single_var_input", "Patient number:", choices = NULL,
                                              options = list(placeholder = "Select a patient",
                                                             onInitialize = I('function() { this.setValue(""); }')))
                        ),
                        shiny::column(2,
                                      shiny::numericInput("AEplot_EarlyAECut","Early AE Time Point:", value = NULL, step = 1, min = 1)
                        ),
                        shiny::column(3,
                                      shiny::checkboxInput("AEplot_ShowTime","Display Time Annotation", value = T)
                        )
                      ),

                      htmltools::br(),
                      shiny::fluidRow(
                        shiny::column(1,
                                      shiny::downloadButton('download_single_var_plot_early',"Save plot")
                        ),
                        shiny::column(2,
                                      shiny::actionButton("runAllAEplots","Generate All AE Plots")
                        ),
                        shiny::column(1,
                                      shiny::uiOutput("renddownload_ALL_var_plot_early")
                        )
                      ),
                      htmltools::br(),
                      shinycssloaders::withSpinner(shiny::plotOutput("single_var_plot_early", width = "100%", height = "500px")),
                      htmltools::br(),
                      shiny::downloadButton('AEcounttableinddownload',"Download the data"),
                      htmltools::br(),
                      shinycssloaders::withSpinner(  DT::dataTableOutput("AEcounttableind"))

                    )
                  ),

                  # AE  individual tables ####
                  # tabPanel(
                  #   htmltools::div(icon("database"), "Individual AE tables"),
                  #   htmltools::div( column(2,
                  #               selectizeInput("single_var_input_AEtab", "Patient number:", choices = NULL,
                  #                              options = list(placeholder = "Select a patient",
                  #                                             onInitialize = I('function() { this.setValue(""); }')))
                  #   ),#downloadButton('AEDAYSdownload',"Download the data"),
                  #   htmltools::br(),
                  #   htmltools::br(),
                  #   htmltools::br(),
                  #   shinycssloaders::withSpinner(  DT::dataTableOutput("AEcounttableind"))
                  #   )
                  # ),

                  # AE OVERALL table ####
                  shiny::tabPanel(
                    htmltools::div(shiny::icon("database"), "AE table"),
                    htmltools::p("This sub-tab displays the overall AE table (all AEs no cut off time).   ")
                    ,
                    htmltools::div( shiny::downloadButton('AEcounttabledownload',"Download the data"),
                         htmltools::br(),
                         htmltools::br(),
                         htmltools::br(),
                         shinycssloaders::withSpinner(  DT::dataTableOutput("AEcounttables"))
                    )
                  ),


                  #
                  # AE days ####S
                  shiny::tabPanel(
                    htmltools::div(shiny::icon("database"), "AE days data"), 
                    htmltools::p("This sub-tab displays the number of days a patient had an AE both total and unique days.")
                    ,
                    htmltools::div(shiny::downloadButton('AEDAYSdownload',"Download the data"),
                        htmltools::br(),
                        htmltools::br(),
                        htmltools::br(),
                        shinycssloaders::withSpinner(  DT::dataTableOutput("selected_data_AEDAYS"))
                    )
                  ),





    
               #RECIST PLOT TAB #
               tabPanel(div(icon("chart-line"), "RECIST plot"),
                        span(textOutput("RECIST_Data_Message2"), style="color:red"),
                        uiOutput("rendrecist_plot_patient"),
                        br(),
                        fluidRow(
                          column(1,
                                 downloadButton('download_recist_plot',"Save plot")
                          ),
                          column(2,
                                 actionButton("runAllREplots","Generate All RECIST Plots")
                          ),
                          column(1,
                                 uiOutput("renddownload_all_recist_plots")
                          )
                        ),
                        br(),
                        withSpinner(plotOutput("recist_plot", width = "100%", height = "500px")),
                        br(),
                        downloadButton('RECISTtableinddownload',"Download the data"),
                        br(),
                        DT::dataTableOutput("re_dataind")
                        ),
               tabPanel(div(icon("chart-line"), "AE RECIST plot"),
                        span(textOutput("RECIST_Data_Message3"), style="color:red"),
                        fluidRow(
                          column(2,
                                 uiOutput("rendAErecist_plot_patient")
                          ),
                          column(2,
                                 numericInput("AEREplot_EarlyAECut","Early AE Time Point:", value = NULL, step = 1, min = 1)
                          ),
                          column(3,
                                 checkboxInput("AEREplot_ShowTime","Display Time Annotation", value = F)
                          )
                        ),
                        br(),
                        fluidRow(
                          column(1,
                                 downloadButton('download_AErecist_plot',"Save plot")
                          ),
                          column(2,
                                 actionButton("runAllAEREplots","Generate All AE + RECIST Plots")
                          ),
                          column(1,
                                 uiOutput("renddownload_all_AErecist_plots")
                          )
                        ),
                        br(),
                        withSpinner(plotOutput("AErecist_plot", width = "100%", height = "600px")),
                        br(),
                        tabsetPanel(
                          tabPanel("AE Data",
                                   downloadButton('AEcounttableinddownloadRE',"Download the data"),
                                   br(),
                                   DT::dataTableOutput("AEcounttableindRE")
                          ),
                          tabPanel("RECIST Data",
                                   downloadButton('AERECISTtableinddownload',"Download the data"),
                                   br(),
                                   DT::dataTableOutput("AEre_dataind")
                          )
                        )
                        )

      )) # END NEXT  tabPanel("AE Plots and Measures", tabsetPanel(
      ,

      # Survival Analysis tab <<<<<<<<<<<<<####
      shiny::tabPanel("Survival Analysis",


               htmltools::p("This tab panel allows for survival analysis in the form of Cox PH models for all the adverse event metrics.
             KM plots for overall survival and progression free survival can be selected, as well as box plots of the AE metrics
             by outcome.")
             ,

             shiny::tabsetPanel(
               shiny::tabPanel( #https://stackoverflow.com/questions/46471756/download-pdf-report-in-shiny
                 htmltools::div(shiny::icon( "database" ), "Coxph AE measures"),
                 htmltools::p("This sub-tab displays the results of the Cox PH models for all AE metrics original continous measure (labelled .y)
              and dicotomized as an indictor (ie. > 0, labelled .bin).
              Both for OS and PFS. Please click the 'Run Coxph models' button.")
              ,
              htmltools::div(
                shiny::actionButton("gogocoxmodels", "Run Coxph models" ),
                htmltools::br()),
              shiny::fluidRow(
                shiny::column(2,
                       htmltools::div(shiny::downloadButton('download_BPKP_PlotOutput',"Download selected plot")),
                ),
                shiny::column(2,
                       htmltools::div(shiny::downloadButton('downloadplotspdf',"Download all plots")),
                ),
                shiny::column(4,
                              shiny::uiOutput("rendBPKPplotSelection")
                )
              ),
              htmltools::br(),
              shinycssloaders::withSpinner(shiny::plotOutput("BPKP_PlotOutput", width = "100%", height = "450px"), type = 6),
              htmltools::br(),
              htmltools::div(shiny::downloadButton('kmcoxphinfodownload',"Download CoxPh results"),
                  htmltools::br(),
                  htmltools::br(),
                  htmltools::br(),
                  shinycssloaders::withSpinner( DT::dataTableOutput("kmcophinfo"))
              )
               ),
              #Forest plots OS and PFS tab ####
              shiny::tabPanel( #https://stackoverflow.com/questions/46471756/download-pdf-report-in-shiny
                htmltools::div(shiny::icon("database"), "Forest plots OS and PFS"),
                
                htmltools::p("This sub-tab displays forest plots and the results of the Cox PH models for all AE metrics by AE Category.
              Both for OS and PFS. Please click the 'Run Coxph models' button.")
              ,
                
                htmltools::div(
                  shiny::actionButton("goforstplotstests", "Run tests" ),
                  htmltools::br()),
                shiny::fluidRow(
                  shiny::column(4,
                         htmltools::div(shiny::downloadButton('downloadforestplotspdf',"Download Forest plots")),
                  ),
                  shiny::column(4,
                         shiny::uiOutput("rendForestplotSelection")
                  )
                ),
                shinycssloaders::withSpinner(shiny::plotOutput("Forest_PlotOutput", width = "100%", height = "450px"), type = 6),
                htmltools::br(),
                htmltools::div(shiny::downloadButton('forestplotcophinfodownload',"Download Coxph results"),
                    htmltools::br(),
                    htmltools::br(),
                    htmltools::br()
                    ,
                    shinycssloaders::withSpinner(DT::dataTableOutput("forestplotcophinfo"))
                )
              )

             )) #End   tabPanel("Survival Analysis", tabsetPanel(
      ,

      shiny::tabPanel("Response and Correlations", shiny::tabsetPanel(

        # Response tests tab ####
        shiny::tabPanel( #https://stackoverflow.com/questions/46471756/download-pdf-report-in-shiny
          htmltools::div(shiny::icon("database"), "Response tests"),
          
          htmltools::p("This sub-tab displays the results of t-tests for all AE metrics comparing the PD group vs SD, CR+PR, and CR+PR+SD groups.
              Please click the 'Run tests' button. Plots and results can be downloaded")
          ,
          
          
          htmltools::div(
            shiny::actionButton("goresponsetests", "Run tests" ),
            htmltools::br()),
          shiny::fluidRow(
            shiny::column(4,
                   htmltools::div(shiny::downloadButton('download_responseplots',"Download Response plots")),
            ),
            shiny::column(4,
                          shiny::uiOutput("rendRESPplotSelection")
            )
          ),
          shinycssloaders::withSpinner(shiny::plotOutput("RESP_PlotOutput_all", width = "100%", height = "450px"), type = 6),
          htmltools::br(),
          # shinycssloaders::withSpinner(plotOutput("RESP_PlotOutput_DC_PD", width = "100%", height = "450px"), type = 6),

          htmltools::div(shiny::downloadButton('responsetabledownload',"Download response tests results"),
              htmltools::br(),
              htmltools::br(),
              htmltools::br()
              ,
              shinycssloaders::withSpinner(DT::dataTableOutput("responsettestoutput"))
          )


          # htmltools::div(icon("database"), "Response tests"),
          # htmltools::div(
          #   actionButton("goresponsetests", "Run tests" ),
          #   htmltools::br(),
          #   # downloadButton('download_responseplots',"Download Response plots"),
          #   # htmltools::br(),
          #
          #   downloadButton('responsetabledownload',"Download response test results"),
          #   htmltools::br()
          #   ,
          #   shinycssloaders::withSpinner(DT::dataTableOutput("responsettestoutput"))
          # )






        ),

        # Correlation tab ####
        shiny::tabPanel(
          htmltools::div(shiny::icon("database"), "Correlation"),
          htmltools::p("This sub-tab displays the results of correlation tests for AE metrics with duration of time with AEs.
              Please click the 'Run tests' button. Plots and results can be downloaded")
          ,
          
          htmltools::div(
            shiny::actionButton("goCorrelationtests", "Run tests" ),
            htmltools::br()),
          shiny::fluidRow(
            shiny::column(4,
                   htmltools::div(shiny::downloadButton('downloaddurationplotspdf',"Download correlation plots")),
            ),
            shiny::column(4,
                   shiny::uiOutput("rendCORplotSelection")
            )
          ),
          shinycssloaders::withSpinner(shiny::plotOutput("COR_PlotOutput_all", width = "100%", height = "450px"), type = 6),
          htmltools::br(),
          # shinycssloaders::withSpinner(plotOutput("RESP_PlotOutput_DC_PD", width = "100%", height = "450px"), type = 6),

          htmltools::div(shiny::downloadButton('correlationtabledownload',"Download correlation summary"),
              htmltools::br(),
              htmltools::br(),
              htmltools::br()
              ,
              shinycssloaders::withSpinner(DT::dataTableOutput("durationanalysistableoutput"))
          )
          ,

          htmltools::div(
            shiny::downloadButton('numericcorrelationtabledownload',"Download numeric correlation results"),
            shinycssloaders::withSpinner(DT::dataTableOutput("cornumericalresults"))

          )

          #  htmltools::div(
          #   actionButton("goCorrelationtests", "Run tests" ),
          #   htmltools::br(),
          #   downloadButton('downloaddurationplotspdf',"Download correlation plots"),
          #   htmltools::br(),
          #   downloadButton('correlationtabledownload',"Download correlation test results"),
          #   htmltools::br(),
          #   shinycssloaders::withSpinner(DT::dataTableOutput("durationanalysistableoutput"))
          # )
          #


        )
      )) # END   tabPanel("Response and Correlations", tabsetPanel(
      ,
      shiny::tabPanel("Tables and Reports", tabsetPanel(
        # Survival summary tab ####
        shiny::tabPanel(
          htmltools::div(shiny::icon("database"), "Survival summary"),
          htmltools::div(
            htmltools::p("Please run the 'Coxph AE measures' and 'Forest plots OS and PFS tabs' before downloading the survival summary report." ),
            htmltools::br(),
            shiny::downloadButton('survival_summarydownload',"Download Survival summary"),
            htmltools::br(),
            htmltools::br(),
            htmltools::br(),
            shinycssloaders::withSpinner(DT::dataTableOutput("survivalsummarytable"))
          )
        ),
        # Response summary tab ####
        shiny::tabPanel(
          htmltools::div(icon("database"), "Response summary"),
          htmltools::div( htmltools::p("Please run the 'Response tests tab' before attempting to download the 'Response summary report'." ),
               htmltools::br(),
               shiny::downloadButton('response_summarydownload',"Download Response summary"),
               htmltools::br(),
               htmltools::br(),
               htmltools::br(),
               shinycssloaders::withSpinner(DT::dataTableOutput("responsesummarytable"))
          )
        ),

        # Summary Report download tab ####
        shiny::tabPanel(
          htmltools::div(shiny::icon("database"), "Summary Report"),
          htmltools::div(htmltools::p("Please run the 'Survival summary tab' and 'Response summary tab' before clicking
                  'Download summary report'." ),
              htmltools::br(),
              shiny::downloadButton('downloadsumaryReport',"Download summary report"),
              htmltools::br()

          )
        )
      ))
      #End  tabPanel("Tables and Reports", tabsetPanel(
      ,
      # tab to documentation/feedback
      shiny::tabPanel(
        htmltools::div(shiny::icon("info-circle"), "Documentation"),
        # htmltools::p("To use this app upload a file that has adverse event data stored in table format with first row as a header.
        #   Adverse event data from Encore must be merged with the on treatment date typically found in the demographics data set in Encore.
        #   The file can be of any format i.e. excel (.xlsx, .xls), csv, text (.txt), SAS (.sas7bdat), SPSS (.sav), Stata (.dta) etc.
        #   and it should contain only one table (for example, a table in the first sheet of an Excel file)."),
        # htmltools::hr(),
        # # htmltools::p("Numerical variable with unique values less than or equal to 5 is treated as factor variable. Data type conversion is not available in the app for now."),
        # # htmltools::hr(),
        # htmltools::p(" Variables needed (please ensure variables are named exactly as below):    "),
        # htmltools::tags$htmltools::div(htmltools::tags$ul(
        #     htmltools::tags$li(htmltools::tags$span("sequence_no")),
        #     htmltools::tags$li(htmltools::tags$span("onset_date_of_ae")),
        #     htmltools::tags$li(htmltools::tags$span("cdus_ctcae_toxicity_type_code")),
        #     htmltools::tags$li(htmltools::tags$span("resolved_date")),
        #     htmltools::tags$li(htmltools::tags$span("on_treatment_date")),
        #     htmltools::tags$li(htmltools::tags$span("grade")),
        #     htmltools::tags$li(htmltools::tags$span("attribution_possible")),
        #     htmltools::tags$li(htmltools::tags$span("attribution_probable")),
        #     htmltools::tags$li(htmltools::tags$span("attribution_definite"))
        #                 )
        #          ),
        # htmltools::hr(),

        htmltools::p(" 
                     
                     
                     
                      

# Introduction ", htmltools::br(),   htmltools::br(), "

This is an application for analysis of AE data from Oncore. The application requires 4 data sets 
( a 5th tumor measurement file,with RECIST format as response is optional for additional plots).  
In the OnCore biostats console download the clinical trial data from the 'Data Export' page by 
selecting the protocol number, clicking 'Use Descriptions' in export options, and finally click export. See the Oncore data export screen shot. 
", htmltools::br(), "
- Demographics", htmltools::br(), "
- Follow up", htmltools::br(), "
- Drug administration data", htmltools::br(), "
- Adverse event data", htmltools::br(), "
- RECIST data (optional)

"),

htmltools::p(" 
 
# Running the app", htmltools::br(),   htmltools::br(),  "

Once installed run the function runAEapp() to launch a browser with the applciation and demo data will be loaded. 

"),

htmltools::p(" 
# RECIST data
", htmltools::br(),    htmltools::br(), "
RECIST (Response Evaluation Criteria in Solid Tumors) is a set of standardized criteria used to assess how well a tumor responds to treatment in clinical trials, particularly for cancer therapies. These criteria provide a consistent method for measuring tumor size and determining changes in tumor burden over time. Developed by an international collaboration of cancer organizations, RECIST ensures that tumor response can be compared across different studies.
", htmltools::br(),  htmltools::br(), "
### Key Aspects of RECIST:", htmltools::br(),htmltools::br(), "
1. **Tumor Measurement**: Tumors are classified as target or non-target lesions. Target lesions are selected for precise measurement, while non-target lesions are qualitatively assessed.
   - Target lesions are measured in their longest diameter (except for lymph nodes, which are measured in their short axis).
   - Up to five target lesions are typically selected for measurement, with a maximum of two per organ.
", htmltools::br(),htmltools::br(), "
2. **Response Categories**:", htmltools::br(), "
   - **Complete Response (CR)**: Disappearance of all target lesions.", htmltools::br(), "
   - **Partial Response (PR)**: At least a 30% decrease in the sum of the diameters of target lesions, compared to the baseline.", htmltools::br(), "
   - **Progressive Disease (PD)**: At least a 20% increase in the sum of the diameters of target lesions, or the appearance of new lesions.", htmltools::br(), "
   - **Stable Disease (SD)**: Neither sufficient shrinkage to qualify for PR nor sufficient increase to qualify for PD.", htmltools::br(), "
", htmltools::br(),  "
3. **Evaluation Frequency**: ", htmltools::br(), "Tumor measurements are taken at regular intervals during treatment to monitor changes. The results are used to assess whether the treatment is effective, should be continued, or requires adjustment.
", htmltools::br(), htmltools::br(),"
### Importance of RECIST:", htmltools::br(),htmltools::br(),   "
- **Standardization**: Provides a consistent and objective way to assess tumor response across different clinical trials.", htmltools::br(), "
- **Treatment Evaluation**: Helps determine whether a therapy is working by objectively measuring changes in tumor size.", htmltools::br(), "
- **Regulatory Use**: Used by regulatory agencies like the FDA and EMA to evaluate the efficacy of new cancer treatments."
, htmltools::br() , htmltools::br(), "
Overall, RECIST is a critical tool in oncology research and clinical practice, guiding decisions about patient care and drug approval.
", htmltools::br(),htmltools::br(), "
If using tumor measurement/RECIST data the each subject must have baseline measures and follow up meaures for the tartget lesions.
", htmltools::br(), "The response should be one of the following: 
", htmltools::br(), "
[1] 'Baseline (BL)'        
", htmltools::br(), "
[2] 'Stable Disease (SD)'    
", htmltools::br(), "
[3] 'Progressive Disease (PD)'
", htmltools::br(), "
[4] 'Partial Response (PR)'   
", htmltools::br(), "
[5] 'Not Evaluable (NE)'

 
"),

htmltools::p(" 
## Data tab ", htmltools::br(), htmltools::br(), "

This is the home page (data tab), on the left upload the data. There are multiple tabs to view each of the raw uploaded
data sets and the toxicity data is constructed from merging the uploaded data. 
 ", htmltools::br(),htmltools::br(), "
On the data page there are 5 sub-tabs:
", htmltools::br(), "
- AE data", htmltools::br(), "
- Demographic data", htmltools::br(), "
- Follow up data", htmltools::br(), "
- Drug administration data", htmltools::br(), "
- Toxicity data
 

"),

htmltools::p("  
 
## AE Plots and Measures", htmltools::br(),   htmltools::br(),"

The next tab panel is the AE Plots and Measures page. This tab panel contains a swimmers plot of adverse events and 
tables of AEs. It also provides download buttons to save the plots and data. The AE measures sub-tab contains the
data with the calculated AE metrics used in all other tabs for analysis. 
", 
htmltools::tags$b("One must click 'Calculate measures' before the 
Survival analysis and correlation and response tests can be done. 
"), 
 
   htmltools::br(),htmltools::br(),  "
On the data page there are 4 sub-tabs:", htmltools::br(), "

- AE individual swimmers plot and AE table by grade", htmltools::br(), "
- AE table data (aggregated)", htmltools::br(), "
- AE days data", htmltools::br(), "
- AE measures", htmltools::br(), "
- RECIST plot

 


   

"),

htmltools::p(" 

### AE measures ", htmltools::br(),htmltools::br(), "

This data set contains all the AE variables for analysis on subsequent tabs. 
It creates 18 variables, 6 for all grade AEs, 6 for low grade (1,2) and 6 for grade 3 AEs. 
Half of the AE variables are calculated for all AEs and half are calculated for treatment related AEs.
", htmltools::br(),htmltools::br(), "
- all.grade.duration", htmltools::br(), "
- all.grade.fre", htmltools::br(), "
- all.grade.occurrence", htmltools::br(), "
- all.grade.treatment.related.duration", htmltools::br(), "
- all.grade.treatment.related.fre", htmltools::br(), "
- all.grade.treatment.related.occurrence", htmltools::br(), "
- grade12.duration", htmltools::br(), "
- grade12.fre", htmltools::br(), "
- grade12.occurrence", htmltools::br(), "
- grade12.treatment.related.duration", htmltools::br(), "
- grade12.treatment.related.fre", htmltools::br(), "
- grade12.treatment.related.occurrence", htmltools::br(), "
- grade3.duration", htmltools::br(), "
- grade3.fre", htmltools::br(), "
- grade3.occurrence", htmltools::br(), "
- grade3.treatment.related.duration", htmltools::br(), "
- grade3.treatment.related.fre", htmltools::br(), "
- grade3.treatment.related.occurrence", htmltools::br(), "

The duration is measured in days these are continuouse measures and are the total number of days with an AE.
Fre denotes the frequency of AEs. Occurrence is the unique number of occurrences.  
", htmltools::br(), "
Entering a number in the 'Early AE Time Point' box will use that time as a cutoff. Example: if 30 is used
only AEs in the first 30 days will be used. 



"),

htmltools::p("   
## Survival Analysis 
", htmltools::br(), htmltools::br(),"
This tab panel allows for survival analysis in the form of Cox PH models for all the adverse event metrics. KM plots for overall survival and progression free survival can be selected, as well as box plots of the AE metrics by outcome (partial response, stable disease, progressive disease).
", htmltools::br(),htmltools::br(),  "
- Cox ph measures", htmltools::br(), "
- Forest plots OS and PFS", htmltools::br(), "
", htmltools::br(), "
The Coxph measures sub-tab displays the results of Cox PH models for all AE metrics: original continous measure (labelled .y) and dicotomized as an indictor (ie. > 0, labelled .bin). Both for OS and PFS. Please click the 'Run Coxph models' button
", htmltools::br(), "

                                          
The Forest plot sub-tab displays forest plots for OS and PFS for all the AE metrics. 
The analysis builds on the COXph measures tab analysis but it includes 6 more AE metrics for the unique number of AEs. 
It also can run the models on particular types of adverse events, specified in the drop down box 
under 'Select plot to view'. This sub-tab also includes a table of the results. 


"),

htmltools::p(" 
 
## Response and Correlation tab
", htmltools::br(),htmltools::br(), "
This panel has two sub-panels.
", htmltools::br(),htmltools::br(),  "
- Response tests", htmltools::br(), "
- Correlation ", htmltools::br(), "

The repsonse tests tab runs t-tests comparing the AE metrics between disease control group (complete response + partial response + stable disease) vs progressive disease groups (DC vs PD), partial response vs progressive disease groups (PR vs PD), and stable disease vs progressive disease groups (SD vs PD). Bar plots of p values are displayed along with a table of results (p value and difference) by AE type. Similar to the forest plot panel there is a drop box for AE category. 
", htmltools::br(), "
 
The correlation tab displays a bar plot of the Pearson's correlation coefficient for each AE metric with treatment time. There are also two tables. A text table with the AE category, AE type, and a summary of which type of metric has a negative or positive correlation with treatment time. A second table of numeric results is also displayed and both tables are downloadable. 


 
"),

htmltools::p(" 

## Tables and reports", htmltools::br(), htmltools::br()," 


- Survival analysis ", htmltools::br(), "
- Response analysis ", htmltools::br(), "
- Summary report ", htmltools::br(),htmltools::br(),  "

These sub tabs produce text tables summarizing the relationship between the AE metrics and OS (PFS) by AE category and AE type.
The survival analysis and response tests must be run before these tables will be displayed. Each table can be downloaded as a CSV file. The summary report sub-tab is a pdf that can be downloaded. 


       
                     
                     ")
      )

        ),#END ORigiNAL TABSETPANEL
      htmltools::hr(),
      htmltools::br(),
      htmltools::br(),
      htmltools::br(),
      htmltools::br(),
      htmltools::br(),
      htmltools::br()


      )
    )
  )
}

