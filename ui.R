library(shiny, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(shinydashboard, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(shinyjs, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.3/")
library(leaflet, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(rhandsontable, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(shinycssloaders, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(glue, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(DT)
library(ggvis)


options(shiny.reactlog = FALSE,
        shiny.maxRequestSize = 30 * 1024 ^ 2)

nx.actionButton = function (inputId, label, icon = NULL) {
  if (!is.null(icon))
    buttonContent <- list(icon, label)
  else
    buttonContent <- label
  tags$button(id = inputId,
              type = "button",
              class = "btn btn-primary action-button",
              buttonContent)
}
#
shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    includeCSS("www/progress.css"),
    includeCSS("www/bootstrap.min.css"),
    includeCSS("www/animate.min.css"),
    includeCSS("www/modern-business.css"),
    includeCSS(path = "www/AdminLTE.css"),
    includeScript(path = "www/app.js"),
    tags$head(includeScript("www/ga.js")),
    includeHTML('www/header.html'),
    tags$body(tags$style(
      HTML(
        "
        #mymap { background-color: #D4DADC;}
        body{
        color: black;
        }
        a:link, a:visited, a:hover, a:active {
        text-decoration: none;
        color: black;
        }
        li.active {
        color: black;
        }
        h1 {
        font-size: 200%;
        font-weight: bold;
        }
        hr {
        border: 0.1;
        height: 1px;
        background-image: linear-gradient(to right, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.75), rgba(0, 0, 0, 0));
        }
        .leaflet-control-zoom{
        visibility: hidden
        }
        
        "
      )
      )),
    #
    headerPanel(title = "GADGET Compute", windowTitle = 'GADGET Compute'),
    sidebarPanel(
      width = 4,
      radioButtons(
        "subType",
        "0.   How many SNPs do you want to analyze?",
        c(
          "Use the interactive PTS calculator for <1000 trait-associated SNPs" = "interactive",
          "Use the batch mode PTS calculator for >1000 trait-associated SNPs" = "batch"
        )
      ),
      conditionalPanel(
        condition = "input.subType == 'interactive'",
        radioButtons(
          "entryMethod",
          "1. Input your SNP table for PTS calculation",
          c("Paste" = "paste",
            "Upload" = "upload")
        ),
        conditionalPanel(
          condition = "input.entryMethod == 'paste'",
          strong("2. Copy and paste your SNP data"),
          rHandsontableOutput("hot"),
          actionButton('clearTable', "Reset SNP table"),
          actionButton("loadTable", "Load example table")
        ),
        conditionalPanel(
          condition = "input.entryMethod == 'upload'",
          strong("2. Upload your SNP data"),
          fileInput(
            "snpfile",
            label = NULL,
            multiple = FALSE,
            accept = c("text/tsv",
                       "text/tab-separated-values,text/plain",
                       ".tsv")
          ),
          tags$script('$( "#snpfile" ).on( "click", function() { this.value = null; });')
        ),
        
        
        a('Download example SNP table', href = 'kidney_disease_snps.tsv', style = "text-decoration: underline; color: #123339;font-size: .75em;"),
        br(),
        br(),
        p(
          "Pasting large amounts of data can cause your browser to freeze briefly."
        ),
        p("Please be patient while your input is validated"),
        radioButtons(
          "weight",
          "3.   Type of weighting to use",
          c(
            "None" = " -w unweighted ",
            "Log odds ratio" = " -w OR ",
            "Beta" = " -w Beta "
          )
        ),
        strong("4.   Calculate PTS"),
        tags$br(),
        nx.actionButton('submit', 'Go go GADGET!'),
        tags$script(
          '$( "#submit" ).on( "click", function() { window.scrollTo(0, 0) });'
        ),
        
        p(
          "Calculation typically takes less than 30secs, large sets can take several minutes"
        ),
        tags$br(),
        tags$br(),
        bookmarkButton("Share table")
      ),
      conditionalPanel(
        condition = "input.subType == 'batch'",
        strong("1. Upload your SNP data"),
        fileInput(
          "batchFile",
          label = NULL,
          multiple = FALSE,
          accept = c("text/tsv",
                     "text/tab-separated-values,text/plain",
                     ".tsv")
        ),
        tags$script('$( "#snpfile" ).on( "click", function() { this.value = null; });'),
        p(
          "Please make sure column 4 of your SNP Table contains Beta or transformed OR values if you want weighting to be used"
        ),
        a('Download example SNP table', href = 'kidney_disease_snps.tsv', style = "text-decoration: underline; color: #123339;font-size: .75em;"),
        p("Accepts files up to 30MB"),
        textInput("email", "Email to send results:"),
        nx.actionButton("batchSubmit", 'Go go GADGET!'),
        tags$script(
          HTML(
            "$(document).on('shiny:idle',
            function() {
            ga('set','userId', Shiny.user);
            }
          );"
  )
        ),
  tags$script(
    HTML(
      "$(document).on('shiny:filedownload', function(event) {
      ga('send','event', 'output',
      'download', event.name, event.href);
      });"
  )
      ),
  tags$script(
    HTML(
      "$(document).on('shiny:recalculating', function(recalculating) {
      ga('send','event', 'output',
      'recalc', 'recalculating');
      });"
  )
        ),
  tags$script(
    HTML(
      "$(document).on('shiny:recalculated', function(recalculating) {
      ga('send','event', 'output',
      'recalc', 'recalculated');
      });"
  )
      ),
  
  tags$script(
    HTML(
      "$(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'submit' || event.name === 'batchSubmit' || event.name === 'loadTable' || event.name === 'clearTable' || event.name === 'phenotype') {
      ga('send','event', 'input',
      'updates', event.name, event.value);
      }
      });"
  )
  )
      )
  
  
      ),
  
  mainPanel(
    tabsetPanel(
      id = 'GRS',
      tabPanel(
        "Instructions",
        fluidRow(
          width = 8,
          br(),
          box(
            id = "info",
            title = "PTS calculator for user-supplied SNP sets",
            width = NULL,
            status = "primary",
            solidHeader = T,
            includeHTML('www/text.html')
          ),
          box(
            id = "QC",
            title = "SNP quality control",
            width = NULL,
            status = "primary",
            solidHeader = T,
            includeHTML('www/qc.html')
          )
        ),
        tags$br(),
        tags$hr()
      ),
      tabPanel("Batch submission",
               value = "Batch",
               htmlOutput("submission")),
      tabPanel(
        "GADGET Plots",
        value = "plots",
        
        fluidRow(
          width = 10,
          box(
            id = "phenobox",
            title = "Choose a phenotype to explore",
            width = 9,
            status = "primary",
            solidHeader = T,
            uiOutput("choose_phenotype")
          ),
          box(
            id = "mapbox",
            title = "Distribution of PTS Worldwide",
            width = 9,
            status = "warning",
            solidHeader = F,
            leafletOutput("mymap") %>% withSpinner(color = "#3C8DBC"),
            checkboxInput("legend", "Show legend", TRUE)
          ),
          box(
            id = "stripbox",
            title = "PTS Distribution by Population",
            width = 9,
            status = "warning",
            solidHeader = F,
            plotOutput("stripplot") %>% withSpinner(color = "#F39C12")
          ),
          #height set to maintain readability of plot
          box(
            id = "plotbox",
            title = "PTS Distribution by Continental Group",
            width = 9,
            status = "warning",
            solidHeader = F,
            plotOutput("plot") %>% withSpinner(color = "#F39C12")
          ),
          br(),
          br(),
          box(
            title = "Trait set information",
            width = 9,
            status = "primary",
            solidHeader = F,
            collapsible = T,
            collapsed = T,
            DT::dataTableOutput("mytable") %>% withSpinner(color =
                                                             "#F39C12"),
            DT::dataTableOutput("SNPtable") %>% withSpinner(color =
                                                              "#F39C12"),
            htmlOutput('aovSummary') %>% withSpinner(color = "#F39C12")
            
          )
        ),
        fluidRow(),
        tags$hr(),
        fluidRow(
          # Buttons only visible after plot function has finished
          # dlReports is a reactive UI element for the download reports button
          column(width = 7, uiOutput("dlReport")),
          br(),
          br(),
          br(),
          # dlPlots is a reactive UI element for the download plots button
          column(width = 7, uiOutput("dlPlots"))
        ),
        tags$br()
      ),
      tabPanel(
        "Human population abbreviation key",
        value = "key",
        tags$br(),
        fluidRow(
          box(
            title = "Human population abbreviation key",
            width = NULL,
            status = "warning",
            solidHeader = T,
            collapsible = F,
            includeMarkdown("labels.md")
          ),
          br(),
          br(),
          br()
        )
      )
    ),
    fluidRow(column(
      width = 12,
      box(
        title = NULL,
        width = NULL,
        status = "danger",
        solidHeader = F,
        p(
          "GADGET is intended as a tool for researchers to explore population-specific distributions of genetic variants that have been associated with a wide variety of human traits.
          Users of this site should treat the results with caution, as the interpretation PTS across populations can be complicated by a number of factors.  We provide more detail on these issues on the
          Learn page and in our manuscript describing the server."
        )
        )
        ))
    
      )
    )
    )

