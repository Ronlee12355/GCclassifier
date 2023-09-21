library(shiny)
options(shiny.maxRequestSize=30*1024^2)

## ======= UI ===========
ui <- navbarPage(
  title = 'GCclassifier',
  position = 'static-top',
  inverse = T,
  collapsible = T,

  tabPanel(
    'Analyze',
    icon = icon('wrench'),
    fluidPage(
      h1('Molecular subtype prediction based on gene expression profiles',style='font-weight:bold;'),
      br(),
      fluidRow(
        column(
          6,
          wellPanel(
            fileInput(
              'Expr',label = 'Gene expression profile data to upload (30MB maximum):', buttonLabel = 'File',multiple=FALSE, accept = ".csv"
            ),
            br(),
            uiOutput('mRNA_msg'),
            uiOutput('mRNA_view'),
            br(),
            radioButtons(
              inputId = "method",label = "Prediction method to choose:",
              choices = c('EMP', 'ACRG', 'TCGA'),
              selected = "EMP",inline = TRUE
            ),
            br(),
            conditionalPanel(
              condition = "input.method == 'EMP'",
              radioButtons(
                'useMinPosterior',
                "Whether minimal posterior probability is used to classify a sample:",
                choices = c('Yes', 'No'), selected = 'No',inline = TRUE
              )
            ),
            br(),
            conditionalPanel(
              condition = "input.method == 'TCGA' || input.useMinPosterior == 'Yes'",
              sliderInput(
                'minPosterior',
                "Minimal posterior probability to classify a sample",
                min = 0, max = 1, value = 0.5, step = 0.01
              )
            ),
            br(),
            selectizeInput(
              'idType', label = 'Input gene ID:',
              choices = c("SYMBOL", "ENSEMBL", "ENTREZID", "REFSEQ"),
              selected = "SYMBOL"
            ),
            br(),
            HTML('Example input dataset could download <a href="GCclassifier_example.csv", target="_blank" download="GCclassifier_example.csv">HERE</a>'),
            helpText('Gene IDs column should be specified as \'Gene_ID\' in gene expression profiles'),
            br(),
            br(),
            br(),
            p(actionButton('submit', 'Submit', class='btn-success', style="color:white;", icon = icon('paper-plane')),
              align='center')
          )
        ),
        column(
          6,
          DT::dataTableOutput('prediction_result')
        )
      )
    )
  ),

  tabPanel(
    'Tutorial',
    icon=icon('map-signs'),
    fluidPage(
      h1('A step-by-step guide to use GCclassifier',style='font-weight:bold;'),
      br(),
      hr(),

      h3('Step 1: Preparation of gene expression profiles'),
      p('Upload your gene expression data in .csv format, where columns are samples and rows are genes.
      The name of the column of gene IDs should be specified as \'Gene_ID\' in the file to upload.'),
      p('A preview of the expression profiles to be uploaded: '),
      div(tags$img(src=paste0('images', '/data_example.png'), width='40%'), style="text-align: left;"),
      br(),

      h3('Step 2: Molecular subtype prediction'),
      p('(1) Uploading gene expression profiles'),
      p('(2) Selecting a method for subtype prediction'),
      p('(3) Specifying additional parameters (if any) for the selected prediction method'),
      p('(4) Selecting the type of gene identifiers in the uploaded data'),
      p('(5) Clicking \'Submit\' for gastric cancer subtype classification'),
      div(tags$img(src=paste0('images', '/prediction_step.png'), width='40%'), style="text-align: left;"),
      br(),

      h3('Step 3: Downloading subtype prediction results'),
      p('The predicted results will appear on the right side of the webpage,
      and can be copied or downloaded as a file in .csv, .xlsx or .pdf format.'),
      div(tags$img(src=paste0('images', '/result.png'), width='40%'), style="text-align: left;"),
      br(),

      h3('Cautions', style='color: red;'),
      HTML(
        '<div>
        <ul>
          <li><p style="font-weight:bold;">Currently, the expression profiles with \'NA\' values cannot be uploaded and processed by GCclassifier. It is suggested to perform imputation before uploading to the online server.</p></li>
          <li><p style="font-weight:bold;">Please choose the right type of gene identifiers according to your data. Currently, GCclassifier can accept NCBI Entrez, Ensembl, HGNC symbol and  Refseq.</p></li>
          <li><p style="font-weight:bold;">Only numeric values in gene expression profiles are accepted.</p></li>
          <li><p style="font-weight:bold;">Gene expression profiles should not contain any negative value(s).</p></li>
        </ul>
      </div>'
      )
    )
  ),

  tabPanel(
    'Contact',
    icon = icon('user'),
    fluidPage(
      h1("Contact",style='font-weight:bold;'),
      p('If you have any questions or comments, please feel free to contact us.'),
      hr(),
      tags$div(
        p(strong('Correspondence:')),
        p(("Xin Wang, PhD")),
        p("Associate Professor"),
        p("Department of Surgery"),
        br(),
        p(strong('Technician:')),
        p('Jiang Li'),
        p("Department of Surgery"),
        br(),
        br(),
        p(strong('Email: \n'), p('xwang(a)surgery.cuhk.edu.hk \n'), p('jiangli9-c(a)my.cityu.edu.hk')),
        p(strong('Address: \n'), p('Room 124031, 10/F, Lui Che Woo Clinical Sciences Building, Prince of Wales Hospital, SHA TIN DISTRICT, NEW TERRITORIES, HONG KONG')),
        p(strong('Phone: \n'), p('(852) 3505 2789')),
        br()
      )
    )
  ),

  ### ===== footer=====
  br(),
  br(),
  br(),
  br(),
  h4(
    'Copyright @ ',
    a(href = 'https:://xinlab.netlify.com/', 'xinlab'), ', ',
    a(href = 'https://www.surgery.cuhk.edu.hk/', 'Department of Surgery, Faculty of Medicine'), ', ',
    a(href = 'https://www.cuhk.edu.hk/chinese/index.html', 'The Chinese University of Hong Kong'),
    align = 'center'
  ),
  h4('Prince of Wales Hospital, Shatin, N.T., Hong Kong SAR, China', align = 'center'),
  h4('Any comments and suggestions, please contact us.', align = 'center'),
  shinyjs::useShinyjs()
)

server <- function(input, output, session){
  data.inputs<-reactiveValues(mRNA=NULL, message = T)
  observe({
    if(is.null(input$Expr$datapath)){
      shinyjs::disable('submit')
    }else{
      shinyjs::enable('submit')
    }

    if(!isTRUE(data.inputs$message) || is.null(input$Expr$datapath)){
      shinyjs::disable('submit')
    }else{
      shinyjs::enable('submit')
    }
  })

  observeEvent(input$Expr,{
    req(input$Expr$datapath)
    df <- read.csv(input$Expr$datapath, check.names = F)
    if(!('Gene_ID' %in% colnames(df))){
      message <- 'Column names should include Gene_ID in the file to identify gene ids.'
      data.inputs$message <- F
      output$mRNA_msg<-renderUI({
        p(icon('window-close'),message,style='color:red;')
      })
    }

    if(sum(duplicated(df$Gene_ID)) > 0 || any(df$Gene_ID == '')){
      message <- 'duplicate or empty Gene_ID are not allowed.'
      data.inputs$message <- F
      output$mRNA_msg<-renderUI({
        p(icon('window-close'),message,style='color:red;')
      })
    }else{
      data.inputs$mRNA<-read.csv(input$Expr$datapath, check.names = F, row.names = 'Gene_ID')
      if(any(is.na(data.inputs$mRNA))){
        message <- 'Gene expression profile cannot contain any NA value(s).'
        data.inputs$message <- F
        output$mRNA_msg<-renderUI({
          p(icon('window-close'),message,style='color:red;')
        })
      }else if(any(data.inputs$mRNA < 0, na.rm = T)){
        message <- 'Gene expression profile cannot contain any negative value(s).'
        data.inputs$message <- F
        output$mRNA_msg<-renderUI({
          p(icon('window-close'),message,style='color:red;')
        })
      }else if(any(colnames(data.inputs$mRNA) %in% c("SYMBOL","ENSEMBL","ENTREZID", "REFSEQ"))){
        message <- 'Sample names in expression profile should not contain "SYMBOL", "ENSEMBL", "ENTREZID" and "REFSEQ".'
        data.inputs$message <- F
        output$mRNA_msg<-renderUI({
          p(icon('window-close'),message,style='color:red;')
        })
      }else if(ncol(data.inputs$mRNA) <= 1){
        message <- 'Sample size in expression profile should be larger than one.'
        data.inputs$message <- F
        output$mRNA_msg<-renderUI({
          p(icon('window-close'),message,style='color:red;')
        })
      }else{
        data.inputs$message <- T
        output$mRNA_msg<-renderUI({
          p(icon('check-square'),'Data is ready to upload',style='color:green;')
        })
      }

      output$mRNA_view<-renderTable({
        data.inputs$mRNA[1:8,1:5]
      }, rownames = T)
    }
  })

  observeEvent(input$submit, {
    showModal(modalDialog(
      tagList(
        h3(
          img(src="Loading_icon.gif", heigth='35%', width='35%'),
          br(),
          'Gastric cancer molecular subtype prediction is processing......',
          align = 'center',
          style = 'color:black;'
        )
      ),footer = NULL,size='l'))

    Sys.sleep(1.5)
    tryCatch({
      res <- GCclassifier::classifyGC(
        Expr = data.inputs$mRNA, method = input$method ,idType = input$idType,
        minPosterior = ifelse(is.null(input$minPosterior), 0.5, input$minPosterior),
        useMinPosterior = ifelse(input$useMinPosterior == 'Yes', T, F),
        maxp = NULL, verbose = F)
    },
    error = function(e){
      removeModal()
      showModal(modalDialog(
        title = p(icon('exclamation'),strong("Error information")),
        tagList(
          h3('An error happens, please check your upload file or parameters and refresh the webpage, below is the error info from server', style='color:red;', align='center'),
          h4(as.character(e), align='center')
        ), footer = NULL, easyClose = F, size='l'))
    }
    )
    res$subtype <- as.character(res$subtype)
    res[is.na(res)] <- 'Unclassified'
    output$prediction_result<-DT::renderDataTable(server = F, {
      DT::datatable(
        res,
        rownames=F,
        extensions = 'Buttons',width = '100%',
        options = list(
          paging = TRUE,searching = TRUE,
          scrollX=TRUE,fixedColumns = F,
          autoWidth = F,ordering = TRUE,
          dom = 'Bfrtip', pageLength = 30,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ))
    })
    removeModal()
  })
}

## ====== run app ========
shinyApp(ui = ui, server = server)
