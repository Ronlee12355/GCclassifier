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
      h1('Upload gastric cancer gene expression profile to predict molecular subtype',style='font-weight:bold;'),
      br(),
      fluidRow(
        column(
          6,
          wellPanel(
            fileInput(
              'Expr',label = 'Gene expression profile data to upload (30MB maximum): ', buttonLabel = 'File',accept = ".csv"
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
              condition = "input.method == 'TCGA'",
              sliderInput(
                'minPosterior',
                "Minimal posterior probability to classify a sample",
                min = 0, max = 1, value = 0.5, step = 0.01
              )
            ),
            br(),
            selectizeInput(
              'idType', label = 'Input gene id:',
              choices = c("SYMBOL", "ENSEMBL", "ENTREZID", "REFSEQ"),
              selected = "SYMBOL"
            ),
            br(),
            HTML('Example input dataset could download <a href="GCclassifier_example.csv", target="_blank" download="GCclassifier_example.csv">HERE</a>'),
            helpText('1. Gene id column should be specified as Symbol in uploaded file.'),
            helpText('2. No empty values were allowed in the expression profile.'),
            helpText('3. Gene expression profile cannot contain any negative value(s).'),
            helpText('4. Please choose the right gene identifier according to your data.'),
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
    'Contact',
    icon = icon('user'),
    fluidPage(
      h1("Contact",style='font-weight:bold;'),
      p('If you have any questions or comments, please feel free to contact us.'),
      hr(),
      tags$div(
        h3(strong("Xin Wang, Ph.D. Associate Professor")),
        p(strong('Email: \n'), p('xwang(a)surgery.cuhk.edu.hk')),
        p(strong('Address: \n'), p('Prince of Wales Hospital, Shatin, N.T., The Chinese University of Hong Kong, Hong Kong SAR')),
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
    if(!('Symbol' %in% colnames(df))){
      message <- 'Column names should include Symbol in the file to identify gene ids.'
      data.inputs$message <- F
      output$mRNA_msg<-renderUI({
        p(icon('window-close'),message,style='color:red;')
      })
    }else{
      data.inputs$mRNA<-read.csv(input$Expr$datapath, check.names = F, row.names = 'Symbol')
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

    Sys.sleep(1)
    tryCatch({
        res <- GCclassifier::get_molecular_subtype(
          Expr = data.inputs$mRNA, method = input$method ,idType = input$idType,
          minPosterior = ifelse(is.null(input$minPosterior), 0.5, input$minPosterior),
          maxp = NULL, verbose = F)
      },
      error = function(e){
        removeModal()
        showModal(modalDialog(
          title = p(icon('exclamation'),strong("Error information")),
          tagList(
            h3('An error happens, please check your upload file or parameters and refresh the webpage, below is the error info from server', style='color:red;', align='center'),
            h4(as.character(e), align='center')
          ), footer = NULL, easyClose = F,size='l'))
        stop()
      }
    )
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
