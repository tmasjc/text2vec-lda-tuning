library(shiny)
library(text2vec)
library(LDAvis)
library(V8)
library(shinyjs)
library(servr)
library(shinycssloaders)

# set theme if lib is available
skin = ifelse(require(shinythemes), shinytheme("cosmo"), NULL)

# custom JS code to hide default background color for LDAvis
jscode = "shinyjs.modLDAvis = function(){$('#top').children().css('background-color', 'rgba(0,0,0,0)');}"

ui <- fluidPage(
    # shinythemes::themeSelector(),
    theme = skin,
    titlePanel("Hyperparameters Tuning for LDA in {text2vec}"),
    useShinyjs(),
    extendShinyjs(text = jscode),
    sidebarLayout(
        sidebarPanel(
            # (¯`·._.·(¯`·._.·(¯`·._.· CONTROL ·._.·´¯)·._.·´¯)·._.·´¯)
            numericInput("topics", "Number of Topics:", 
                         value = 3, min = 1, max = 99, step = 1/2,
            ), tags$hr(),
            # (¯`·._.·(¯`·._.·(¯`·._.· CONTROL ·._.·´¯)·._.·´¯)·._.·´¯)
            sliderInput("doc_topic_prior", "Document-Topic Prior:",
                         value = 0.1, min = 0.05, max = 1, step = 0.05
            ), tags$hr(),
            # (¯`·._.·(¯`·._.·(¯`·._.· CONTROL ·._.·´¯)·._.·´¯)·._.·´¯)
            numericInput("word_topic_prior", "Word-Topic Prior:",
                         value = 0.001, min = 0.0001, max = 1, step = 0.0
            ), 
            numericInput("word_topic_prior_step", "Step for Word-Topic Prior:",
                         value = 0.0001, min = 0.0001, max = 0.01, step = 0.0001/2
            ), tags$hr(),
            # (¯`·._.·(¯`·._.·(¯`·._.· CONTROL ·._.·´¯)·._.·´¯)·._.·´¯)
            textInput("seed", "Random Seed (optional)", value = "1234", width = "100%", 
                      placeholder = "Do not leave blank."),
            # (¯`·._.·(¯`·._.·(¯`·._.· CONTROL ·._.·´¯)·._.·´¯)·._.·´¯)
            fileInput("rds", HTML("Upload <i>.dtm</i> File <br>(see Example directory)"), accept = ""),
            actionButton("go", "Fit model", width = "100%"),
            tags$hr(),
            # (¯`·._.·(¯`·._.·(¯`·._.· FOOTNOTE ·._.·´¯)·._.·´¯)·._.·´¯)
            tags$strong("DOCS"),
            tags$p("http://text2vec.org/topic_modeling")
        ),
        mainPanel(
            withSpinner(visOutput('ldavis'), type = 8, color = "#333333")
        )
    )
)

server <- function(input, output, session) {
    
    # create a cache for LDAvis JSON
    tmp <- tempdir(check = FALSE)
    
    observeEvent(input$word_topic_prior_step, {
        updateNumericInput(session, "word_topic_prior", 
                           step = input$word_topic_prior_step/2)
    })
    
    load_rds <- eventReactive(input$go, {
        
        req(input$rds)
        
        # let error fails naturally and be displayed on screen
        dtm <- readRDS(input$rds$datapath)
        
        # initiate random seeding
        rs = ifelse(isTruthy(input$seed), input$seed, "1234")
        set.seed(as.numeric(rs))
        
        # create model based on user's inputs
        lda_model <- LDA$new(
            n_topics         = input$topics,
            doc_topic_prior  = input$doc_topic_prior,
            topic_word_prior = input$word_topic_prior
        )
        
        # todo: move params to control sidebar
        doc_topic_distr <- 
            lda_model$fit_transform(x = dtm, n_iter = 1000, 
                                    convergence_tol = 0.001, n_check_convergence = 25, 
                                    progressbar = FALSE)
        
        # save to cache 
        lda_model$plot(out.dir = tmp, open.browser = FALSE)
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    output$ldavis <- renderVis({
        load_rds()
        readLines(file.path(tmp, "lda.json"), warn = FALSE)
    })
    
    # trigger custom JS but wait for some delay
    observe({
        load_rds()
        delay(1000, js$modLDAvis())
    })
}

shinyApp(ui = ui, server = server)
