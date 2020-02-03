#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
require(shiny)
library(DT)
library(readxl)
library(curl)
library(shinydashboard)
library(jsonlite)
library(data.table)
library(dplyr)
library(swfscMisc)
library(shinycssloaders)

#options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title = "Target associations"),
    dashboardSidebar(
        fileInput('file1', 'Choose xlsx file',
                  accept = c(".xlsx")
        )
    ),
    
    # Show a plot of the generated distribution
    dashboardBody(
        
        fluidRow(
            box(withSpinner(DTOutput('tbl'), type = 6), width = 20)
        )    
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    options(shiny.maxRequestSize=60*1024^2)
    baseur_opt<- "https://platform-api.opentargets.io/v3/platform/public/association/filter?target="
    parm <- "&size=10000&datastructure=full"
    search_ids <- "https://platform-api.opentargets.io/v3/platform/public/search?q="  
    tar_1 <- eventReactive(input$file1, {  ### This table contains the results for targets
        req(input$file1)
        inFile <- input$file1
        df <- as.data.frame(readxl::read_excel(inFile$datapath, 1, col_names = F))
        names(df)[1] <- "gene_symbol"
        df
        genes_test <- df$gene_symbol
        length_ids <- length(genes_test)
        pba_ids <- lapply(genes_test, function(x){
            fromJSON(paste0(search_ids,x, parm),flatten=TRUE)
        })
        m_sim_ids <- lapply(1:length_ids, function(x) pba_ids[[x]]$data[1,3:4])
        ids_opt <- as.data.frame(rbindlist(m_sim_ids, fill = TRUE))
        pba <- lapply(ids_opt$id, function(x){
            fromJSON(paste0(baseur_opt,x, parm),flatten=TRUE)
        })
        
        m_sim <- lapply(1:length(ids_opt$id), function(x) pba[[x]]$data)
        
        m2_sim <- as.data.frame(rbindlist(m_sim, fill = TRUE))
        
        asso_table <- m2_sim[,c(1,3,17,18,48,50,19:26,4,12)]
        names(asso_table) <- c("direct association", "target_id","gene_symbol","gene_descript",
                               "disease_id","disease_descript","score_overall","score_literature",
                               "score_rna_expression","score_genetic_association","score_somatic_mutation",
                               "score_known_drug","score_animal_model","score_affected_pathway",
                               "tractability_smallmol","tractability_antibody")
        asso_table[is.na(asso_table$tractability_smallmol),15] <- "Unknown"
        asso_table[is.na(asso_table$tractability_antibody),16] <- "Unknown"
        asso_table <- round(asso_table,2)
        asso_table
        
    })
    
    output$tbl = DT::renderDT({
        req(input$file1)
        DT::datatable(tar_1(),  rownames = F, caption = "Target - Disease Results", filter = 'top',extensions = list('Buttons' = NULL, 'ColReorder' = NULL, 'FixedColumns' = NULL), 
                      options = list(escape = F,   
                                     autoWidth = T, scrollX = T, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     fixedColumns = list(leftColumns = 3)
                      ) 
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
