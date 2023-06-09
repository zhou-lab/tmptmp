#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sesame)
library(shinybusy)
# library(plotly)
library(tidyverse)
library(ggiraph)
library(stringr)
library(tibble)
library(glue)

#custom font (Rubrik)
library(showtext)
font_add(family = "rubrik", regular = "./www/rubrik-regular-webfont.ttf")
showtext_auto()

#include file with customized KYCG functions
source("customized_KYCG_functions.R")

# global variables
dbgroups_under_analysis <- data.frame()
results_pgc <-data.frame()

##############################################################################

# Create dataframe containing all DB group names - mapping group name and platform (EPIC     metagene) to long title (KYCG.EPIC.metagene.20220126)
dbgroups <- KYCG_listDBGroups() %>% tidyr::extract(col=Title,regex ="^(\\w+).(\\w+).(.+)\\.(\\w+)$",into=c("kycg","platform","group","group_suffix"),remove=FALSE)


  #data frames looks like
  #Title                           kycg  platform group       group_suffix Description                                          type
  #1 KYCG.EPIC.metagene.20220126     KYCG  EPIC     metagene    20220126     metagene coordinates with respect to GENCODE v36   cate…
  #2 KYCG.HM450.metagene.20220126    KYCG  HM450    metagene    20220126     metagene coordinates with respect to GENCODE v36     cate…
  #3 KYCG.Mammal40.metagene.20220126 KYCG  Mammal40 metagene    20220126     metagene coordinates with respect to GENCODE v36     cate…
  #4 KYCG.MM285.metagene.20220126    KYCG  MM285    metagene    20220126     metagene coordinates with respect to GENCODE vM25    cate…
  #5 KYCG.EPIC.mask.20220123         KYCG  EPIC     mask        20220123     EPIC probe masking by copy number and SNP effects b… cate…
  #6 KYCG.HM450.mask.20220123        KYCG  HM450    mask        20220123     HM450 probe masking by copy number and SNP effects … cate…
  #7 KYCG.MM285.mask.20220123        KYCG  MM285    mask        20220123     MM285 probe masking 20220123_MM285_masks.xlsx        cate…

#define string containing query example
query <- KYCG_getDBs("MM285.designGroup")[["PGCMeth"]]
example_query <- paste(query, collapse = "\n")

# get default DB groups 
default_db_groups<-read_tsv("DefaultDbGroups.txt")%>% filter(default==1)

# Define server logic 
function(input, output, session) {

  # Handler for "Try A query set example" button
  # When Try a query set example is clicked, populate the textbox with the example query set
  observeEvent(input$link_show_example,{
    updateTextAreaInput(session, "query",value = example_query)
  })

  # infer platform
  observeEvent(input$query,{
    query<- input$query
    selected_platform <- input$selected_platform
    
    if( is.null(input$query) || input$query ==""){  #input$selected_platform != "" ||
      output$inferredPlatformMsg <- renderText({""})        
      output$notInferredPlatformMsg <- renderText({""})
      updateSelectizeInput(session, 'selected_platform',choices = unique(dbgroups$platform), selected = NULL) 
    }else{
      cpg_ids_vector <- unlist(strsplit(input$query,"\n"))
      tryCatch({
        platform <- inferPlatformFromProbeIDs(cpg_ids_vector)
        output$inferredPlatformMsg <- renderText({"Platform inferred from CpG IDs above."})
        output$notInferredPlatformMsg <- renderText({""})        
        updateSelectizeInput(session, 'selected_platform',choices = unique(dbgroups$platform), selected = platform) 
      },error = function(e) {
        output$notInferredPlatformMsg <- renderText({"Platform cannot be inferred from CpG IDs above.  Please select a platform."})
        output$inferredPlatformMsg <- renderText({""})
        updateSelectizeInput(session, 'selected_platform',choices = unique(dbgroups$platform), selected = NULL) 
        
        return()
      }
      )      
    }
  })

  # for Platform selectinput control, define options and initialize as MM285
  updateSelectInput(session,
                    "selected_platform",
                    choices = unique(dbgroups$platform), selected = NULL) #"MM285"

  # Handler for Platform selectinput control
  # when a new platform is selected, update the available db groups to select from.
  observeEvent(c(input$selected_platform),
  {
    # get a list of db groups that are associated with the selected platform
    dbgroups_in_platform <- dbgroups %>%
         filter(platform == input$selected_platform) %>%
         pull(group)
    
    default_db_group_selection <- default_db_groups %>% filter(platform == input$selected_platform) %>% pull(group)
    
    #update db select input choices to the list db groups defined above   
    updateSelectInput(session,
                       "selected_groups",
                       choices=dbgroups_in_platform, selected = default_db_group_selection) #c("chromHMM") NULL
   })

    # Handler for upload of CSV or TXT file containing CpG IDs (1 per line)
    observeEvent(input$uploaded_cpg_ids,{
      inFile <- input$uploaded_cpg_ids
      if (is.null(inFile)) return(NULL)
      uploadedData <- read.csv(inFile$datapath, header = FALSE)
      uploadedDataStr <- paste(uploadedData$V1, collapse = "\n")
      updateTextAreaInput(session, "query",value = uploadedDataStr)
    })

    # Handler for download example file
    output$example_file_download <- downloadHandler(
      filename = function(){
        paste0(Sys.Date(),"_sample_query_ids.csv")
      },
      content = function(file){
        file.copy("data/sample_query_ids.csv",
                  file)
      }
    )
    
    # Initialize CPG IDs textbox to empty.
    updateTextInput(session,"query",value = NULL) 
    
    output$dotPlot <- renderGirafe({return(NULL)})#renderPlot
    output$distPlot <- renderGirafe({return(NULL)})

    observeEvent(input$submitBUtton,{
      req(input$selected_platform,input$query)
      
      tryCatch({
      # Use isolate function so plot won't be updated when control 
      # values change but rather only when Submit is clicked
      # Pull list of CpG Ids from text box into vector
      cpg_ids_vector <- isolate(unlist(strsplit(input$query,"\n")))
      
      if(is.null(cpg_ids_vector) || length(cpg_ids_vector) ==0){}
      else{
        show_modal_spinner(spin = "orbit")
        # Pull platform and groups values from controls
        selected_platform <- isolate(input$selected_platform)
        selected_groups <- isolate(input$selected_groups)
        
        if(! is.null(selected_groups)){
          # Given selected_platform and selected_groups, 
          # build list of db_groups' fullnames (KYCG.MM285.metagene.20220126)
          selected_groups_df <- data.frame(group=selected_groups)
          available_dbgroups <- dbgroups %>% filter(platform == selected_platform )
          dbgroups_fullnames <-left_join(selected_groups_df,available_dbgroups, by="group") %>% select(Title)
          results_pgc_db <- isolate(testEnrichment(cpg_ids_vector,
                                                databases = dbgroups_fullnames$Title,platform =  input$selected_platform))
        }
        else{
          results_pgc_db <- isolate(testEnrichment(cpg_ids_vector,platform =  input$selected_platform))
        }
        results_pgc_db$gene_name <- NA
        
        # Handle the case of if Gene Association checkbox is checked
        if(input$geneAssociation){
          results_pgc_gene <- testEnrichmentGene(cpg_ids_vector,platform = input$selected_platform)
          if(nrow(results_pgc_gene)>0){
            results_pgc_gene$label <- NA
            results_pgc_gene$n_min <-NA
            results_pgc<-rbind(results_pgc_db,results_pgc_gene)
          }
          else{
            results_pgc<- results_pgc_db
          }
        }
        else{
          results_pgc<- results_pgc_db
        }
        
      output$distPlot <- renderGirafe({
        tryCatch({
          ggcode <- KYCG_plotEnrichAll_interactive(results_pgc)

          x <- girafe(code = print(ggcode),
                      width_svg = 12, height_svg = 5,#10
                      options = list(
                        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE),
                        opts_selection(
                          type = "multiple", css = "fill:#FF3333;stroke:black;")
                      ))
          x},error = function(e) {
            showNotification('There was an error generating plot.','',type = "error")
            return()
          }
          )
      })

        dbgroups_under_analysis <-results_pgc %>% group_by(group) %>% summarise(min=min(FDR,na.rm=TRUE)) %>% arrange(min) %>%
          tidyr::extract(col=group,regex ="^(\\w+).(\\w+).(.+)\\.(\\w+)$",into=c("kycg","platform","groupName",
                                                                                   "group_suffix"),remove=FALSE)
      
      output$dynamic_tabs <- renderUI({
        if(is.null(dbgroups_under_analysis)){return ()}
        groups = dbgroups_under_analysis$groupName
        myTabs = unname(Map(tabPanel, groups))
        do.call(navlistPanel , c(myTabs, id="grouptab"))#tabsetPanel
      })

      output$dotPlot <- renderGirafe({ #renderPlot
        if(is.null(dbgroups_under_analysis) || is.null(input$grouptab)){return ()}
        
        tryCatch({
          group_fullnames_df <- dbgroups_under_analysis %>% filter(groupName==input$grouptab) %>% select(group) 
          group_fullname <- group_fullnames_df$group[1]
          results <- results_pgc %>% filter(group==group_fullname)
          #results <- testEnrichment(cpg_ids_vector, input$grouptab)
          
          dot_ggcode <- KYCG_plotDot_interactive(results)
          
          y <- girafe(code = print(dot_ggcode),
                      width_svg = 7, height_svg = 7,
                      options = list(
                        opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE),
                        opts_selection(
                          type = "multiple", css = "fill:#FF3333;stroke:black;")
                      ))
          y
        },error = function(e) {
          showNotification('There was an error generating plot.','',type = "error")
          return()
        })
      })
      }
      },
      error = function(e) {
        showNotification('There was an error due to the specific combination of inputs.','',type = "error")
        return()
      },
      finally={
        remove_modal_spinner()        
      })

      # Downloadable csv of dataframe ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("Results", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(results_pgc, file, row.names = FALSE)
        }
      )

      # Downloadable csv of dataframe behind Overview dotplot----
      output$downloadDataOverview <- downloadHandler(
        filename = function() {
          paste("OverviewDotplotData", ".csv", sep = "")
        },
        content = function(file) {
          sub_results <- results_pgc
          sub_results$"-log10(FDR)" <- -log10(sub_results$FDR)
          sub_results$"log2(OR)" <- sub_results$estimate
          sub_results$dbname[sub_results$group == "gene"] <- sub_results$gene_name[sub_results$group == "gene"] 

          sub_results <- sub_results %>%
            tidyr::extract(col=group,regex ="^(\\w+).(\\w+).(.+)\\.(\\w+)$",
                           into=c("kycg","platform","group","group_suffix"),remove=TRUE )   
          sub_results$dbname[sub_results$group == "gene"] <- sub_results$gene_name[sub_results$group == "gene"] 
          sub_results <- sub_results %>% select(group,dbname, "-log10(FDR)", "log2(OR)")

          write.csv(sub_results, file, row.names = FALSE)
        }
      )
    
      
      # Downloadable csv of dataframe behind ByGroup dotplot----
      output$downloadDataByGroup <- downloadHandler(
        filename = function() {
          paste(input$grouptab,"DotplotData", ".csv", sep = "")
        },
        content = function(file) {
          sub_results <- results_pgc
          sub_results$"-log10(FDR)" <- -log10(sub_results$FDR)

          sub_results$dbname[sub_results$group == "gene"] <- sub_results$gene_name[sub_results$group == "gene"] 
          
          sub_results <- sub_results %>%
            tidyr::extract(col=group,regex ="^(\\w+).(\\w+).(.+)\\.(\\w+)$",
                          into=c("kycg","platform","group","group_suffix"),remove=TRUE )   %>%
                          filter(group==input$grouptab) 
          sub_results$dbname[sub_results$group == "gene"] <- sub_results$gene_name[sub_results$group == "gene"] 
          sub_results <- sub_results %>%  select(group,dbname, "-log10(FDR)", overlap, estimate)
          
          write.csv(sub_results, file, row.names = FALSE)
        }
      )
      
      
        
      }
    )

    

}
