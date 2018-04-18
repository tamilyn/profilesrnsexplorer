#

shinyServer(function(input, output) {

  authorData <- reactive({
    df <- authors %>% select(-lname, -fname, -author, -label)
    if(input$grant_status_filter == "unfunded") {
       df <- df %>% filter(grant_status == "unfunded") 
    } else if (input$grant_status_filter == "funded") {
       df <- df %>% filter(grant_status == "funded") 
    } else if (input$grant_status_filter == "applicant") {
       df <- df %>% filter(grant_status %in% c("funded","unfunded")) 
    } else if (input$grant_status_filter == "non applicant") {
       df <- df %>% filter(grant_status == "non_applicant") 
    }
    print(str_c("authorData: " , input$grant_status_filter))
    #sample_n(df, size = 20, replace = FALSE)
    df
  })

  authorDataCaption <- reactive({
    str_c( "ProfilesRNS Authors [", input$grant_status_filter, "]")
  })

  output$authorTable <- renderDataTable ({
    df <- authorData() 
    DT::datatable(df, caption = authorDataCaption())
  })


  ## Documents tab
  documentsData <- reactive({
     df <- author_pubs

     #use filtered authors data (by grant status)
     authors_df <- authorData() 
     if(input$grant_status_filter != "All") {
        df1 <- df %>% filter(personid %in% authors_df$personid)
        print(str_c("by status before ", nrow(df), " after ", nrow(df1)))
        message(str_c("documents by grant status: before ", nrow(df), " after ", nrow(df1)))
        df <- df1
     }

     show_authors <- input$author_filter
     if(show_authors != "All") {
         pid <- authors_df %>% filter(fullname == show_authors) %>% pull(personid)
         df <- df %>% filter(personid == pid)
     }

     df <- df %>%
       select(-year, -personid, -publicationVenue, -medlineTA, -title, -author) %>%
       select(-infoResource, -authorRank) %>%
       rename(Description = reference, publishDate = pubdate)
     #df <- all_authors %>%
     #  select(document_id = article_dc_identified,
     #         author_id = `@auid`, `@seq`,
     #         aff_id, indexed = `ce:indexed-name`) %>%
     #  mutate(author_rank = as.integer(`@seq`)) %>%
     #  left_join(uniq_article, by=c("document_id"="dc:identifier")) %>%
     #  select(title, publication, publishDate, indexed,
     #         author_rank, document_id, author_id, everything())

#    if(input$has_pubmed == TRUE) {
#       df <- df %>% dplyr::filter(!is.na(`pubmed-id`))
#    }

#    if(input$all_ranks == FALSE) {
#
#      if(input$only_rank_1 == TRUE) {
#          df <- df %>% dplyr::filter(author_rank == 1)
#      } else {
#         max_rank <- as.numeric(input$only_rank_less_than_equal_to)
#         if(input$only_rank_1 == TRUE) {
#           df <- df %>% dplyr::filter(author_rank <= max_rank)
#         }
#       }
#    }

    d1 <- input$articleDateRange[1]
    d2 <- input$articleDateRange[2]

    df <- df %>% dplyr::filter(publishDate > d1, publishDate < d2)
    df
  })

  documentsDataCaption <- reactive({
    grant_filter <- ""
    if(input$grant_status_filter != "All") {
      grant_filter <- str_c("[Author Grant Status: ", input$grant_status_filter, "]")
    }
    pubmed_filter <- ""
    if (input$has_pubmed == TRUE) {
      pubmed_filter <- "(with pubmed ids)"
    }

    if(input$author_filter != "All") {
       author_filter <- str_c("[Author = ", input$author_filter, "]")
    } else {
       author_filter <- ""
    }
    rank_filter <- ""
    if(input$all_ranks == FALSE) {
      rank_filter <- "[Only showing ranks in range]"
    }

    d1 <- input$articleDateRange[1]
    d2 <- input$articleDateRange[2]
    date_filter <- str_c("[Between ", d1, " and ", d2, "]")
    str_c("Documents ", author_filter, grant_filter, pubmed_filter, rank_filter, date_filter)
  })

  coauthorData <- reactive({

    # only include the documents written by our filtered authors
    # select from info_resource_author
    # we want the 'author' url
    documents <- authorsData() %>%    
       left_join(info_resource_author, by=c("author"="author")) # %>%
       #left_join(coauthor_data
;; infoResource


    print(str_c("112: COAUTHOR DATA ", nrow(coauthor_data)))
    # only include from where from is one of the selected authors

#    coauthor_data <- coauthor_links %>%
#        filter(from != to) %>%
#        mutate(min_one = ifelse(from < to, from, to),
#               max_one = ifelse(from >= to, from, to),
#               from_to = str_c(min_one,"-",max_one)) %>%
#        group_by(from_to) %>%
#        summarise(n = sum(n)) %>%
#        separate(from_to, into=c("from","to"), sep="-")

    print(str_c("124: COAUTHOR_DATA: ", nrow(coauthor_data)))

    #max_rank <- as.integer(input$max_filter_rank)
    #coauthor_data <- author_rankings %>%
    # filter(rank <= max_rank)
    # apply filters
    #print(str_c("initial num: ", nrow(max_rank), " Now " , nrow(coauthor_data)))

    #dd <- documentsData() %>% pull(document_id)
    #print(str_c("Coauthors - only include for documents: ", length(dd) ))

    #coauthor_data <- coauthor_data %>% filter(article_id %in% dd)
    #print(str_c("Coauthors - now : ", nrow(coauthor_data) ))

    coauthor_data
  })

  coauthorDataCaption <- reactive({
      d <- coauthorData()
      str_c("All authors for the selected documents. ", input$max_filter_rank)
  })

  output$coauthorTable <- renderDataTable ({
    DT::datatable(coauthorData(), caption = coauthorDataCaption())
  })


  output$documentsTable <- renderDataTable ({
      DT::datatable(documentsData(), caption = documentsDataCaption())
  })

  output$rankingHistogram <- renderPlotly({
     r <- coauthorData()
     ggplot(r) + geom_histogram(aes(x = rank))
  })

  output$plot1 <- renderPlotly({
     print("RENDER PLOTLY")
     #by grant status, by publication, by year,

     date_title <- "Selected Date Range"

     af_df <- authorData() %>%
        mutate(author_id = str_replace_all(author_id, "^AUTHOR_ID:", ""))

     dd_df <- documentsData() %>%
        select(-aff_id)

     df1 <- dd_df %>%
        left_join(af_df, by=c("author_id"="author_id"))

     df <- df1 %>%
        count(publication)

     ggplot(df) +
       geom_bar(aes(x = publication, y = n), stat="identity") +
       theme_minimal() +
       #theme(axis.text.x = element_text(angle = 90)) +
       #coord_flip() +
       ggtitle("Publications By Publication Name")
  })

  output$articleDateRangeText <- renderText({
          browser()
    paste("input$articleDateRange is",
      paste(as.character(input$articleDateRange), collapse = " to ")
    )
  })

  output$rankingOverview <- renderPrint({
     r <- coauthorData()
     print(summary(r$rank))
  })


  output$graphOverview <- renderPrint({
     print(str_c("**** Will only create graph if < ", max_num, " edges ****"))
  })

  xx_make_edge_graph <- function(edges, title = "Sample Graph") {
    require(ggraph)
    require(tidygraph)
    require(tidyverse)
    require(igraph)

  ad <- authorData() 

  aa <- authorData() %>% 
    mutate(sn = str_c(str_sub(firstname,1,1), str_sub(lastname, 1, 4)))

  new_aa <- sample_n(aa, size = 20, replace = FALSE)
  old_aa <- aa
  aa <- new_aa

  aapersons <- aa %>% distinct(personid) %>% pull(personid) 
  print(str_c("NUM PERSONS: ", length(aapersons)))

  ee <- edges %>% filter(from %in% aapersons|to %in% aapersons) 
  print(str_c("EE: ", nrow(ee), " edges: ", nrow(edges)))
  if(nrow(ee) == 0) {
    print("NO EDDGETS")
    return(NULL)
  }

  print(colnames(ad))
  print(colnames(ee))
  print(str_c("AD: ", nrow(ad)))
  ad1 <- authors %>% 
       mutate(sn = str_c(str_sub(firstname,1,1), str_sub(lastname, 1, 4))) %>%
       mutate(id = personid, label=sn) %>% 
       select(id, label, everything())

  browser()
  g <- tbl_graph(directed = FALSE, nodes = ad1, edges = ee)  %>%
    mutate(Popularity = centrality_degree())  

  ggraph(g, layout = 'kk') +
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
    geom_node_point(aes(size = Popularity, colour = grant_status), show.legend = FALSE) +
    geom_node_label(aes(label = sn, color = grant_status, size=8)) +
    geom_edge_link() + 
    #facet_edges(~grant_status) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
    ggtitle(title)
}

  output$graphPlot <- renderPlot({
     print(str_c("**** Will only create graph if < ", max_num, " edges ****"))

     #min_weight <- as.numeric(input$min_weight)
     min_weight <- 1
     coauthors <- coauthorData() %>% arrange(-n)
     subset_coauthors <- coauthors %>%
         filter(n > min_weight)

     print(str_c("Num coauthors: ", nrow(subset_coauthors)))
     print(colnames(coauthors))
     if(nrow(subset_coauthors) > max_num) {
        subset_coauthors <- coauthors %>% slice(1:max_num)
     }
     print(str_c("** FILTER BY WEIGHT: ", nrow(subset_coauthors)))
     gguf <- xx_make_edge_graph(subset_coauthors,"Filter By Coauthor Weight")

     tm <- str_replace_all(str_c(lubridate::ymd_hms(lubridate::now())),":|-","") %>%
          str_replace_all(" ","_")

     graph_filename <- str_c("edge_graph_", tm, "_", min_weight, ".png")
     ggsave(graph_filename, plot = gguf, dpi = 300, width = 20, height = 20)
     return(gguf)
  })

  observeEvent(input$filterButton, {
    print(str_c("filters ", input$filterGroup))
    authorData()
  })

  observeEvent(input$createSampleButton, {
    print(str_c("creating a sample ", input$sample_size))
    authorData()
  })


})
