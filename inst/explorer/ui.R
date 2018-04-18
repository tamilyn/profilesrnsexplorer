
top_part <- tags$nav(class="navbar navbar-default navbar-fixed-top",
  div(class="container",
    tags$ul(class="nav navbar",
      tags$li(class="navbar-text navbar-brand", "Explorer"),
      tags$li(class="navbar-text", tags$b(textOutput("statusMessage"))))))

prep_sidebar <- sidebarPanel(
     p("Dataset parameters")
     , p("Use the whole data set?")
     , p("create a Use the whole data set?")
     , p("Sample")
     , p("select everyone, funded, unfunded, sampled f+u, sample f, sample u")
     , p("what filter for ranking (show summary stats and histogram")
)

prep_rank_panel <- mainPanel(
  p("Configure Author Ranks. Some publications have large number of coauthors, which can skew the results.  Here you can control how many coauthors per publications are used.")
       #, helpText('Output of the examples in the left:')
   , textInput("max_filter_rank",
     "Discard any author rankings greater than ", value = "25")
   , verbatimTextOutput('rankingOverview')
   , plotlyOutput('rankingHistogram')
)

article_sidebar <- mainPanel(
     p("Select dataset - all or a random sample basd on funded/unfunded"),

     p("Filters"),
     selectInput("author_filter",
       "Only show articles for author",
       choices = author_names_choices,
       selected = "All" )

     , selectInput("grant_status_filter",
       "Show by grant status",
       choices = grant_status_choices,
       selected = "All" )

     , checkboxInput("use_sample", "Limit to sample", value = FALSE)
     , textInput("sample_size", "Sample size", value = "20") 
     , actionButton("createSampleBtn", "Create Sample") 

     , checkboxInput("has_pubmed",
       "Only show articles with PUBMED Identifier", value = FALSE)
     , checkboxInput("only_rank_1",
       "Only show when author is first author", value = FALSE)
     , checkboxInput("all_ranks",
       "Show all author rankings", value = TRUE)
     , textInput("only_rank_less_than_equal_to",
       "Only show when author rank is <= ", value = "99999")
     , dateRangeInput('articleDateRange',
          label = paste('Only show articles within this date range'),
          start = lubridate::date("1900-01-01"), end = Sys.Date() + 365,
          min = lubridate::date("1900-01-01"), max = Sys.Date() + 365,
          separator = " - ",
          format = "yyyy/mm/dd",
          startview = 'year'
       ))

prep_panel <- tabsetPanel(
       tabPanel("Document Details",
           mainPanel(
           p("doc details here.  You should be able to select (HERE ARE SOME POSSSIBLE OPTIONS) whether to use all available documents,(AVAILABLE FROM SCOPUS or AVAILABLE from ProfilesRNS)  documents by grant applicant status - funded or unfunded, or a sample of any those or a specific list of pubmed ids that is input via a test box or uploaded"),
        article_sidebar))
 )

# UI for SCOPUS Author affiliations
shinyUI(fluidPage(

  useShinyjs(),

  tags$head(
    tags$title('Article Explorer'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css')),

  #top_part,

  tabsetPanel(
    tabPanel("Setup", prep_panel )
    , tabPanel("Documents",
      mainPanel(dataTableOutput("documentsTable")))
    , tabPanel("Authors" , mainPanel(dataTableOutput("authorTable")))
    , tabPanel("Coauthors",
        mainPanel(dataTableOutput("coauthorTable")))
    , tabPanel("Graph",
        mainPanel(
           p("Graph of coauthors")
    #       , textInput("min_weight",
    #             "Discard coauthor weights below", value = "25")
           , verbatimTextOutput('graphOverview')
           , plotOutput('graphPlot')
           ))
  )))
