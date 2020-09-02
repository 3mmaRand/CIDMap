
################################## SET UP ####################################
# Packages
library(shiny)
library(tidyverse)
library(plotly)

# palette
pal <- viridisLite::viridis(7)

############################# MAIN DATA SET ##################################
# IMPORT
# File
file <- "data/checked_processed_aw_corrections.csv"

# import full display names in first row of csv (gives empty dataframe)
display_names <- read_csv(file, n_max = 0)

# make a vector of display names
display_names <- names(display_names) %>% stringr::str_wrap(width = 40)

# import data from row 2
dat <- read_csv(file, skip = 1) 

# vector of actual variable names
varnames <- names(dat)
# give labels to the variable names using the display names
# this will be a reference throughout the app so figures are created
# with display names rather than actual variable names
names(varnames) <- display_names

# TIDY
# levelling factors

# TIDY
# levelling factors
dat$test_results <- fct_relevel(dat$test_results,
                                "1. % by batch reported",
                                "2. Average % reported",
                                "3. Tests done but % not reported",
                                "4. Stated as 'standard phenotype'",
                                "5. No characterisation discussed")
dat$MOA <- fct_relevel(dat$MOA,
                       "Paracrine",
                       "Immune",
                       "Differentiation",
                       "Multiple",
                       "Not stated")

dat$stem_stromal <- fct_relevel(dat$stem_stromal,
                                "Stromal",
                                "Stem",
                                "Multipotent Stromal",
                                "Regenerative")

dat$O <- fct_relevel(dat$O,
                     "Not mentioned",
                     "Performed, no value reported",
                     "Performed, value reported")

dat$A <- fct_relevel(dat$A,
                     "Not mentioned",
                     "Performed, no value reported",
                     "Performed, value reported")

dat$C <- fct_relevel(dat$C,
                     "Not mentioned",
                     "Performed, no value reported",
                     "Performed, value reported")
dat$other <- fct_relevel(dat$other,
                         "Not mentioned",
                         "Performed, no value reported",
                         "Performed, value reported")

# turn year into a factor
dat$year <- factor(dat$year)

# turn ref in to a factor
dat$ref_number <- factor(dat$ref_number) 

# urls to clickable links
dat$ref_url <- paste0("<a href='", dat$ref_url, "' target='_blank'>", dat$ref_url,"</a>")


########################### CD VALUES DATA SET ###############################
# IMPORT
# two files one for publications that gave data for all participants (single)
# and the other for publications that reported the mean and se (range)
# note - I have assumed range data are mean +/- se (not sd)

file <- "data/cd_values_single_processed.csv"
cd_single <- read_csv(file)

file <- "data/cd_values_range_processed.csv"
cd_range <- read_csv(file) %>% select(-Phase)

# TIDY AND SUMMARISE
# summarise the data for participants in a study
cd_summary <- cd_single %>% 
  group_by(ref_number, marker) %>% 
  summarise(mean_value = mean(value),
            n = length(value),
            se_value = sd(value)/sqrt(n))

# bind the range and the single value dataframes
# and omit the na values
cd_summary <- rbind(data.frame(cd_summary), cd_range) %>% 
  filter(!is.na(mean_value))

# vector for the ISCT phenotypes
isct <- c("CD73",
          "CD90",
          "CD105",
          "CD34",
          "CD45",
          "CD11b",
          "CD14",
          "CD79a",
          "CD19", 
          "HLA-DR")

# add variable to indicate whether a marker is ISCT or other
cd_summary$ISCT <- "Other marker"
cd_summary$ISCT[cd_summary$marker %in% isct] <- "ISCT phenotype marker"

cd_summary$ISCT <- fct_relevel(cd_summary$ISCT,
                               "ISCT phenotype marker",
                               "Other marker")


####################### PREPARATION FOR UI AND SERVER ########################

################################## TAB 1 #####################################
x_var_choice_names <- c("Year",
                        "Country in which trial conducted",
                        "Continent in which trial conducted",
                        "Clinical trial phase (detail)",
                        "Clinical trial phase (broad)",
                        "Mechanism of action",
                        "ISCT compliance claimed",
                        "ISCT compliance demonstrated",
                        "Indication class",
                        "Route of administration",
                        "Source of MSC",
                        "Stem or stromal cell",
                        "Allogeneic or autologous",
                        "Donor sex",
                        "Donor age",
                        "Stringency of characterisation",
                        "Osteogenic differentiation capacity",
                        "Adipogenic differentiation capacity",
                        "Chondrogenic differentiation capacity",
                        "Other functionality assay",
                        "Co-occurrence of differentiation capacity",
                        "Differentiation")
fill_var_choice_names <- c("None",
                           "Clinical trial phase (broad)",
                           "Mechanism of action",
                           "ISCT compliance claimed",
                           "ISCT compliance demonstrated",
                           "Source of MSC",
                           "Stem or stromal cell",
                           "Allogeneic or autologous",
                           "Donor sex",
                           "Donor age",
                           "Stringency of characterisation",
                           "Osteogenic differentiation capacity",
                           "Adipogenic differentiation capacity",
                           "Chondrogenic differentiation capacity",
                           "Other functionality assay",
                           "Differentiation")



############################ Tab 2 ##########################################
#                 Characterisation extent by variable
x_var_choice_names2 <- c("Year",
                         "Continent in which trial conducted",
                         "Clinical trial phase (detail)",
                         "Clinical trial phase (broad)",
                         "Mechanism of action",
                         "ISCT compliance claimed",
                         "ISCT compliance demonstrated",
                         "Indication class",
                         "Source of MSC",
                         "Stem or stromal cell",
                         "Allogeneic or autologous",
                         "Donor sex",
                         "Donor age",
                         "Osteogenic differentiation capacity",
                         "Adipogenic differentiation capacity",
                         "Chondrogenic differentiation capacity",
                         "Other functionality assay")
characterisation_choice_names <- c("Number of characterisation attribute\ntests performed, value reported",
                                   "Number of characterisation attribute\ntests performed, no value reported",
                                   "Number of characterisation attribute\ntests performed, total")


############################ Tab 3 ####################################
#              Characterisation extent by each attribute.
#          Characterisation extent<br/>by each attribute and trial


# select the test columns and count the number of articles for 
# each marker and category
test_cols <- names(select(dat, ends_with("_test")))

test_by_attribute <- dat %>% 
  select(all_of(test_cols), ref_number) %>% 
  pivot_longer(cols = -ref_number, 
               names_to = "marker", 
               values_to = "status")

# process the names to remove _test

test_by_attribute <- test_by_attribute %>% 
  mutate(marker2 = str_replace(marker, "_test", ""))

# change levels of status and markers.
# markers
# process test_col names
test_cols <- str_replace(test_cols, "_test", "")
# set the levels in that order
test_by_attribute$marker2 <- factor(test_by_attribute$marker2, 
                                    levels = test_cols)

# status
# recode status
test_by_attribute <- test_by_attribute %>%
  mutate(test_status = recode_factor(status,
                                     p = "Performed, value reported",
                                     m = "Performed, no value reported",
                                     n = "Not mentioned"))

test_by_attribute$test_status <- fct_relevel(test_by_attribute$test_status,
                                             "Not mentioned",
                                             "Performed, no value reported",
                                             "Performed, value reported")


test_by_attribute_summary <- test_by_attribute %>% 
  group_by(marker2, test_status) %>% 
  summarise(n = length(marker2))




############################ Tab  ##########################################


##########################  USER INTERFACE UI  ###############################

ui <- fluidPage(
  # * Input() functions
  # all input functions have a unique name given in inputId and a label
  # which can be "" and additional input specific inputs
  fluidRow(column(3,
                  img(src = "hex-CIDMap.png", width = "100%")),
           column(6, h1("Clinical trial identifiers for MSCs"),
                  h3("A shiny app to explore the characterisation of mesenchymal stromal cells in clinical trial reports")),
           column(3,
                  h2("Genever Lab"),
                  p("Department of Biology"),
                  p("University of York"),
                  p("York YO10 5DD"),
                  p(a("https://www.geneverlab.info/", href = "https://www.geneverlab.info/")),
           )),
  br(),
  tags$a(
    href = "https://doi.org/10.5281/zenodo.4012398", 
    tags$img(src = "https://zenodo.org/badge/DOI/10.5281/zenodo.4012398.svg", alt="DOI")),
  p("Prepared by",
    a("Emma Rand", href = "mailto:emma.rand@york.ac.uk"),
    "in support of:"), 
  h3(a("Wilson, A.J.,", href = "mailto:ajw638@york.ac.uk"),
    "Rand, E., Webster, A.J. & Genever, P.G. (2020)", em( "Characterization of mesenchymal stromal cells in clinical trial reports: analysis of published descriptors."),
    "Manuscript submitted"),
  p(strong("Abstract:"),"Mesenchymal stem or stromal cells (MSCs) are the most widely used cell therapy to
date. They are heterogeneous, with variations in growth potential, differentiation capacity and
protein expression profile depending on tissue source and production process. Nomenclature and
defining characteristics have been debated for almost 20 years, yet the generic term “MSC” is
used to cover a wide range of cellular phenotypes. Against a documented lack of definition of
cellular populations used in clinical trials, our study evaluated the extent of characterization of
the cellular population or study drug. A literature search of clinical trials involving mesenchymal
stem/stromal cells was refined to 84 papers upon application of pre-defined inclusion/exclusion
criteria. Thirty-two studies (38.1%) include no characterization data whatsoever. Forty-one
(48.8%) reported average values per marker for all cell lots used in the trial, and only eleven
(13.1%) studies included individual values per cell lot. Viability was reported in 57% of studies.
Differentiation was discussed: osteogenesis (29% of papers) adipogenesis (27%) and
chondrogenesis (20%); and other functional assays arose in 6 papers (7%). Extent of
characterization was not related to clinical phase of development. Assessment of functionality
was very limited and did not always relate to likely mechanism of action. We discuss the
potential implications of these findings for the use of MSCs in regenerative medicine, and the
importance of characterization for transparency and comparability of literature."),
  hr(), 
  h3("Visualisation tools"),
  tabsetPanel(
    ## TAB 1
    tabPanel(HTML("Clinical trial<br/>information"),
             sidebarLayout(
               sidebarPanel(width = 2,
                            selectInput(inputId = 'xaxisvar', 
                                        label = 'Number of articles for each category of:', 
                                        choices = x_var_choice_names,
                                        selected = x_var_choice_names[1]),
                            selectInput(inputId = 'flip',
                                        label = 'Flip the axes?',
                                        choices = c("No", "Yes"),
                                        selected = "No"),
                            selectInput(inputId = 'fillvar', 
                                        label = 'Choose an additional fill variable', 
                                        choices = fill_var_choice_names, 
                                        selected = fill_var_choice_names[1]),
                            checkboxInput("bar_chart_relative", "Relative display"),
                            helpText("Check to see the fill variable proportionally.")
                            
               ),
               mainPanel(
                 h4("Barplots for categorical variables"),
                 p("These barplots allow you to explore the number of trials for various categorical variables."), 
                 p("Where there are many categories, or category names are long, barplots with flipped axes are often clearer. You can select an additional categorical variable using the fill option. A test for the association between the x-axis variable and the fill variable is given below the plot."),
                 plotlyOutput(outputId = "summaryplot", height = "400px"),
                 br(),
                 textOutput(outputId = "associationtest")
               )
             )
    ),
    ## TAB 2
    tabPanel(HTML("Characterisation<br/>extent by variable"),
             sidebarLayout(
               sidebarPanel(width = 3,
                            selectInput(inputId = 'characterisation', 
                                        label = 'Characterisation measure', 
                                        choices = characterisation_choice_names, 
                                        selected = characterisation_choice_names[1]),
                            h4("By categorical variable."),
                            selectInput(inputId = 'xaxisvar_cat', 
                                        label = 'Number of characterisations for each category of:', 
                                        choices = x_var_choice_names2,
                                        selected = x_var_choice_names2[1]),
                            selectInput(inputId = 'flip2',
                                        label = 'Flip the axes?',
                                        choices = c("No", "Yes"),
                                        selected = "No")
                            
               ),
               mainPanel(
                 p("Each point represents a publication - hover over the point for the publication id number. The extent of characterisation is given by the number of tests done without values being reported, with values being reported, or the sum of these (total)."),
                 h4("Boxplots for categorical variable"),
                 p("A test for a difference in the extent of characterisations between different levels of the categorical variable is given below the plot."), 
                 plotlyOutput(outputId = "marker_counts_cat", height = "400px"),
                 br(),
                 textOutput(outputId = "kwtest"),
               )
             )
    ),
    ## TAB 3
    tabPanel(HTML("Characterisation extent<br/>by each attribute"),
             mainPanel(
               p("The number of trials testing for an attribute and reporting values, testing without reporting values or not testing for each characterisation attribute."),
               plotlyOutput(outputId = "teststatus_prevalence", height = "500px"),
               p("In detail for each trial. The box indicates ISCT markers"),
               plotOutput(outputId = "teststatus_by_marker_detail", width = "700px")
             )
    ),
     ## TAB 4
    tabPanel(HTML("Reported percent cells<br/>expressing attribute"),
             mainPanel(
               br(),
               p("Each point represents a publication - hover over the point for the publication id number."), 
               p("Where there are no error bars - the publication reported average values for all cell lots used in the trial."),
               p("Where error bars are included - either the standard error on the average values for all cell lots used in the trial reported by the publication or the standard error calculated from individual values reported per cell lot."),
               h3("ISCT markers"),
               plotlyOutput(outputId = "cd_isct", height = "400px"),
               h3("Other markers"),
               p("The following markers were not reported in any paper: CD133, CD146, CD271, STRO-1, MSCA-1, SSEA-4"),
               plotlyOutput(outputId = "cd_other", height = "400px")
             )
    ),
    ## TAB 5
    tabPanel("Complete dataset",
             sidebarLayout(
               sidebarPanel(width = 3,
                            checkboxGroupInput("show_vars", 
                                               "Columns to show:",
                                               display_names, 
                                               selected = display_names[c(3, 6, 9, 14)]),
               ),
               mainPanel(
                 div( DT::dataTableOutput(outputId = "complete_data"), style = "font-size:80%"),
                 # Button
                 downloadButton("downloadData", "Download complete dataset")
               )
             )
    ),
    ## TAB 7
    tabPanel(HTML("Figures from<br/>the paper"),
             fluidRow(
               column(width = 6,
                      "Fig. 1. Literature Search Strategy and Results. (A) The schematic shows search terms, refinements and exclusions used. Numbers refer to the total number of papers remaining at each stage. (B) Reported characteristics for MSCs in clinical research studies: data elements captured for this analysis. Basic information on the trial included clinical phase, indication, route of administration and mechanism(s) of action. Specifics of the cell source included donor details, tissue source and usage (allogeneic/autologous) and the descriptor used by the study: stem/stromal cells or other nomenclature. Aspects of characterization reported in the study were captured, focusing on assessment of viability, phenotypic profile, differentiation capacity and potency evaluations. Reference to ISCT minimal criteria for identification of MSC was also recorded."
               ),
               column(width = 6,
                      a(img(src = "paper_figs/fig_1.svg", height = "300px", alt = "Fig. 1. Literature Search Strategy and Results"), href = "paper_figs/fig_1.svg")
               )
             ),
             hr(),
             fluidRow(
               column(width = 6,
                      "Fig. 2. Background Trial Information. (A) Origin of clinical research publications, ranked by number from each country represented in the analysis. (B) Clinical trials reported in literature by clinical phase, ranked by most commonly represented phase of clinical study. (C) Route of administration, ranked by most commonly used in the studies. (D) Indications addressed by the clinical studies, ranked by most commonly represented indication."
               ),
               column(width = 6,
                      a(img(src = "paper_figs/fig_2_black.svg", height = "300px", alt = "Fig. 2. Background Trial Information"), href = "paper_figs/fig_2_black.svg")
               )
             ),
             hr(),
             fluidRow(
               column(width = 6,
                      "Fig. 3. Background information on cells used in clinical trials. (A) Sources of tissue from which MSCs were derived. (B) Reported use of autologous and allogeneic MSCs (C) Nomenclature used to describe the cells used in the clinical trials."
               ),
               column(width = 6,
                      a(img(src = "paper_figs/fig_3_black.svg", height = "300px", alt = "Fig. 3. Background information on cells used in clinical trials"), href = "paper_figs/fig_3_black.svg")
               )
             ),
             hr(),
             fluidRow(
               column(width = 6,
                      "Fig. 4. Extent and stringency of characterization. (A) Number of articles reporting each category of characterization. (B) Stringency of characterization reported at each clinical phase of development (coloured as in A). (C)  Number of phenotypic markers, and viability, evaluated in articles that reported values/averages."
               ),
               column(width = 6,
                      a(img(src = "paper_figs/fig_4.svg", alt = "Fig. 4. Extent and stringency of characterization"), href = "paper_figs/fig_4.svg")
               )
             ),
             hr(),
             fluidRow(
               column(width = 6,
                      "Fig.5. Phenotypic characterization and viability. The minimal criteria recommended by ISCT for identification of MSC are shown between the black bars on the y-axis. (A) Analysis of individual markers reported in the clinical data set, showing whether an attribute was performed with results reported, whether it was performed but no results stated, or not mentioned in the study report. (B) Number of studies that addressed each attribute, defined by extent of reporting for each marker. Required expression or absence of a marker according to the ISCT recommendation is indicated on the y-axis."
               ),
               column(width = 6,
                      a(img(src = "paper_figs/fig_5.svg", alt = "Fig.5. Phenotypic characterization and viability"), href = "paper_figs/fig_5.svg")
               )
             ),
             hr(),
             fluidRow(
               column(width = 6,
                      "Fig. 6. Differentiation and other functionality assessments. (A) Frequency of functionality assessments. (B) Nomenclature (stem/stromal) in relation to potential mechanism of actions relevant to each study indication. (C) Evaluation of MSC differentiation capacity (multi-potentiality) in relation to the mechanism of action anticipated for each study."
               ),
               column(width = 6,
                      a(img(src = "paper_figs/fig_6.svg", alt = "Fig. 6. Differentiation and other functionality assessments"), href = "paper_figs/fig_6.svg")
               )
             ),
             hr(),
            
             
    )
  )
)



##########################  Server  ###################################

server <- function(input, output) {
  # for tab 1
  fig <- reactive({
    if (input$fillvar == "None") {
      ggplot(dat, aes_string(x = varnames[input$xaxisvar])) +
        geom_bar(size = 0, 
                 fill = pal[5])  +
        scale_y_continuous(name = "Number of articles",
                           expand = expansion(c(0, 0.1))) +
        scale_x_discrete(expand = c(0, 0), name = input$xaxisvar) +
        theme_classic() +
        theme(axis.text = element_text(size = 10, colour = "black"),
              axis.title = element_text(size = 10, colour = "black"),
              legend.text = element_text(size = 10, colour = "black"),
              legend.title = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank()) 
    }
    else {
      if (!input$bar_chart_relative) {
        ggplot(dat, aes_string(x = varnames[input$xaxisvar], 
                               fill = varnames[input$fillvar])) +
          geom_bar(size = 0)  +
          scale_fill_manual(values = pal) +
          scale_y_continuous(name = "Number of articles",
                             expand = expansion(c(0, 0.1))) +
          scale_x_discrete(expand = c(0, 0), 
                           name = input$xaxisvar) +
          theme_classic() +
          theme(axis.text = element_text(size = 10, colour = "black"),
                axis.title = element_text(size = 10, colour = "black"),
                legend.text = element_text(size = 10, colour = "black"),
                legend.title = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank())
      } else {
        ggplot(dat, aes_string(x = varnames[input$xaxisvar],
                               fill = varnames[input$fillvar])) +
          geom_bar(size = 0, position = "fill")  +
          scale_fill_manual(values = pal) +
          scale_y_continuous(name = "Proportion of articles",
                             expand = expansion(c(0, 0.1))) +
          scale_x_discrete(expand = c(0, 0), 
                           name = input$xaxisvar) +
          theme_classic() +
          theme(axis.text = element_text(size = 10, colour = "black"),
                axis.title = element_text(size = 10, colour = "black"),
                legend.text = element_text(size = 10, colour = "black"),
                legend.title = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank())
        
      }
    }
  })
  
  output$summaryplot <- renderPlotly({
    if (input$flip == "No") {
      ggplotly(fig() ) %>%
        layout(legend = list(orientation = "v",
                             x = 1.8,
                             xanchor = "right",
                             y = 1,
                             title = list(text = input$fillvar)))
    }
    
    else {
      ggplotly(fig() + 
                 coord_flip()) %>%
                 layout(legend = list(orientation = "v",
                                      x = 1,
                                      xanchor = "left",
                                      y = 1,
                        title = list(text = input$fillvar))  )
      
    }
    
  })
  
  # association between x var and fill var 
  results <- reactive({
    if (input$fillvar == "None") {
      "Choose a fill variable to test for an association  between variables"
      
    }
    else{
      dat %>% 
        select(varnames[input$xaxisvar], varnames[input$fillvar]) %>% 
        table() %>%
        fisher.test(simulate.p.value = TRUE)
    }
  })
  
  output$associationtest <- renderText({
    if (input$fillvar == "None") {
      print(results())
    }
    else {
      if (results()$p.value > 0.05) {
        print(paste0("There is no significant association between '", 
                     input$xaxisvar,
                     "' and '",
                     input$fillvar, "'."))
      }
      else {
        print(paste0("There is a significant association between '", 
                     input$xaxisvar,
                     "' and '",
                     input$fillvar,
                     "' (", 
                     results()$method,
                     ": p = ",
                     round(results()$p.value, 5),
                     ")."))
      }
    }
    
  })
  
  # for tab 2
  fig2_cat <- reactive({
    ggplot(dat, aes_string(x = varnames[input$xaxisvar_cat], 
                           y = varnames[input$characterisation],
                           text = "ref_number")) +
      geom_boxplot(fill = pal[5], outlier.shape = NA, outlier.size = 0) +
      geom_jitter(width = 0.2, colour = pal[1]) +
      scale_y_continuous(name = input$characterisation,
                         expand = expansion(c(0, 0.1))) +
      scale_x_discrete(expand = c(0, 0), 
                       name = input$xaxisvar_cat) +
      theme_classic() +
      theme(axis.text = element_text(size = 10, colour = "black"),
            axis.title = element_text(size = 10, colour = "black"),
            legend.text = element_text(size = 10, colour = "black"),
            legend.title = element_text(size = 10, colour = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank()) 
  })
  
  output$marker_counts_cat <- renderPlotly({
    if (input$flip2 == "No") {
      ggplotly( fig2_cat() ) 
    }
    
    else {
      ggplotly( fig2_cat() + coord_flip()) 
      
    }
    
  })
  
  # number of characterisations 
  results2 <- reactive({
    kruskal.test(dat[[varnames[input$characterisation]]] ~ dat[[varnames[input$xaxisvar_cat]]])
    
  })
  
  output$kwtest <- renderText({
    
    if (results2()$p.value > 0.05) {
      print(paste0("There is no significant difference in the median '", 
                   input$characterisation, 
                   "' between '",
                   input$xaxisvar_cat, "' levels.")) }
    
    else {
      print(paste0("There is a significant difference in the median '", 
                   input$characterisation,
                   "' between '",
                   input$xaxisvar_cat,
                   "s'. (",
                   results2()$method,
                   ": chi-squared = ",
                   round(results2()$statistic, 2),
                   "; p = ", 
                   round(results2()$p.value, 5),
                   ")."))
    }
  })
  
  
  # for tab 3
  fig3 <-
    test_by_attribute_summary %>% 
    ggplot() +
    geom_bar(aes(x = marker2, y = n, fill = test_status),
             stat = "identity", width = 0.6) +
    scale_fill_manual(values =  c("#BABABA", pal[c(2, 5)]),
                      name = "Test results",
                      guide = guide_legend(nrow = 1, 
                                           title.position = "top", 
                                           title.hjust = 0.5 )) +
    scale_y_continuous(name = "Number of Articles",
                       breaks = seq(0, 90, 10),
                       minor_breaks = seq(0, 90, 10),
                       limits = c(0, 90),
                       expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0.5), 
                     name = "Characterisation attribute") +
    geom_rect(xmin = 1.5, xmax = 11.5,
              ymin = -Inf, ymax = Inf,
              size = 1, linetype = 1, 
              colour = "black", alpha = 0) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, colour = "black"),
          axis.text.y  = element_text(size = 10, colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size = 10, colour = "black"),
          legend.position = "top",
          legend.key.size = unit(0.3, "in")) +
    coord_flip()
  output$teststatus_prevalence <- renderPlotly({
    ggplotly(fig3)
  })
  

  fig4 <- test_by_attribute %>% 
    ggplot() +
    geom_tile(aes(x = ref_number, y = marker2 , fill = test_status),
              colour = "white") +
    scale_fill_manual(values =  c("#BABABA", pal[c(2, 5)]),
                      name = "Test results",
                      guide = guide_legend(nrow = 1, 
                                           title.position = "top",
                                           title.hjust = 0.5 )) +
    scale_x_discrete(name = "Reference",
                     expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0, 0), 
                     name = "Characterisation attribute") +
    geom_rect(ymin = 1.5, ymax = 11.5,
              xmin = -Inf, xmax = Inf,
              size = 1, linetype = 1,
              colour = "black", alpha = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, colour = "black", 
                                     angle = 90, hjust = 0, vjust = 0.3),
          axis.text.y  = element_text(size = 10, colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 10, colour = "black"),
          legend.key.size = unit(0.2, "in"))
  
  output$teststatus_by_marker_detail <- renderPlot({
    fig4
  })
  
  # for tab 4
  cd_isct <- cd_summary %>% 
    filter(ISCT == "ISCT phenotype marker") %>% 
    ggplot(aes(x = marker, 
               y = mean_value,
               text = ref_number)) +
    geom_pointrange(aes(ymin = mean_value - se_value,
                        ymax = mean_value + se_value), 
                    position = position_jitter(width = 0.4),
                    shape = 20,
                    colour = pal[1]) +
    xlab("ISCT phenotype marker") +
    scale_y_continuous(name = "Reported % cells expressing marker", 
                       breaks = seq(0, 100, 10),
                       minor_breaks = seq(0, 110, 10),
                       limits = c(0, 110),
                       expand = c(0, 0)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8, colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_text(size = 8, colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  
  output$cd_isct <- renderPlotly({
    ggplotly(cd_isct, tooltip = c("x","text"))
  }) 
  
  cd_other <- cd_summary %>% 
    filter(ISCT == "Other marker") %>% 
    ggplot(aes(x = marker, 
               y = mean_value,
               text = ref_number)) +
    geom_pointrange(aes(ymin = mean_value - se_value,
                        ymax = mean_value + se_value), 
                    position = position_jitter(width = 0.4),
                    shape = 20,
                    colour = pal[1]) +
    xlab("Other marker") +
    scale_y_continuous(name = "Reported % cells expressing marker", 
                       breaks = seq(0, 100, 10),
                       minor_breaks = seq(0, 110, 10),
                       limits = c(0, 110),
                       expand = c(0, 0)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8, colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_text(size = 8, colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) 
  
  output$cd_other <- renderPlotly({
    ggplotly(cd_other, tooltip = c("x","text"))
  })  
  
  # for tab 6
  # choose columns to display
  output$complete_data <- DT::renderDataTable({
    DT::datatable(dat[, varnames[input$show_vars], drop = FALSE],
                  colnames = varnames[input$show_vars],
                  options = list(orderClasses = TRUE),
                  escape = FALSE,
                  filter = "top",
                  class = "compact" )
  })
  output$downloadData <- downloadHandler(
    filename = "wilson_et_al_2020.csv",
    content = function(file) {
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)
