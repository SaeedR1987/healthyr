
#' Run Mortality Monitoring Dashboard
#'
#' Runs a local shiny dashboard to explore data quality for Mortality outcome indicators
#'
#' @param df A dataframe with Mortality data that has been formatted by healthry::format_nut_health_indicators function.
#' @param grouping_var A character value naming the column to group the results by.
#' @param filter_var1 A character value naming the first column to filter the data by.
#' @param filter_var2 A character value naming an additional column to filter the data by.
#'
#' @return Opens a shiny dashboard.
#' @export
#'
#' @examples
run_mortality_monitoring_dashboard <- function(df,
                                     grouping_var = NULL,
                                     filter_var1 = NULL,
                                     filter_var2 = NULL) {

  # library(healthyr)
  # library(shiny)
  # library(shinydashboard)
  # library(shinythemes)
  # library(shinyWidgets)
  # library(plotly)
  # library(DT)

  # MUST HAVE STANDARDIZED ENUMERATOR, GROUPING VAR, DATE DATA COLLECTION VARIABLES IN THE DATASET

  # check if df has been standardized

  filtering_var1 <- filter_var1
  filtering_var2 <- filter_var2

  grouping_var <- grouping_var

  filter_var1 <- df %>% dplyr::select(filtering_var1) %>% t %>% c %>% unique()
  filter_var2 <- df %>% dplyr::select(filtering_var2) %>% t %>% c %>% unique()

  enum_list <- df %>% dplyr::select(enum) %>% t %>% c %>% unique()
  grouping_list <- df %>% dplyr::select(grouping_var) %>% t %>% c %>% unique()

  min_date <- min(df$date_dc_date)
  max_date <- max(df$date_dc_date)

  # analysis_vars <- check_which_analysis_vars(df, anthro_grouping_var = grouping_var)

  # check if grouping vars are included in the dataset and
  # are categorical

  # check if plaus_args are needed for demographic plausibility checks,
  # if not included, will use default values

  # All other dataframe and such arguments processing data can go up here
  # before being used in the shinyApp function below.


   shiny::shinyApp(

    ui = shiny::fluidPage(theme = shinythemes::shinytheme("sandstone"),

      # Application title
      shiny::titlePanel("Mortality Monitoring Dashboard"),

      shiny::fluidRow(
        shiny::column(2, "Controls",
               # fileInput("file", "Data", multiple = FALSE, accept = NULL, buttonLabel = "Select Dataset", placeholder = "Your Dataset"),
               # pickerInput("analysis_var", choices = analysis_vars, selected = analysis_vars[1], multiple = FALSE),
               shinyWidgets::pickerInput("filter_var1", label = paste0("Filter 1: ", filtering_var1), choices = filter_var1, selected = filter_var1[1], multiple = TRUE),
               shinyWidgets::pickerInput("filter_var2", label = paste0("Filter 2: ", filtering_var2), choices = filter_var2, selected = filter_var2[1], multiple = TRUE),
               shinyWidgets::pickerInput("grouping_var1", label = paste0("Grouping Variable: ", grouping_var) , choices = grouping_list, selected = grouping_list[1], multiple = TRUE),
               shiny::sliderInput("date_range1", label = "Date range to analyze:", min = min_date, max = max_date, value = c(min_date, max_date)),

               shiny::conditionalPanel(condition = "input.tabSwitch == '3'",
                                       shinyWidgets::pickerInput("time_plot_var",
                                            label = "Select Variable over Time",
                                            choices = c("filler1", "filler2"))
               ),
               shiny::actionButton("desButton1", "Apply",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        ),
        shiny::column(10, "Output",
               shiny::tabsetPanel(id = "tabSwitch",
                           tabPanel("Load Data", value = 1,
                                    shiny::fluidRow(column(11, DT::dataTableOutput(outputId = "main_data")))),

                           shiny::tabPanel("Short Report", value = 2,
                                           shiny::fluidRow(column(11, DT::dataTableOutput("quality_table1")))),

                           shiny::tabPanel("Full Report", value = 2,
                                           shiny::fluidRow(column(11, DT::dataTableOutput("quality_table2")))),

                           shiny::tabPanel("Flag Table", value = 2,
                                           shiny::fluidRow(column(11, DT::dataTableOutput("flag_table")))),

                           shiny::tabPanel("Age Pyramid", value = 4,
                                           shiny::fluidRow(shiny::column(11, shiny::plotOutput("test_plot5")))),

                           shiny::tabPanel("Age Pyramid, Deaths", value = 5,
                                           shiny::fluidRow(shiny::column(11, shiny::plotOutput("test_plot6")))),

                           shiny::tabPanel("Age Years", value = 5,
                                           shiny::fluidRow(shiny::column(11, shiny::plotOutput("test_plot7")))),

                           shiny::tabPanel("Age Years, by Group", value = 5,
                                           shiny::fluidRow(shiny::column(11, shiny::plotOutput("test_plot8")))),

                           shiny::tabPanel("Causes, Location, Barriers", value = 5,
                                           shiny::fluidRow(shiny::column(11, shiny::plotOutput("test_plot9")))),

                           shiny::tabPanel("Dates of Death", value = 5,
                                           shiny::fluidRow(shiny::column(11, shiny::plotOutput("test_plot10")))),

                           ),
        )
      ),

    ), #closing for ui

    server = function(input, output, grp = grouping_var) {

      # REACTIVES

      reactive_db <- shiny::eventReactive(input$desButton1 , {

        df %>%
          # filter(!!rlang::sym(filtering_var1) %in% input$filter_var1) %>%
          # filter(!!rlang::sym(filtering_var2) %in% input$filter_var2) %>%
          dplyr::filter(!!rlang::sym(grouping_var) %in% input$grouping_var1) %>%
          dplyr::filter(date_dc_date >= input$date_range1[1] & date_dc_date <= input$date_range1[2])

      })

      # OUTPUTS

      output$quality_table1 <- DT::renderDT({

          DT::datatable(healthyr::create_mortality_quality_report(df = reactive_db(), grouping = grouping_var, short_report = TRUE),
                        filter = 'top',  extension = 'FixedColumns', options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, rightColumns = 0)))

      })

      output$quality_table2 <- DT::renderDT({

        DT::datatable(healthyr::create_mortality_quality_report(df = reactive_db(), grouping = grouping_var, short_report = FALSE),
                      filter = 'top',  extension = 'FixedColumns', options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, rightColumns = 0)))

      })

      output$main_data <- DT::renderDT({

        DT::datatable(reactive_db(), filter = 'top', extension = 'FixedColumns', options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, rightColumns = 0)))

      })

      output$flag_table <- DT::renderDT({

        DT::datatable(healthyr::flag_summary_table(df = reactive_db(), grouping = grouping_var), filter = 'top', extension = 'FixedColumns', options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2, rightColumns = 0)))

      })


      output$test_plot5 <- renderPlot({

        healthyr::plot_agepyramid(df = reactive_db())

      })

      output$test_plot6 <- renderPlot({

        healthyr::plot_agepyramid(df = reactive_db(), filtering = "death")

  })

      output$test_plot7 <- renderPlot({

        healthyr::plot_age_years_distribution(df = reactive_db(), min_age = 0, max_age = 10)


        })

      output$test_plot8 <- renderPlot({

        healthyr::plot_age_years_distribution(df = reactive_db(), min_age = 0, max_age = 10, by_group = grouping_var)


      })

      output$test_plot9 <- renderPlot({

        # Death Causes, Locations, Healthseeking Behaviours, etc.
        g <- ggplot2::ggplot(data = reactive_db() %>% dplyr::filter(death_cause != ""), aes(x = factor(death_cause), fill = factor(death_cause))) + ggplot2::geom_bar(stat = "count") + theme_minimal() + theme(legend.position = "none") + xlab("Enumerator ID")

        g1 <- ggplot2::ggplot(data = reactive_db() %>% dplyr::filter(death_cause != ""), aes(x = factor(enum), fill = factor(death_cause))) + ggplot2::geom_bar(stat = "count") + theme_minimal()  + theme(legend.position = "bottom", title = element_blank()) + xlab("Enumerator ID")

        gridExtra::grid.arrange(g, g1, nrow = 2)


      })

      output$test_plot10 <- renderPlot({

        # Death Locations

        g2 <- ggplot2::ggplot(data = reactive_db() %>% dplyr::filter(death_cause != ""), aes(x = factor(death_cause), fill = factor(death_cause))) + ggplot2::geom_bar(stat = "count") + theme_minimal() + theme(legend.position = "none") + xlab("Enumerator ID")

        g3 <- ggplot2::ggplot(data = reactive_db() %>% dplyr::filter(death_cause != ""), aes(x = factor(enum), fill = factor(death_cause))) + ggplot2::geom_bar(stat = "count") + theme_minimal()  + theme(legend.position = "bottom", title = element_blank()) + xlab("Enumerator ID")

        gridExtra::grid.arrange(g2, g3, nrow = 2)


      })

      output$test_plot11 <- renderPlot({

        # Death Health Care Seeking
        g4 <- ggplot2::ggplot(data = df4 %>% dplyr::filter(!is.na(lieu_recherche_soins)), aes(x = factor(enum), fill = factor(lieu_recherche_soins))) + ggplot2::geom_bar(stat = "count")

        g5 <- ggplot2::ggplot(data = reactive_db() %>% dplyr::filter(!is.na(lieu_recherche_soins)), aes(x = factor(enum), fill = factor(lieu_recherche_soins))) + ggplot2::geom_bar(stat = "count") + theme_minimal()  + theme(legend.position = "bottom", title = element_blank()) + xlab("Enumerator ID")

        gridExtra::grid.arrange(g4, g5, nrow = 2)



      })

      output$test_plot12 <- renderPlot({

        # Death Barriers
        g6 <- ggplot2::ggplot(data = df4 %>% dplyr::filter(!is.na(renonce_soins_raison)), aes(x = factor(enum), fill = factor(renonce_soins_raison))) + ggplot2::geom_bar(stat = "count")

        g7 <- ggplot2::ggplot(data = reactive_db() %>% dplyr::filter(!is.na(renonce_soins_raison)), aes(x = factor(enum), fill = factor(renonce_soins_raison))) + ggplot2::geom_bar(stat = "count") + theme_minimal()  + theme(legend.position = "bottom", title = element_blank()) + xlab("Enumerator ID")

        gridExtra::grid.arrange(g6, g7, nrow = 2)

      })

      # OBSERVES

    }

   )

}
