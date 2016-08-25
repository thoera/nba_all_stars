library("dplyr")
library("ggplot2")
library("radarchart")
library("shiny")

# ----------

## Helpers.

# ggplot2 theme.
theme_light_grey <- function(base_size = 16, text_size = 18) {
  bg_color <- "#F4F4F4"
  bg_rect <- element_rect(fill = bg_color, color = bg_color)
  
  theme_bw(base_size = base_size) +
    theme(axis.title = element_text(size = text_size),
          plot.background = bg_rect,
          panel.background = bg_rect,
          panel.border = element_rect(fill = NA, color = "grey80"),
          legend.background = bg_rect,
          panel.grid.major = element_line(color = "grey80", size = 0.25),
          panel.grid.minor = element_line(color = "grey90", size = 0.25),
          legend.key.width = unit(1.5, "line"),
          legend.key = element_blank())
}

# ----------

## Load the dataset.
df <- read.csv("./data/main_stats.csv",
               header = TRUE, stringsAsFactors = FALSE, sep = ",", 
               strip.white = TRUE, colClasses = c("position_5" = "factor"), 
               fileEncoding = "UTF-8")

# ----------

ui <- fluidPage(
  
  fluidRow(
    
    column(3,
           
           wellPanel(
             
             selectInput("select_player",
                         label = h3("Select one or more players:"),
                         choices = df$player, 
                         multiple = TRUE,
                         width = "75%"),
             
             h3("Filters:"),
             
             selectInput("filter_position",
                         label = h4("Position:"),
                         choices = c("point guard", "shooting guard", 
                                     "small forward", "power forward", 
                                     "center"),
                         multiple = TRUE,
                         width = "75%"),
             
             helpText("If empty, no filtering on the position is done."),
             
             sliderInput("filter_points",
                         label = h4("Points:"),
                         min = 0,
                         max = 30,
                         value = c(0, 30)),
             
             sliderInput("filter_assists",
                         label = h4("Assists:"),
                         min = 0,
                         max = 10,
                         value = c(0, 12)),
             
             sliderInput("filter_rebounds",
                         label = h4("Rebounds:"),
                         min = 0,
                         max = 14,
                         value = c(0, 14)),
             
             sliderInput("filter_blocks",
                         label = h4("Blocks:"),
                         min = 0,
                         max = 4,
                         step = 0.5,
                         value = c(0, 4)),
             
             sliderInput("filter_turnovers",
                         label = h4("Turnovers:"),
                         min = 0,
                         max = 4,
                         step = 0.5,
                         value = c(0, 4))
           )),
    
    column(4,
           
           br(),
           
           DT::dataTableOutput("table_stats", width = "100%")),
    
    column(4,
           
           br(),
           
           chartJSRadarOutput("radar", width = "450", height = "300"))
  )
)

# ----------

server <- function(input, output, session) {
  
  observe({

    if (is.null(input$filter_position)) {
      pos <- levels(df$position_5)
    } else {
      pos <- input$filter_position
    }

    min_pts <- input$filter_points[1]
    max_pts <- input$filter_points[2]

    min_ast <- input$filter_assists[1]
    max_ast <- input$filter_assists[2]

    min_trb <- input$filter_rebounds[1]
    max_trb <- input$filter_rebounds[2]

    min_blk <- input$filter_blocks[1]
    max_blk <- input$filter_blocks[2]

    min_tov <- input$filter_turnovers[1]
    max_tov <- input$filter_turnovers[2]

    updateSelectInput(session, "select_player",
                      choices = df[(df$position_5 %in% pos) &
                                           (df$PTS >= min_pts &
                                              df$PTS <= max_pts) &
                                           (df$AST >= min_ast &
                                              df$AST <= max_ast) &
                                           (df$TRB >= min_trb &
                                              df$TRB <= max_trb) &
                                           (df$BLK >= min_blk &
                                              df$BLK <= max_blk) &
                                           (df$TOV >= min_tov &
                                              df$TOV <= max_tov),
                                         "player"])
  })

  output$table_stats <- DT::renderDataTable({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
      DT::datatable(df[df$player %in% input$select_player,
                             c("player", "PTS", "AST", "TRB", "BLK", "TOV")],
                    options = list(paging = FALSE, searching = FALSE),
                    rownames = FALSE)
    }
  })

  output$radar <- renderChartJSRadar({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
      df_radar <- filter(df, player %in% input$select_player) %>%
        select(-position_5) %>%
        reshape2::melt(id.vars = "player", 
                       variable.name = "Label", value.name = "Score") %>%
        tidyr::spread(key = player, value = Score)
      return(chartJSRadar(scores = df_radar, showToolTipLabel = TRUE))
    }
  })
}

shinyApp(ui = ui, server = server)

# Dotplot.

df %>%
  filter(player %in% c("Kevin Durant", "Ben Wallace")) %>%
  select(-position_5) %>%
  reshape2::melt(id.vars = "player") %>%
  ggplot(aes(x = variable, y = value, color = player)) +
    geom_point(size = 4, position = position_jitterdodge(jitter.width = 0, 
                                                         dodge.width = 0.2)) +
    scale_y_continuous(breaks = seq(0, 30, by = 4),
                       limits = c(0, 30)) +
    xlab("") + 
    ylab("") +
    theme_light_grey() +
    theme(legend.title = element_blank())
