library("dplyr")
library("ggplot2")
library("radarchart")
library("shiny")

# ----------

## Helpers.

# ggplot2 theme.
theme_simple <- function(base_size = 18, text_size = 20) {
  bg_color <- "#FFFFFF"
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

# Colors used by ggplot2.
ggplot_colors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
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
           
           br(),
           
           wellPanel(
             
             shinyjs::useShinyjs(),  # Set up shinyjs.
             
             id = "well-panel",  # Give an id to the panel.
             
             selectInput("select_player",
                         label = h3("Select one or more All-Stars:"),
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
                         value = c(0, 4)),
             
             actionButton("reset_filters", "Reset filters")
           )),
    
    column(8,
           
           br(),
           
           DT::dataTableOutput("table_stats"),
           
           br(),
           
           br(),
           
           conditionalPanel(
             condition = "input.select_player",
             
             tabsetPanel(
               
               tabPanel("Dot Plots", 
                        
                        tagList(
                          
                          br(),
                          
                          column(6, plotOutput("dotplot_1")),
                          
                          column(6, plotOutput("dotplot_2"))
                        )),
               
               tabPanel("Radar Charts", 
                        
                        tagList(
                          
                          br(),
                          
                          column(6, chartJSRadarOutput("radar_1")),
                          
                          column(6, chartJSRadarOutput("radar_2"))
                        ))
             )
           )
    )
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
  
  observeEvent(input$reset_filters, {
    shinyjs::reset("well-panel")
  })
  
  output$table_stats <- DT::renderDataTable({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
      DT::datatable(df[df$player %in% input$select_player, ],
                    options = list(paging = FALSE, searching = FALSE),
                    rownames = FALSE, colnames = c("POS" = "position_5",
                                                   "2P%" = "X2P_p",
                                                   "3P%" = "X3P_p",
                                                   "FG%" = "FG_p",
                                                   "FT%" = "FT_p"))
    }
  })
  
  output$dotplot_1 <- renderPlot({
    df %>%
      filter(player %in% input$select_player) %>%
      select(player, PTS, AST, TRB, BLK, TOV) %>%
      reshape2::melt(id.vars = "player") %>%
      ggplot(aes(x = variable, y = value, color = player)) +
      geom_point(size = 5, 
                 position = position_jitterdodge(jitter.width = 0,
                                                 dodge.width = 0.35)) +
      scale_y_continuous(breaks = seq(0, 30, by = 4),
                         limits = c(0, 30)) +
      xlab("") + 
      ylab("") +
      theme_simple() +
      theme(legend.title = element_blank(), legend.position = "bottom")
  })
  
  output$dotplot_2 <- renderPlot({
    df %>%
      filter(player %in% input$select_player) %>%
      select(player, X2P_p, X3P_p, FG_p, FT_p) %>%
      reshape2::melt(id.vars = "player") %>%
      ggplot(aes(x = variable, y = value, color = player)) +
      geom_point(size = 5, 
                 position = position_jitterdodge(jitter.width = 0,
                                                 dodge.width = 0.35)) +
      scale_y_continuous(breaks = seq(0.1, 1, by = 0.2),
                         limits = c(0, 1)) +
      scale_x_discrete(breaks = c("X2P_p", "X3P_p", "FG_p", "FT_p"),
                       labels = c("2P%", "3P%", "FG%", "FT%")) +
      xlab("") + 
      ylab("") +
      theme_simple() +
      theme(legend.title = element_blank(), legend.position = "bottom")
  })
  
  output$radar_1 <- renderChartJSRadar({
    df_radar <- filter(df, player %in% input$select_player) %>%
      select(player, PTS, AST, TRB, BLK, TOV) %>%
      reshape2::melt(id.vars = "player", 
                     variable.name = "Label", value.name = "Score") %>%
      tidyr::spread(key = player, value = Score)
    
    cols <- col2rgb(ggplot_colors(n = length(input$select_player)))
    
    return(chartJSRadar(scores = df_radar, showToolTipLabel = TRUE, 
                        colMatrix = cols))
  })
  
  output$radar_2 <- renderChartJSRadar({
    df_radar <- filter(df, player %in% input$select_player) %>%
      select(player, X2P_p, X3P_p, FG_p, FT_p) %>%
      reshape2::melt(id.vars = "player", 
                     variable.name = "Label", value.name = "Score") %>%
      tidyr::spread(key = player, value = Score)
    
    cols <- col2rgb(ggplot_colors(n = length(input$select_player)))
    
    return(chartJSRadar(scores = df_radar, showToolTipLabel = TRUE, 
                        colMatrix = cols, labs = c("2P%", "3P%", "FG%", "FT%"),
                        maxScale = 1, scaleStartValue = 0, 
                        scaleStepWidth = 0.25))
  })
}

shinyApp(ui = ui, server = server)
