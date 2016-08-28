library("dplyr")
library("ggplot2")
library("gridExtra")
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
df <- read.csv("main_stats.csv",
               header = TRUE, stringsAsFactors = FALSE, sep = ",", 
               strip.white = TRUE, colClasses = c("position_5" = "factor"), 
               fileEncoding = "UTF-8")

# ----------

ui <- fluidPage(
  
  fluidRow(
    
    br(),
    
    column(3,
           
           wellPanel(
             
             shinyjs::useShinyjs(),  # Set up shinyjs.
             
             id = "well-panel",  # Give an id to the panel.
             
             selectizeInput("select_player",
                            label = h3("Select up to five players:"),
                            choices = df$player,
                            multiple = TRUE,
                            width = "80%",
                            options = list(maxItems = 5)),
             
             actionButton("random", "Random", 
                          icon = icon("random")),
             
             h3("Filters:"),
             
             selectizeInput("filter_position",
                            label = h4("Position:"),
                            choices = c("point guard", "shooting guard",
                                        "small forward", "power forward",
                                        "center"),
                            multiple = TRUE,
                            width = "80%",
                            options = list(placeholder = "If empty, no filtering on the position is done.")),
             
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
             
             actionButton("reset_filters", "Reset filters",
                          icon = icon("undo"))
             )),
    
    column(8,
           
           br(),
           
           conditionalPanel(
             condition = "input.select_player",
             tagList(
               
               DT::dataTableOutput("table_stats"),
               
               br(),
               
               h5("Data from", 
                  a("wikipedia", href = "https://www.wikipedia.org/"), "and",
                  a("basketball-reference.", 
                    href = "http://www.basketball-reference.com/")),
               
               br(),
               
               br(),
               
               tabsetPanel(
                 
                 tabPanel("Dot Plots", 
                          
                          tagList(
                            
                            br(),
                            
                            plotOutput("dotplot")
                          )),
                 
                 tabPanel("Radar Charts", 
                          
                          tagList(
                            
                            br(),
                            
                            column(6, chartJSRadarOutput("radar_1")),
                            
                            column(6, chartJSRadarOutput("radar_2"))
                            )
                          )
                 )
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
    
    updateSelectizeInput(session, "select_player",
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
  
  observeEvent(input$random, {
    updateSelectizeInput(session, "select_player",
                         selected = sample(df$player, sample(1:5, 1)))
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
  
  output$dotplot <- renderPlot({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
      data_to_plot <- df %>%
        filter(player %in% input$select_player) %>%
        select(-position_5) %>%
        reshape2::melt(id.vars = "player")
      
      g1 <- data_to_plot %>%
        filter(variable %in% c("PTS", "AST", "TRB", "BLK", "TOV")) %>%
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
        
      
      g2 <- data_to_plot %>%
        filter(variable %in% c("X2P_p", "X3P_p", "FG_p", "FT_p")) %>%
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
        theme(legend.position = "none")

      g_legend <- function(a.gplot) {
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)
        }
      
      grid.arrange(arrangeGrob(g1 + theme(legend.position = "none"), 
                               g2, nrow = 1), 
                   g_legend(g1), nrow = 2, heights = c(10, 1))
    }
  })
  
  df_radar <- reactive({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
      df %>%
        filter(player %in% input$select_player) %>%
        select(-position_5) %>%
        reshape2::melt(id.vars = "player", 
                       variable.name = "Label", value.name = "Score") %>%
        tidyr::spread(key = player, value = Score)
    }
  })
  
  cols <- reactive({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
      col2rgb(ggplot_colors(n = length(input$select_player)))
    }
  })

  output$radar_1 <- renderChartJSRadar({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
      chartJSRadar(scores = df_radar() %>%
                     filter(Label %in% c("PTS", "AST", "TRB", "BLK", "TOV")) %>%
                     select(order(names(.)), -Label), 
                   labs = c("PTS", "AST", "TRB", "BLK", "TOV"), 
                   showToolTipLabel = TRUE, colMatrix = cols())
    }
  })
  
  output$radar_2 <- renderChartJSRadar({
    if (is.null(input$select_player)) {
      return(NULL)
    } else {
    chartJSRadar(scores = df_radar() %>%
                   filter(Label %in% c("X2P_p", "X3P_p", "FG_p", "FT_p")) %>%
                   select(order(names(.)), -Label), 
                 labs = c("2P%", "3P%", "FG%", "FT%"),
                 showToolTipLabel = TRUE, colMatrix = cols(), maxScale = 1,
                 scaleStartValue = 0, scaleStepWidth = 0.25)
      }
    })
}

shinyApp(ui = ui, server = server)
