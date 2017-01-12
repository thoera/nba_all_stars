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

# A way to add a legend to a grid of plots.
g_legend <- function(plot_) {
  tmp <- ggplot_gtable(ggplot_build(plot_))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Colors used by ggplot2.
ggplot_colors <- function(n) {
  hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
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
             
             actionButton("options_filters", "Filters",
                          icon = icon("filter")),
             
             div(id = "options_filters_",
                 
                 hr(),
                 
                 selectizeInput("filter_position",
                                label = h4("Position:"),
                                choices = c("point guard", "shooting guard",
                                            "small forward", "power forward",
                                            "center"),
                                multiple = TRUE,
                                width = "80%",
                                options = list(placeholder =
                                                 paste0("If empty, ",
                                                        "no filtering on the ",
                                                        "position is done."))),
                 
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
             )
           )),
    
    column(8,
           
           br(),

           conditionalPanel(
             condition = "input.select_player",
             tagList(
               
               DT::dataTableOutput("table_stats"),
               
               br(),
               
               h5("Data from", 
                  a("wikipedia.org", href = "https://www.wikipedia.org/"),
                  "and", a("basketball-reference.com",
                           href = "http://www.basketball-reference.com/"), "."),
               
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
  
  # When the "filter" button is clicked, more options are available.
  observe({
    shinyjs::hide("options_filters_")
  })

  observeEvent(input$options_filters, {
    shinyjs::toggle("options_filters_", anim = TRUE)
  })
  
  pos_ <- reactive({
    if (is.null(input$filter_position)) {
      levels(df$position_5)
    } else {
      input$filter_position
    }
  })
  
  pts_ <- reactive({
    c(input$filter_points[1], input$filter_points[2])
  })
  
  ast_ <- reactive({
    c(input$filter_assists[1], input$filter_assists[2])
  })
  
  trb_ <- reactive({
    c(input$filter_rebounds[1], input$filter_rebounds[2])
  })
  
  blk_ <- reactive({
    c(input$filter_blocks[1], input$filter_blocks[2])
  })
  
  tov_ <- reactive({
    c(input$filter_turnovers[1], input$filter_turnovers[2])
  })
  
  df_filter <- reactive({
    df[(df$position_5 %in% pos_()) &
         (df$PTS >= pts_()[[1]] & df$PTS <= pts_()[[2]]) &
         (df$AST >= ast_()[[1]] & df$AST <= ast_()[[2]]) &
         (df$TRB >= trb_()[[1]] & df$TRB <= trb_()[[2]]) &
         (df$BLK >= blk_()[[1]] & df$BLK <= blk_()[[2]]) &
         (df$TOV >= tov_()[[1]] & df$TOV <= tov_()[[2]]), ]
  })

  observe(
    updateSelectizeInput(session, "select_player",
                         choices = df_filter()[, "player"])
  )

  observeEvent(input$random, {
    updateSelectizeInput(session, "select_player",
                         selected = sample(df_filter()[, "player"],
                                           sample(1:min(5, nrow(df_filter())),
                                                  1)))
  })
  
  observeEvent(input$reset_filters, {
    shinyjs::reset("well-panel")
  })
  
  output$table_stats <- DT::renderDataTable({
    req(input$select_player)
    
    DT::datatable(df[df$player %in% input$select_player, ],
                  options = list(paging = FALSE, searching = FALSE),
                  rownames = FALSE, colnames = c("POS" = "position_5",
                                                 "2P%" = "X2P_p",
                                                 "3P%" = "X3P_p",
                                                 "FG%" = "FG_p",
                                                 "FT%" = "FT_p"))
  })
  
  output$dotplot <- renderPlot({
    req(input$select_player)
    
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
    
    grid.arrange(arrangeGrob(g1 + theme(legend.position = "none"),
                             g2, nrow = 1),
                 g_legend(g1), nrow = 2, heights = c(10, 1))
  })
  
  df_radar <- reactive({
    req(input$select_player)
    
    df %>%
      filter(player %in% input$select_player) %>%
      select(-position_5) %>%
      reshape2::melt(id.vars = "player", 
                     variable.name = "Label", value.name = "Score") %>%
      tidyr::spread(key = player, value = Score)
  })
  
  cols <- reactive({
    req(input$select_player)
    
    col2rgb(ggplot_colors(n = length(input$select_player)))
  })
  
  output$radar_1 <- renderChartJSRadar({
    req(input$select_player)
    
    chartJSRadar(scores = df_radar() %>%
                   filter(Label %in% c("PTS", "AST", "TRB", "BLK", "TOV")) %>%
                   select(order(names(.)), -Label), 
                 labs = c("PTS", "AST", "TRB", "BLK", "TOV"), 
                 showToolTipLabel = TRUE, colMatrix = cols())
  })
  
  output$radar_2 <- renderChartJSRadar({
    req(input$select_player)
    
    chartJSRadar(scores = df_radar() %>%
                   filter(Label %in% c("X2P_p", "X3P_p", "FG_p", "FT_p")) %>%
                   select(order(names(.)), -Label), 
                 labs = c("2P%", "3P%", "FG%", "FT%"),
                 showToolTipLabel = TRUE, colMatrix = cols(), maxScale = 1,
                 scaleStartValue = 0, scaleStepWidth = 0.25)
  })
}

shinyApp(ui = ui, server = server)
