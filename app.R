library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)
library(bslib)
library(shinyjs)
load("rda/match_logs.rda")
load("rda/shooting_logs.rda")
load("rda/keepers_stats.rda")


# Function pass completion plots
pass_completion_plot_donut <- function(data, kpi_type) {
	# Transform data
	data <- data %>%
		reframe(
			KPI = c("Short", "Medium", "Long"),
			success = c(round(mean(Cmp_percent_Short, na.rm = T), 2), round(mean(Cmp_percent_Medium, na.rm = T), 2), round(mean(Cmp_percent_Long, na.rm = T), 2)),
			missed = c(100 - round(mean(Cmp_percent_Short, na.rm = T), 2), 100 - round(mean(Cmp_percent_Medium, na.rm = T), 2), 100 - round(mean(Cmp_percent_Long, na.rm = T), 2)),
			target = c(100, 100, 100)
		) %>%
		as.data.frame()
	data <- subset(data, KPI == kpi_type)
	data <- data.frame(category = c("Success", "Missed"), count = c(data$success, data$missed))
	data$fraction = data$count / sum(data$count)		# Compute percentages
	data$ymax = cumsum(data$fraction)		# Compute cummulative percentages
	data$ymin = c(0, head(data$ymax, n=-1))		# Compute the bottom of each rectangle
	
	# Make the plot
	p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
		geom_rect(color="transparent", aes(fill = category), linewidth=0.5, position = "identity") +
		scale_fill_manual(values = c("Success" = "darkgreen", "Missed" = "white")) +
		coord_polar(theta="y") +
		xlim(c(-1, 4)) +
		theme_void() +
		theme(legend.position = "none") +
		annotate(geom = 'text', x = -0.8, y = 0, label = paste0(data$count[1], "%", sep = ""), size = 17)
	
	return(p)
}

# Sidebar filters
ply_name <- selectInput("ply_name",
												"Player",
												c("Enzo Fernández", "Julián Álvarez", "Kevin De Bruyne", "Moisés Caicedo"),
												selected = "Enzo Fernández",
												multiple = T)
ply_team <- selectInput("ply_team",
												"Team",
												c("Chelsea", "Manchester City"),
												selected = "Chelsea",
												multiple = T)
compared_league <- selectInput("compared_league",
												"Compared Leagues",
												c("Premier League", "Serie A"),
												selected = "Chelsea",
												multiple = T)

# Pass completion cards
pass_completion_cards <- {list(
	card(
		full_screen = TRUE,
		card_header("Short Passes"),
		plotOutput("short_completion_passes")
	),
	card(
		full_screen = TRUE,
		card_header("Medium Passes"),
		plotOutput("medium_completion_passes")
	),
	card(
		full_screen = TRUE,
		card_header("Long Passes"),
		plotOutput("long_completion_passes")
	)
)}



ui <- {page_sidebar(
	title = "Fut Dashboard",
	sidebar = fluidRow(ply_name, ply_team, compared_league),
	theme = bs_theme(
		bootswatch = "darkly",
		base_font = font_google("Fira Sans"),
		code_font = font_google("Fira Code"),
		heading_font = font_google("Fredoka One"),
		navbar_bg = "#81968F"
	),
	layout_columns(pass_completion_cards[[1]], pass_completion_cards[[2]], pass_completion_cards[[3]])  # Pass completion (%) plots
)}



# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))


server <- function(input, output){
	# DATA FITLERING ----
	# Player match logs
	filtered_match_logs <- reactive({
		match_logs %>%
			filter(Player %in% input$ply_name,
						 Team %in% input$ply_team)
	})
	# Comp. Pool match logs
	
	
	
	
	# OUTPUTS ----
	# Pass completion (%) plots
	output$short_completion_passes <- renderPlot({
		pass_completion_plot_donut(filtered_match_logs(), "Short")
	})
	
	output$medium_completion_passes <- renderPlot({
		pass_completion_plot_donut(filtered_match_logs(), "Medium")
	})
	
	output$long_completion_passes <- renderPlot({
		pass_completion_plot_donut(filtered_match_logs(), "Long")
	})
	
	
	
}



shinyApp(ui, server)
