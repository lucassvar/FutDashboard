library(dplyr)
library(worldfootballR)
library(stringr)

# get match urls from a specified year or period
get_all_match_urls <- function(year = NA){
	start_time <- Sys.time()
	
	# Countries to extract data
	top5_male_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")
	other_male_countries <- c("NED","POR","BEL", "ARG","BRA","MEX","USA")
	
	top5_female_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")
	other_women_countries <- c("AUS", "USA")
	
	second_tier_countries <- c("ENG","ESP","GER","ITA","FRA")
	
	
	# Male Football
	match_urls_MASC <- c(
		fb_match_urls(top5_male_leagues, "M", year, "1st"),
		fb_match_urls(other_male_countries, "M", year, "1st"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/14/history/Copa-Libertadores-Seasons"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/8/history/Champions-League-Seasons"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/19/history/Europa-League-Seasons"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/882/history/Europa-Conference-League-Seasons")
	)
	
	# Female Football
	match_urls_FEM <- c(
		fb_match_urls(top5_female_leagues, "F", year, "1st"),
		fb_match_urls(other_women_countries, "F", year, "1st"),
		fb_match_urls("", "F", year, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons")
	)
	
	# Second Tier Football
	match_urls_SECOND <- fb_match_urls(second_tier_countries, "M", year, "2nd")
	
	# Save both groups of links in same variable
	all_match_urls <- list("male" = match_urls_MASC, "female" = match_urls_FEM, "second" = match_urls_SECOND)
	
	save(all_match_urls, file = "rda/all_match_urls.rda")
	Sys.time() - start_time
}


# update the links rda file with new links from a certain year
update_links <- function(new_year = NA){
	start_time <- Sys.time()
	
	# Countries to extract data
	top5_male_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")
	other_male_countries <- c("NED","POR","BEL", "ARG","BRA","MEX","USA")
	
	top5_female_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")
	other_women_countries <- c("AUS", "USA")
	
	second_tier_countries <- c("ENG","ESP","GER","ITA","FRA")
	
	
	# Male Football
	match_urls_MASC <- c(
		fb_match_urls(top5_male_leagues, "M", year, "1st"),
		fb_match_urls(other_male_countries, "M", year, "1st"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/14/history/Copa-Libertadores-Seasons"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/8/history/Champions-League-Seasons"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/19/history/Europa-League-Seasons"),
		fb_match_urls("", "M", year, non_dom_league_url = "https://fbref.com/en/comps/882/history/Europa-Conference-League-Seasons")
	)
	
	# Female Football
	match_urls_FEM <- c(
		fb_match_urls(top5_female_leagues, "F", year, "1st"),
		fb_match_urls(other_women_countries, "F", year, "1st"),
		fb_match_urls("", "F", year, non_dom_league_url = "https://fbref.com/en/comps/181/history/Champions-League-Seasons")
	)
	
	# Second Tier Football
	match_urls_SECOND <- fb_match_urls(second_tier_countries, "M", year, "2nd")
	
	# Save both groups of links in same variable
	all_match_urls <- list("male" = match_urls_MASC, "female" = match_urls_FEM, "second" = match_urls_SECOND)
	new_links <- all_match_urls
	
	load("rda/all_match_urls.rda")
	
	all_match_urls$male <- unique(c(new_links$male, all_match_urls$male))
	all_match_urls$female <- unique(c(new_links$female, all_match_urls$female))
	all_match_urls$second <- unique(c(new_links$second, all_match_urls$second))
	
	save(all_match_urls, file = "rda/all_match_urls.rda")
	Sys.time() - start_time
}


# function to extract shooting logs from not used links (ratios :: masc, second, fem)
extract_match_data <- function(selected_ratios = "50/30/20") {
	start_time <- Sys.time()
	
	# load the needed links
	load("rda/all_match_urls.rda")
	if (file.exists("rda/used_links.rda")) {
		load("rda/used_links.rda")
		new_links <- list("male" = all_match_urls$male[!(all_match_urls$male %in% used_links$male)],
											"female" = all_match_urls$female[!(all_match_urls$female %in% used_links$female)],
											"second" = all_match_urls$second[!(all_match_urls$second %in% used_links$second)])
	} else {
		new_links <- all_match_urls
	} # if used_links exists, get the links that haven't been used
	
	# stat types used for match logs extraction
	stat_types_used <- c("passing", "passing_types", "defense", "possession", "misc")
	
	# select the amount of links analyzed
	links_ratios <- as.list(setNames(as.numeric(strsplit(selected_ratios, "/")[[1]]), c("male", "second", "female")))
	if (selected_ratios != "all") {
		new_links <- list(
			male = new_links$male[1:links_ratios$male],
			second = new_links$second[1:links_ratios$second],
			female = new_links$female[1:links_ratios$female]
		)
	}
	
	# remove NA links
	new_links <- lapply(new_links, na.omit)
	
	# match logs ----
	print("Male - Match Logs")
	if (length(new_links$male) > 0) {
		male_matchLogs <- c()
		for (stat in stat_types_used) {
			new_male_matchLogs <- fb_advanced_match_stats(match_url = new_links$male, stat_type = stat, team_or_player = "player") %>% 
				mutate(Sex = "M", Tier = "1st")
			
			if (is.null(male_matchLogs)) {
				male_matchLogs <- new_male_matchLogs
			} else {
				common_cols <- intersect(names(male_matchLogs), names(new_male_matchLogs))
				new_cols <- setdiff(names(new_male_matchLogs), common_cols)
				
				# Ensure unique column names
				new_cols <- make.unique(new_cols, sep = "_")
				
				male_matchLogs <- bind_cols(male_matchLogs, new_male_matchLogs %>% select(all_of(new_cols)))
			}
		}
	} else {
		male_matchLogs <- NULL
	}
	
	print("Female - Match Logs")
	if (length(new_links$female) > 0) {
		female_matchLogs <- c()
		for (stat in stat_types_used) {
			new_female_matchLogs <- fb_advanced_match_stats(match_url = new_links$male, stat_type = stat, team_or_player = "player") %>% 
				mutate(Sex = "M", Tier = "1st")
			
			if (is.null(female_matchLogs)) {
				female_matchLogs <- new_female_matchLogs
			} else {
				common_cols <- intersect(names(female_matchLogs), names(new_female_matchLogs))
				new_cols <- setdiff(names(new_female_matchLogs), common_cols)
				
				# Ensure unique column names
				new_cols <- make.unique(new_cols, sep = "_")
				
				female_matchLogs <- bind_cols(female_matchLogs, new_female_matchLogs %>% select(all_of(new_cols)))
			}
		}
		
		
	} else {
		female_matchLogs <- NULL
	}
	
	print("Second - Match Logs")
	if (length(new_links$second) > 0) {
		second_matchLogs <- c()
		for (stat in stat_types_used) {
			new_second_matchLogs <- fb_advanced_match_stats(match_url = new_links$male, stat_type = stat, team_or_player = "player") %>% 
				mutate(Sex = "M", Tier = "1st")
			
			if (is.null(second_matchLogs)) {
				second_matchLogs <- new_second_matchLogs
			} else {
				common_cols <- intersect(names(second_matchLogs), names(new_second_matchLogs))
				new_cols <- setdiff(names(new_second_matchLogs), common_cols)
				
				# Ensure unique column names
				new_cols <- make.unique(new_cols, sep = "_")
				
				second_matchLogs <- bind_cols(second_matchLogs, new_second_matchLogs %>% select(all_of(new_cols)))
			}
		}
		
		
		
	} else {
		second_matchLogs <- NULL
	}
	new_match_logs <- bind_rows(bind_rows(male_matchLogs, female_matchLogs), second_matchLogs) # bind the data frames
	new_match_logs$Matchweek <- as.numeric(str_extract(new_match_logs$Matchweek, "\\b(\\d+)\\b"))
	
	
	# keeper stats ----
	print("Male - Keeper Stats")
	if (length(new_links$male) > 0) {
		male_keepers <- c()
		new_male_keepers <- fb_advanced_match_stats(match_url = new_links$male, stat_type = "keeper", team_or_player = "player") %>% 
			mutate(Sex = "M", Tier = "1st")
		male_keepers <- bind_rows(male_keepers, new_male_keepers)
	} else {
		male_keepers <- NULL
	}
	print("Female - Keeper Stats")
	if (length(new_links$female) > 0) {
		female_keepers <- c()
		new_female_keepers <- fb_advanced_match_stats(match_url = new_links$female, stat_type = "keeper", team_or_player = "player") %>% 
			mutate(Sex = "F", Tier = "1st")
		female_keepers <- bind_rows(female_keepers, new_female_keepers)
	} else {
		female_keepers <- NULL
	}
	print("Second - Keeper Stats")
	if (length(new_links$second) > 0) {
		second_keepers <- c()
		new_second_keepers <- fb_advanced_match_stats(match_url = new_links$second, stat_type = "keeper", team_or_player = "player") %>% 
			mutate(Sex = "M", Tier = "2nd")
		second_keepers <- bind_rows(second_keepers, new_second_keepers)
	} else {
		second_keepers <- NULL
	}
	new_keeper_stats <- bind_rows(bind_rows(male_keepers, female_keepers), second_keepers) # bind the data frames
	
	
	# shooting logs ----
	print("Male - Shooting Logs")
	if (length(new_links$male) > 0) {
		male_shotsLogs <- fb_match_shooting(match_url = new_links$male) %>% mutate(Tier = "1st", Sex = "M")
	} else {
		male_shotsLogs <- NULL
	}
	print("Female - Shooting Logs")
	if (length(new_links$female) > 0) {
		female_shotsLogs <- fb_match_shooting(match_url = new_links$female) %>% mutate(Tier = "1st", Sex = "F")
	} else {
		female_shotsLogs <- NULL
	}
	print("Second - Shooting Logs")
	if (length(new_links$second) > 0) {
		second_shotsLogs <- fb_match_shooting(match_url = new_links$second) %>% mutate(Tier = "2nd", Sex = "M")
	} else {
		second_shotsLogs <- NULL
	}
	new_shots <- bind_rows(bind_rows(male_shotsLogs, female_shotsLogs), second_shotsLogs) # bind the data frames
	
	
	# merge the new links into the used_links variable and save them ----
	if (file.exists("rda/used_links.rda")) {
		used_links <- list("male" = unique(c(used_links$male, new_links$male)),
										"female" = unique(c(used_links$female, new_links$female)),
										"second" = unique(c(used_links$second, new_links$second)))
	} else {
		used_links <- new_links
	}
	save(used_links, file = "rda/used_links.rda")
	
	
	
	# expand match logs data frame only if the new one is not empty
	if (length(new_match_logs) > 0) {
		# change team names to match between match_logs and shooting_logs
		new_match_logs <- new_match_logs %>%
			mutate(Team = case_when(
				grepl("United", Team, ignore.case = TRUE) ~ gsub("United", "Utd", Team, ignore.case = TRUE),
				Team == "Wolves" & League == "Premier League" ~ "Wolverhampton",
				Team == "West Ham United" ~ "West Ham",
				Team == "Nott'ham Forest" ~ "Nottingham Forest",
				Team == "Brighton" ~ "Brighton & Hove Albion",
				Team == "Tottenham" ~ "Tottenham Hotspur",
				TRUE ~ Team
			))
		
		# add Id column (unique for each combination of date - team - tier - sex)
		new_match_logs <- new_match_logs %>%
			mutate(Id = gsub(" ", "", paste(as.numeric(format(as.Date(Match_Date), "%Y%m%d")),
																			Team,
																			Tier,
																			Sex)))
		new_match_logs <- new_match_logs %>% distinct()		# delete duplicates
		
		# load the match logs data frame (if it exists) and bind the new one
		if (file.exists("rda/match_logs.rda")) {
			load("rda/match_logs.rda")
			match_logs <- bind_rows(match_logs, new_match_logs)
		} else {
			match_logs <- new_match_logs
		}
		
		# save match_logs
		save(match_logs, file = "rda/match_logs.rda")
	}
	
	# expand keepers stats data frame only if the new one is not empty
	if (length(new_keeper_stats) > 0) {
		# change team names to match between match_logs and shooting_logs
		new_keeper_stats <- new_keeper_stats %>%
			mutate(Team = case_when(
				grepl("United", Team, ignore.case = TRUE) ~ gsub("United", "Utd", Team, ignore.case = TRUE),
				Team == "Wolves" & League == "Premier League" ~ "Wolverhampton",
				Team == "West Ham United" ~ "West Ham",
				Team == "Nott'ham Forest" ~ "Nottingham Forest",
				Team == "Brighton" ~ "Brighton & Hove Albion",
				Team == "Tottenham" ~ "Tottenham Hotspur",
				TRUE ~ Team
			))
		
		# add Id column (unique for each combination of date - team - tier - sex)
		new_keeper_stats <- new_keeper_stats %>%
			mutate(Id = gsub(" ", "", paste(as.numeric(format(as.Date(Match_Date), "%Y%m%d")),
																			Team,
																			Tier,
																			Sex)))
		new_keeper_stats <- new_keeper_stats %>% distinct()		# delete duplicates
		
		# load the keepers stats data frame (if it exists) and bind the new one
		if (file.exists("rda/keepers_stats.rda")) {
			load("rda/keepers_stats.rda")
			keepers_stats <- bind_rows(keepers_stats, new_keeper_stats)
		} else {
			keepers_stats <- new_keeper_stats
		}
		
		# save keepers_stats
		save(keepers_stats, file = "rda/keepers_stats.rda")
	}
	
	# expand shooting logs data frame only if the new one is not empty
	if (length(new_shots) > 0) {
		# change team names to match between match_logs and shooting_logs
		new_shots <- new_shots %>%
			mutate(Squad = case_when(
				grepl("United", Squad, ignore.case = TRUE) ~ gsub("United", "Utd", Squad, ignore.case = TRUE),
				Squad == "Wolves" ~ "Wolverhampton",
				Squad == "West Ham United" ~ "West Ham",
				Squad == "Nott'ham Forest" ~ "Nottingham Forest",
				Squad == "Brighton" ~ "Brighton & Hove Albion",
				Squad == "Tottenham" ~ "Tottenham Hotspur",
				TRUE ~ Squad
			))
		
		# transform minute column to numeric
		new_shots <- new_shots %>%
			mutate(Minute = case_when(grepl("\\+", Minute) ~ sum(as.numeric(strsplit(Minute, "\\+")[[1]])),
																TRUE ~ as.numeric(Minute)))
		
		# add Id column (unique for each combination of date - team - tier - sex)
		new_shots <- new_shots %>%
			mutate(Id = gsub(" ", "", paste(as.numeric(format(as.Date(Date), "%Y%m%d")),
																			Squad,
																			Tier,
																			Sex)))
		
		# convert xG and PSxG to numeric, put 0 if empty
		new_shots$PSxG <- as.numeric(ifelse(new_shots$PSxG == "", 0, new_shots$PSxG))
		new_shots$xG <- as.numeric(ifelse(new_shots$xG == "", 0, new_shots$xG))
		
		if (length(new_match_logs) > 0) {		# add League and Min columns from the match_logs df
			df <- new_match_logs %>% select(Id, League, Player, Min)
			new_shots <- merge(new_shots, df, by = c("Player", "Id"), all.x = TRUE)
			new_shots <- new_shots %>% distinct()
		}
		
		# load the shooting logs data frame (if it exists) and bind the new one
		if (file.exists("rda/shooting_logs.rda")) {
			load("rda/shooting_logs.rda")
			shooting_logs <- bind_rows(shooting_logs, new_shots)
		} else {
			shooting_logs <- new_shots
		}
		
		# save shooting_logs
		save(shooting_logs, file = "rda/shooting_logs.rda")
	}
	
	
	print("------------------------")
	Sys.time() - start_time
}
