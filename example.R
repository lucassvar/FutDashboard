source("functions.R")

# get the URLs of the 2023/24 and 2024 Leagues
get_all_match_urls(2024)

# run the extraction code 100 times
replicate(100, extract_match_data("11/3/6"))

# Check characters used in the Player column
unique(unlist(strsplit(as.character(match_logs$Player), "")))