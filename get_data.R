# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Scrape Masters leaderboard data every ten minutes and write to a
# Google sheet.

# Packages
library(tidyverse)
library(RSelenium)          # Selenium server
library(rvest)              # Web scraping
library(googlesheets4)
library(lubridate)

# Interactive authorization puts token in secrets. This only has to be done
# once when the script is first run.
# gs4_auth(email = "kenyonboyd@gmail.com", cache = ".secrets")
# gs4_deauth()

# Scrape this page
url <- "https://2022.masters.com/en_US/scores/index.html"
# https://www.masters.com/en_US/scores/index.html

# Authorize access to Google sheets
gs4_auth(email = "kenyonboyd@gmail.com",
         cache = ".secrets")

# Start Docker
system("open --background -a Docker", wait = TRUE)
Sys.sleep(40)
message("Finished starting Docker.")

# Start Selenium
system("docker run -d -p 4445:4444 selenium/standalone-firefox", wait = TRUE)
message("Finished starting Selenium server.")

# Connect to server
remDr <- remoteDriver(port = 4445L)
Sys.sleep(5)
remDr$open()
Sys.sleep(5)
message("Finished connecting to server.")

# Navigate to URL and wait for page to load
remDr$navigate(url)
Sys.sleep(5)
message(paste0("Finished navigating to ", url, "."))

# The leaderboard page has two different types of boards: "Traditional" (which
# is what we want), and "Over/Under". When the page first loads, it's set to
# Over/Under. The code below (1) finds the drop-down menu that controls the type
# of leaderboard and clicks it to reveal the "Over/Under" and "Traditional"
# options, then (2) finds the "Traditional" option and clicks it. This loads the
# traditional table, which can then be scraped. Once this process is completed,
# the page can be refreshed and will stay on the traditional table.
# Click to open menu
webElems <- remDr$findElements(
    using = "xpath",
    value = '//*[contains(concat( " ", @class, " " ), concat( " ", "select-menu-tabs2dropdown", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "navigation_down_arrow", " " ))]')
resHeaders <- unlist(lapply(webElems, function(x) { x$getElementText() }))
target <- webElems[[which(resHeaders == "Over/Under")]]
target$clickElement()

# Click to select traditional table
webElems <- remDr$findElements(
    using = "xpath",
    value = '//*[contains(concat( " ", @class, " " ), concat( " ", "option", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]')
resHeaders <- unlist(lapply(webElems, function(x) { x$getElementText() }))
target <- webElems[[which(resHeaders == "Traditional")]]
target$clickElement()

# Scrape leaderboard every 10 minutes and send to Google sheet
while (TRUE) {
    
    # User message
    message(paste("Initiating leaderboard refresh at", Sys.time()))
    
    # Refresh page
    remDr$refresh()
    Sys.sleep(6)
    message("Page refresh complete.")
    
    # Leaderboard doesn't seem to be structured as a table. Instead, read in all
    # data elements as a character string.
    leader_char <- remDr$getPageSource() %>%
        .[[1]] %>%
        read_html() %>%
        html_elements(".data") %>%
        html_text2()
    
    # Convert character string to a table by first finding the indices of player
    # names.
    # NOTE: This could break if there's a player name that's less than two
    # characters, of if other characters are introduced into the leaderboard.
    # Currently only missed cut (MC) and withdrawn (WD) are at least two
    # characters. One character uses are "T", as in "T3" (tied for third place),
    # and "F" (finished).
    name_idx <- which(str_detect(leader_char, "^[A-Za-z]{2}") &
                          !str_detect(leader_char, "^(MC|WD)"))
    
    # Based on where player names occur, we can compute the indices for the
    # start and end of each row in the table.
    start <- name_idx - 1
    end <- start + 9
    
    # Iterate over start & end indices to extract rows and combine into a table
    leader_tab <- map2(start, end, function(s, e) {
        as_tibble(leader_char[s:e]) }) %>%
        list_cbind() %>%
        t() %>%
        as_tibble()
    
    # Add column names
    colnames(leader_tab) <- c("place", "player", "total_under", "thru", "today_under", "R1", "R2", "R3", "R4", "total_score")
    
    # Fix rows for players who missed the cut
    # NOTE: Could break if a different number of players are allowed to make the
    # cut
    leaderboard <- bind_rows(
        
        # These rows are okay
        leader_tab[1:52, ],
        
        # Fix these rows
        leader_tab[53:nrow(leader_tab), ] %>%
            select(place, player, thru, today_under, R3) %>%
            rename(
                R1 = thru,
                R2 = today_under,
                total_score = R3)) %>%
        
        # Convert to numeric
        mutate(
            across(c("total_under", "today_under"),
                   ~ if_else(.x == "E", "0", .x)),
            across(
                c("total_under", "today_under", R1:total_score), ~ as.integer(.x)),
            
            # Add a datetime stamp. Google sheets converts all datetimes to UTC,
            # so subtract six hours to show mountain time.
            last_updated = Sys.time() - hours(6))
    
    # Write to Google sheet
    write_sheet(
        data = leaderboard,
        ss = "1-Mq_xMxERqTPUnSerpig5NU9oDVj4a09KFH1WSSedBw",
        sheet = "leaderboard")
    
    # Pause 600 seconds before running loop again
    message("Waiting for next loop...")
    Sys.sleep(600)
}
