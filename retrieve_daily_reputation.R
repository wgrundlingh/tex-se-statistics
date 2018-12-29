# 0 Package loading ####

library(tidyverse)
library(lubridate)
library(httr)
library(rvest)
library(magrittr)

# 1 Setup ####

base_folder <- '~/Research/LaTeX/tex.sx'


# 2 Load user database ####

user_data <- read_csv(
  file = paste0(
    base_folder,
    '/Data/tex_users.csv'
  )
) %>% filter(
  Id > 0 # Exclude user -1 (Community)
  #  (Reputation < 1000) & (Reputation >= 200)
) %>% select(
  Id
) %>% arrange(
  Id
)


# 3 Download user daily reputation ####

all_user_rep_history <- NULL
user_count <- 0

if (file.exists(paste0(
  base_folder,
  '/Data/temp_reputation_history.csv'
))) {
  all_user_rep_history <- read_csv(
    file = paste0(
      base_folder,
      '/Data/temp_reputation_history.csv'
    )
  )
  
  user_data <- user_data %>% filter(
    Id > max(
      all_user_rep_history$USERID
    )
  )
}

for (userId in user_data$Id) {
  
  user_count <- user_count + 1 # Move to next user
  print(paste0('Working on user ', userId, ' (', user_count, '/', nrow(user_data), ')'))

  user_profile <- tryCatch(
    content(
      GET(
        paste0(
          'https://tex.stackexchange.com/users/',
          userId,
          '/?tab=reputation&sort=graph'
        )
      ),
      'parsed'
    ) %>% html_nodes(
      'script'
    ) %>% extract(
      15
    ) %>% html_text()
  )
      

  if (length(user_profile) > 0) {

    # Extract only relevant parts of rep_line
    user_profile <- user_profile %>% str_sub(
      start = str_locate(user_profile, '\\[\\[')[1,'end'] + 1,
      end = str_locate(user_profile, '\\]\\]')[1, 'start'] - 1
    ) %>% str_split(
      '\\],\\['
    ) %>% unlist() %>% str_sub(
      start = 1L,
      end = 15L
    ) %>% trimws()
    
    # Translate rep_line into data frame; convert data into usable form
    rep_history <- as.data.frame(
      user_profile
    ) %>% separate(
      col = user_profile, 
      into = c(
        'YEAR', 'MONTH', 'DAY', 'REP'
      ),
      sep = ','
    ) %>% mutate(
      REP = REP %>% as.numeric(),
      DATE = paste(
        YEAR, MONTH, DAY, sep = '-'
      ) %>% ymd(),
      USERID = userId
    ) %>% select(
      DATE,
      USERID,
      REP
    ) %>% filter(
      REP != 0 # Remove zero-reputation days
    )
    
    if (is.null(all_user_rep_history)) {
      all_user_rep_history <- rep_history
    } else { # !is.null(all_user_rep_history)
      all_user_rep_history <- all_user_rep_history %>% bind_rows(
        rep_history
      )
    } # if
  
  
  }
  
  
  # Write all_user_rep_history to file temporarily
  if (user_count %% 500 == 0) {
    all_user_rep_history %>% write_csv(
      path = paste0(
        base_folder,
        '/Data/temp_reputation_history.csv'
      )
    )
  } else {
    Sys.sleep(2.5) # This seems sufficient to avoid too many requests
  }
  
}

all_user_rep_history <- all_user_rep_history %>% left_join(
  users %>% rename(USERID = Id) %>% select(USERID, DisplayName),
  by = 'USERID'
) %>% rename(
  DISPLAYNAME = DisplayName
) %>% select(
  USERID,
  DISPLAYNAME,
  DATE,
  REP
)

all_user_rep_history %>% write_csv(
  path = paste0(
    base_folder,
    '/Data/user_reputation_history.csv'
  )
)
file.remove(
  'temp_reputation_history.csv'
)
