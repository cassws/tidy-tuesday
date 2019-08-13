

library(tidyverse)

# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

# read in raw data
raw_df <- url %>% 
  read_csv() %>% 
  janitor::clean_names() 

# clean up some of the factors and playtime data
clean_df <- raw_df %>% 
  mutate(price = as.numeric(price),
         score_rank = word(score_rank_userscore_metascore, 1),
         average_playtime = word(playtime_median, 1),
         median_playtime = word(playtime_median, 2),
         median_playtime = str_remove(median_playtime, "\\("),
         median_playtime = str_remove(median_playtime, "\\)"),
         average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
           as.numeric(str_sub(average_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
           as.numeric(str_sub(median_playtime, 4, 5)),
         metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3))) %>% 
  select(-score_rank_userscore_metascore, -score_rank, -playtime_median) %>% 
  rename(publisher = publisher_s, developer = developer_s)

View(raw_df)
View(clean_df)
summary(clean_df)
clean_df$owners
str_split_fixed(clean_df$owners, "\\s\\.\\.\\s", 2)

' .. '
clean_df <- clean_df %>%
  mutate(owners = str_remove_all(owners, ",")) %>%
  separate(owners, c("owners_low", "owners_high"), "\\s\\.\\.\\s") %>%
  mutate(owners_mean = (as.integer(owners_low) + as.integer(owners_high))/2)

clean_df <- clean_df %>%
  mutate(year = as.Date(release_date, format="%b %d, %Y")) %>%
  mutate(year = format(year, "%Y"))

clean_df$year

clean_df %>% 
  ggplot(aes(x=owners, y=median_playtime)) +
  geom_boxplot()


clean_df %>% 
  filter(owners_low >= 1000 ) %>%
  filter(median_playtime > 0) %>%
  ggplot(aes(x=metascore, y=median_playtime)) +
  geom_point(shape=1)

clean_df %>%
  group_by(owners_mean) %>%
  summarise(total = n()) %>%
  ggplot(aes(x=owners_mean, y=total)) +
  geom(po)

