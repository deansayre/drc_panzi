rm(list = ls())

pacman::p_load(rio,
               svDialogs,
               tidyverse)

pacman::p_load_gh("deansayre/Rtesunate")

source("panzi_routine_import_dhis2.R")


filtered_totals_all <- filtered_totals %>% 
  list_rbind(names_to = "var")

filtered_totals_long <- filtered_totals_all %>% 
  pivot_longer(cols = 3:last_col()) %>% 
  mutate(value = as.numeric(value))

totals_long_grouped <- filtered_totals_long %>% 
  group_by(var, month) %>% 
  summarise(total = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month1 = factor(word(month, sep = "_", 1), 
                         levels = c("janvier",
                                    "fevrier", 
                                    "mars", 
                                    "avril", 
                                    "mai",
                                    "juin", 
                                    "juillet", 
                                    "aout", 
                                    "septembre",
                                    "octobre", 
                                    "novembre", 
                                    "decembre")),
         year = as.numeric(word(month, sep = "_", 2)), 
         month2 = case_match(month1, 
                             "janvier" ~ 1,
                             "fevrier" ~ 2, 
                             "mars" ~ 3, 
                             "avril" ~ 4, 
                             "mai" ~ 5,
                             "juin" ~ 6, 
                             "juillet" ~ 7 , 
                             "aout" ~ 8, 
                             "septembre" ~ 9,
                             "octobre" ~ 10, 
                             "novembre" ~ 11, 
                             "decembre"~ 12), 
         date = lubridate::mdy(paste0(month2, "-15-", year))
         ) %>% 
  arrange(year, month1) 

order <- unique(totals_long_grouped$month)


plots <- totals_long_grouped %>% 
  mutate(month = factor(month, levels = order))%>% 
  split(.$var) %>% 
  map2(names(.), 
       function(x,y){x %>% 
        ggplot()+
        geom_col(aes(x = date, y = total))+
        theme_minimal()+
        labs(title = y, 
             x = "Date", 
             y = "Count")
       }
      )

