# dataloading multiple datasets downloaded from DHIS2 on 20 January, 2025
# stored in folders/subfolders in "data" folder of parent directory

### Choices to make ##########################################################

# The threshold for the number of times a facility could have missed reporting
# a specific element is coded below as 'threshold' and may be changed. 
# Often, zero missing reports are sought after, but this may severely limit
# the amount of available data. 

# Downloaded data also go back to 2019. But can choose to filter to a more c
# current date.

# The option to truncate available data early is also an option due to the 
# incompleteness of most recent months. 'end' are months to not include.

# Deviation sets the threshold for absolute outlier dectection (to be used as 
#  a multiplier of MAD)

threshold <- 5
starting_year <- 2020
end <- "decembre_2024"

deviation <- 10


################################################################################

pacman::p_load(rio,
               svDialogs,
               tidyverse)

pacman::p_load_gh("deansayre/Rtesunate")


test <- map(list.dirs(here::here("data", "complete")), 
            ~list.dirs(.x))

names <- map(test, function(x){
  if(length(x) > 1){NULL}
  else {str_replace_all(str_remove_all(x, paste0(here::here("data", "complete"), "/")), "/", "\\?")}
}
)

test_1 <- map(test, 
              function(x){if(length(x) > 1){NULL} else {
                a <- rownames(file.info(list.files(x,full.names = TRUE)))
                b <- map(a, ~rio::import(.x) %>% 
                           mutate(across(where(is.character), ~str_replace_all(.x, "<", "under")), 
                                  across(where(is.character), ~str_replace_all(.x, ">", "over"))) %>% 
                           Rtesunate::act_clean() %>% 
                           split(.$dataname)
                         )
                return(b)}})%>% 
  set_names(names) %>% 
  compact() %>% 
  list_flatten(name_spec = "{outer}_{inner}") %>% 
  map(~list_rbind(.x, names_to = "data_name"))

test_1a <- test_1 %>% 
  keep(str_detect(names(.), "_1$"))

test_1b <- test_1 %>% 
  keep(str_detect(names(.), "_2$"))


test_2 <- map2(test_1a, test_1b, 
               ~left_join(.x, .y, by = c("data_name",
                                        "organisationunitid", 
                                        "dataid"))
               ) %>%
  map(~.x %>% 
        split(.$data_name)) %>% 
  set_names(., str_remove_all(word(names(.), sep = "\\?", 2), "_1$")) %>% 
  list_flatten(name_spec = "{inner}_{outer}")




duplicated_names <- map(test_2, function(x){x %>% 
                                        mutate(name_match = organisationunitname.x == organisationunitname.y) %>% 
                                        filter(name_match == FALSE | is.na(name_match))}
  )


duplicated_ids <- map(test_2, function(x){x %>% 
      janitor::get_dupes(organisationunitid)}
)

raw_data <- map(test_2, function(x){x %>% 
    select(-data_name) %>% 
    sjmisc::rotate_df() %>% 
    slice(2, 1, 3:n()) %>% 
    janitor::row_to_names(1) %>% 
    janitor::clean_names() %>% 
    rownames_to_column(var = "variable") %>% 
    mutate(month = factor(word(variable, -2, sep = "_"), 
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
           year = word(variable, -1, sep = "_")) %>% 
    arrange(year, month)  
  })


rm_empty1 <- raw_data %>% 
  map(function(x){a <- x %>% 
        select(-c(year, month)) %>% 
        filter(!variable %in% c( "dataid","dataname.x","dataname.y",
                                       "organisationunitcode.x",
                                       "organisationunitcode.y",
                                       "organisationunitid",
                                       "organisationunitname.y")) %>% 
              pivot_longer(cols = -"variable") %>% 
        purrr::discard(~all(is.na(.)))
  
  if(length(names(a))<=2){b <- 1} else {b <- NULL}
      return(b)
  }  
    ) %>% 
  compact()

# lists where indicator completely empty for time of interest removed 
rm_empty <- raw_data[!names(raw_data) %in% names(rm_empty1)]


total_outlier <- rm_empty %>% 
  keep(str_detect(names(.), "total")) %>% 
  map(~.x %>% 
        filter(!variable %in% c("dataid",
                                 "dataname.x","dataname.y",
                                 "organisationunitcode.x",
                                 "organisationunitcode.y",
                                 "organisationunitid",
                                 "organisationunitname.y")) %>%
        select(-c(month, year)) %>% 
        pivot_longer(cols = -"variable") %>% 
        mutate(value = as.numeric(value)) %>% 
        split(.$name) %>% 
        map(~.x %>% 
              ungroup() %>% 
              mutate(med = median(value, na.rm = TRUE), 
                     med_abdev = mad(value, na.rm = TRUE), 
                     outlier_high = med+deviation*med_abdev, 
                     outlier_low = med-deviation*med_abdev, 
                     outlier = case_when(value > outlier_high | outlier_low < outlier_low ~ TRUE,
                                         value <= outlier_high & value >= outlier_low ~ FALSE,
                                         is.na(value)~ NA))
        )
  )

# pulls those flagged for manual examination to determine if want to remove
outliers_only <- total_outlier %>% 
  map(~.x %>% 
        map(function(x){
          a <- x %>% 
              filter(outlier == TRUE)
          
          b <- nrow(a)
          
          if(b==0){c <- NULL}else{c <- a}
         return(c)
        }
        ) %>% 
        compact()
      ) %>% 
  compact()

temp1 <- ggplot()+
  geom_col(data = temp, aes(x = variable, y = value))+
  geom_point(data = filter(temp, outlier == TRUE), 
             aes(x = variable, y = value), color = "red")



total_outlier_plots_prep <- total_outlier %>% 
  map(~.x %>% 
        map(~.x %>% 
              mutate(date = lubridate::mdy(paste0(case_match(word(variable, 
                                                                  1, 
                                                                  sep = "_"), 
                                   "janvier" ~ 1,
                                              "fevrier" ~ 2, 
                                              "mars" ~ 3, 
                                              "avril" ~ 4, 
                                              "mai" ~ 5,
                                              "juin" ~ 6, 
                                              "juillet" ~ 7, 
                                              "aout" ~ 8, 
                                              "septembre" ~ 9,
                                              "octobre" ~ 10, 
                                              "novembre" ~ 11, 
                                              "decembre" ~ 12), 
                                   "-15-", 
                                   word(variable, 2, sep = "_")
                     )
                     )
                     )))

#  temp <- total_outlier_plots_prep[[1]][[6]]
total_outlier_plots <- total_outlier_plots_prep %>% 
  map(~.x %>% 
        map(function(x){
          z <- x %>% 
            filter(!is.na(value))
          
          if (nrow(z) == 0){
            
          a <- NULL
          
          }else if(nrow(filter(z, outlier == TRUE)) > 0){
          a <-ggplot()+
            geom_col(data = z, aes(x = date, y = value))+
            geom_point(data = filter(z, outlier == TRUE), 
                       aes(x = date, y = value),
                       colour = "firebrick4", 
                       size = 2, 
                       shape = 8)+
            theme_minimal()}else{a <- z %>% 
            ggplot()+
            geom_col(aes(x = date, y = value))+
            theme_minimal()+
              coord_cartesian(xlim=as_date(c("2019-01-21","2024-12-31")))}
          return(a)
        }
  ) %>% 
    compact()
  )
          



#           a <- x %>% 
#               ggplot()+
#               geom_col(data = ., aes(x = date, y = value))+
#               geom_point(data = filter(., outlier == TRUE), 
#                          aes(x = date, y = value),
#                          colour = "firebrick4", 
#                          size = 2, 
#                          shape = 8
#                          )+
#               theme_minimal())
#   )
#   



        

  
ts <- total_outlier %>% 
  map(~.x %>% 
        map(~.x %>% 
              select(variable, value) %>% 
              mutate(value = replace_na(value, 0), 
                     month = case_match(word(variable, 1, sep = "_"), 
                                        "janvier" ~ 1,
                                        "fevrier" ~ 2,
                                        "mars" ~ 3, 
                                        "avril" ~ 4, 
                                        "mai" ~ 5,
                                        "juin" ~ 6,
                                        "juillet" ~ 7, 
                                        "aout" ~ 8,
                                        "septembre" ~ 9,
                                        "octobre" ~ 10, 
                                        "novembre" ~ 11,
                                        "decembre" ~ 12), 
                     date = lubridate::mdy(paste0(month, "-15-", word(variable, 2, sep = "_")))) %>% 
              arrange(date)
        )
  )

seasonal_outlier_1 <- ts %>% 
  map(~.x %>% 
        map(~.x %>% 
              pull(value) %>% 
              as.ts( . , frequency = 12 ) %>% 
              tsoutliers(., 
                      lambda = "auto")%>% 
              as.data.frame()
            )
      )


seasonal_outlier <- ts %>% 
  map(~.x %>% 
        map(~.x %>% 
              ungroup() %>% 
              mutate(index = row_number()) 
              )
  ) %>% 
  map2(seasonal_outlier_1, 
       function(a,b)map2(a,b,
                         function(a1,b1)left_join(a1,b1, by = c("index"))
                         )
       ) %>% 
  map(~.x %>% 
        map(~.x %>% 
              mutate(seasonal_outlier = case_when(!is.na(replacements) ~ "yes", 
                                                  is.na(replacements) ~ "no", 
                                                  .default = "error"))))




#  list2env(raw_data, envir = .GlobalEnv)



#### filtering to only relatively consistent reporters for each indicator


missing_counts <- raw_data %>% 
  keep(str_detect(names(.), "count")) %>% 
  map(~.x %>% 
        filter(!str_detect(variable, 
                           "dataid|dataname|organisation")) %>% 
        filter(year >=starting_year) %>% 
        filter(!variable %in% end) %>% 
        select(-c(month, year)) %>%
        mutate(across(-1, as.numeric)) %>% 
        summarise(across(where(is.numeric), ~sum(is.na(.x)))) %>% 
        ungroup() %>% 
        pivot_longer(cols = everything(), 
                     names_to = "hf", 
                     values_to = "na_num")
        ) 

filter_out_high_na <- map(missing_counts, ~.x %>% 
                            filter(na_num <= threshold) %>% 
                            pull(hf)
)


filtered_totals <- raw_data %>% 
  keep(str_detect(names(.), "total")) %>% 
  map(~.x %>% 
        filter(!str_detect(variable, 
                       "dataid|dataname|organisation")) %>% 
      filter(year >=starting_year) %>% 
      select(-c(month, year)) %>%
      mutate(across(-1, as.numeric)) %>% 
      sjmisc::rotate_df() %>% 
      janitor::row_to_names(1) %>% 
      janitor::clean_names() %>% 
      rownames_to_column(var = "variable")) %>% 
  map2(filter_out_high_na, 
       function(x,y)x %>% filter(variable %in% y)) %>% 
  map(~.x %>% 
        sjmisc::rotate_df() %>% 
        janitor::row_to_names(1) %>% 
        janitor::clean_names() %>% 
        rownames_to_column(var = "month"))
all <- ls()

remove <- all[!all %in% c("filtered_totals","filter_out_high_na",
                          "missing_counts", "raw_data", 
                          "end", "starting_year", "threshold")]


message <- paste0("Data starts January ", starting_year, " and excludes facilities with more than ", threshold, " missing reports of the indicator of interest. Time periods excluded from this dataset include: ", end)
dlg_message(message,
  type = "ok",
  rstudio = getOption("svDialogs.rstudio", TRUE),
  gui = .GUI
)

rm(list = remove)
rm(all, remove, message)