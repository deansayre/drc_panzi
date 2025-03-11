pacman::p_load(rio, 
               tmap, 
               raster,
               malariaAtlas, 
               here,
               sf,
               tidyverse)

est_inc <- import("wmr2024_annex_4f.xlsx") %>% 
  janitor::row_to_names(1) %>% 
  rename(country = 1, 
         pop = 3, 
         cases = 5, 
         deaths = 8) %>% 
  fill(country, .direction = "down") %>% 
  filter(Year == 2023) %>% 
  filter(str_detect(country, "Angola|Benin|Burkina Faso|Cameroon|Côte d’Ivoire|Democratic Republic of the Congo|Ethiopia|Ghana|Guinea|Kenya|Liberia|Madagascar|Malawi|Mali|Mozambique|Niger|Nigeria|Rwanda|Senegal|Sierra Leone|Uganda|Tanzania|Zimbabwe|Zambia")) %>%
  filter(!country %in% c("Papua New Guinea", "Guinea-Bissau", 
                        "Equatorial Guinea")) %>% 
  mutate(across(c(cases,deaths), ~as.numeric(.x)), 
         inc = 1000*cases/pop, 
         cfr = deaths/cases, 
         cases_mil = cases/1000000, 
         drc = case_when(country == "Democratic Republic of the Congo" ~ "drc", 
                         .default = "non_drc")) 
  

cases_who <- est_inc %>% 
        ggplot()+
  geom_col(aes(x = reorder(country, cases_mil), y = cases_mil, fill = drc),
           color = "grey20")+
    scale_fill_manual(values = c("cadetblue", "antiquewhite3"))+
  theme_minimal()+
  coord_flip()+
  labs(y = "Estimated Cases, 2023 (millions)", 
       x = element_blank())+
  theme(legend.position = "none")
  



ggsave(filename = here::here("mb_presentation", "cases_est.png"), 
       plot = cases_who, 
       dpi = 700, 
       width = 13.2, 
       height = 6.4, 
       units = "in")

inc_who <- ggplot(est_inc)+
  geom_col(aes(x = reorder(country, inc), y = inc, fill = drc), color = "grey20")+
  scale_fill_manual(values = c("cadetblue", "antiquewhite3"))+
  theme_minimal()+
  coord_flip()+
  labs(y = "Estimated Annual Incidence, 2023 (per 1000)", 
       x = element_blank())+
  theme(legend.position = "none")

ggsave(filename = here::here("mb_presentation","inc_est.png"), 
       plot = inc_who, 
       dpi = 700, 
       width = 13.2, 
       height = 6.4,
       units = "in")





pf_inc22 <- raster(here("202406_Global_Pf_Incidence_Rate_COD_2022.tiff"))
# pop_dens <- raster(here("cod_general_2020.tif"))

drc_sf <- malariaAtlas::getShp(ISO = "COD") %>% 
  st_as_sf()
# 
# sf::st_write(drc_sf, here("spatial", "drc_nat.shp"))

drc_sf <- sf::st_read(here("spatial", "drc_nat.shp"))

geo_codes_dhis2 <- rio::import("C:/Users/omp2/OneDrive - CDC/mgd/drc/geoFeatures_2025-01-19.rds")

drc_prov <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  filter(levelName == "Province (DPS)") %>% 
  mutate(name = str_remove_all(name, " Province"), 
         name = str_remove(name, paste0(word(name, 1, sep = " "), " "))) %>% 
  left_join(rio::import("drc_prev_provence.xlsx"), by = c("name" = "prov")) %>% 
  mutate(prev = as.numeric(str_replace_all(prev, ",", "."))) %>% 
  sf::st_as_sf()

zds <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  filter(levelName == "Zone de Sante" & str_detect(name, "Panzi")) %>% 
  sf::st_as_sf()


diff <- st_difference(drc_sf, zds) 


# name as 'Panzi Zone de Santé'

health_area <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  filter(str_detect(parentName, "Panzi")) %>% 
  filter(!str_detect(parentName, "^sk")) %>% 
  filter(levelName != "Formation Sanitaire (FOSA)") %>% 
  sf::st_as_sf()


fs <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  filter(parentName %in% health_area$name) %>% 
  sf::st_as_sf()



pal1 <- colorFactor(c(ochRe::ochre_palettes[["nolan_ned"]][[4]], 
                      ochRe::ochre_palettes[["nolan_ned"]][[5]]), domain = c(1, 2))


at <- c(0, seq(0.2,0.6,0.05), 1)
cb <- leaflet::colorBin(palette = "inferno", bins = at, domain = at, 
               na.color = NA)

bins <- seq(0, 100, 10)
pal <- colorBin("YlOrRd", domain = drc_prov$prev, bins = bins)


kin <- sf::st_as_sf(tibble(lat = -4.346703, 
                           long = 15.307295), 
                    coords = c("long", "lat"), 
                    crs = 4326)


drc_unmasked <- tm_shape(pf_inc22)+
  tm_raster(palette = "magma", style = "cont")+
  tm_shape(drc_prov)+
  tm_borders()+
  tm_shape(drc_sf)+
  tm_borders(lwd = 3)+
#  tm_shape(kin)+
#  tm_markers()+
  tm_layout(legend.show = FALSE, frame = FALSE)

drc_masked <- tm_shape(pf_inc22)+
  tm_raster(palette = "magma", style = "cont")+
  tm_shape(drc_prov)+
  tm_borders()+
  tm_shape(diff)+
  tm_fill("azure4", fill_alpha = 0.85)+
  tm_borders()+
  tm_shape(drc_sf)+
  tm_borders(lwd = 3)+
  tm_layout(legend.show = FALSE, frame = FALSE)
  

drc_inc_legend <- tm_shape(pf_inc22)+
  tm_raster(palette = "magma", style = "cont", title = "")+
  tm_shape(drc_prov)+
  tm_borders()+
  tm_shape(drc_sf)+
  tm_borders(lwd = 3)+
  tm_layout(legend.only = TRUE, frame = FALSE)


walk2(list(drc_masked, drc_masked, drc_inc_legend),
      map(list("drc_masked", "drc_masked", "drc_inc_legend"), 
          ~paste0(here::here("mb_presentation", .x), ".png")),
      ~tmap_save(tm = .x, 
          filename = .y, 
       dpi = 700, 
       width = 13.2, 
       height = 6.4,
       units = "in")
)

unique(drc_routine$admin_level_1)

drc_routine <- rio::import("DRC.csv") %>% 
  group_by(year, month, admin_level_1) %>% 
  summarise(total = sum(confirmed_cases, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(admin_level_1 == "Kwango" & year != 2018) %>% 
  group_by(month) %>% 
  mutate(ave = mean(total)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("total", "ave")) %>% 
  filter(name == "total" | (name == "ave" & year == 2017)) %>% 
  ungroup() %>% 
  mutate(year = case_when(name == "ave" ~ "Average", 
                           .default = as.character(year))) %>% 
  ggplot()+
  geom_line(aes(x = month, y = value, group = year, color = name, 
                linewidth = name))+
  scale_color_manual(values = c("firebrick4", "grey65"))+
  scale_discrete_manual("linewidth", values = c(2, 0.3))+
  scale_x_continuous(breaks=seq(1, 12, 1), 
                     labels = c(rep("", 2), "March", 
                                rep("", 2), "June", 
                                rep("", 2), "Sept", 
                                rep("", 2), "Dec"))+
  theme_minimal()+
  theme(legend.position = "none", 
        panel.grid.minor.x = element_blank())+
  labs(x = "Month", 
       y = "Cases Reported (n)")

ggsave(plot = drc_routine, 
       filename = here::here("mb_presentation","seasonal_case_counts.png"), 
       dpi = 700, 
       width = 13.2, 
       height = 6.4,
       units = "in")



### Lab results

lab_loc <- here::here("linelist", "lab_linelist.xlsx")

lab_1 <- rio::import(lab_loc) %>% 
  Rtesunate::act_clean() %>% 
  select(-1) %>% 
  filter(!is.na(id)) %>% 
  filter(!is.na(noms))

lab_2 <- rio::import(here::here("linelist", "lab_linelist.xlsx"), which = 2) %>% 
  Rtesunate::act_clean() %>% 
  rename(id = labid) %>% 
  mutate(dte_de_reception = as.character(dte_de_reception)) %>% 
  filter(!is.na(id))


lab <- bind_rows(lab_1, lab_2) %>% 
  filter(!is.na(noms))

lab1 <- lab %>% 
  mutate(influenza = case_when(influenza == "type.a" ~ "type_a", 
                               .default = influenza))

lab2 <- lab1 %>% 
  summarise(flu_count = sum(influenza == "type_a", na.rm = TRUE), 
            flu_prev = round(100*flu_count/(sum(influenza == "negatif", na.rm = TRUE)+
                                              sum(influenza == "type_a", na.rm = TRUE)),1),
            covid_count = sum(sars_cov2 == "positif", na.rm = TRUE),
            covid_prev = round(100*covid_count/(sum(sars_cov2 == "negatif", na.rm = TRUE)+
                                                  sum(sars_cov2 == "positif", na.rm = TRUE)),1), 
            covid_flu_count = sum(sars_cov2 == "positif" & influenza == "type_a", na.rm = TRUE),
            covid_flu_prev = round(100*covid_flu_count/sum(!is.na(sars_cov2)& !is.na(influenza)), 1),
            mal_count = sum(tdr_palu == "positif", na.rm = TRUE),
            mal_flu_count = sum(tdr_palu == "positif" & influenza == "type_a", na.rm = TRUE),
            mal_prev = round(100*mal_count/(sum(tdr_palu == "negatif", na.rm = TRUE)+
                                              sum(tdr_palu == "positif", na.rm = TRUE)), 1),
            mal_flu_prev = round(100*mal_flu_count/sum(!is.na(tdr_palu)& !is.na(influenza)), 1),
            all_neg_count = sum(influenza == "negatif" & 
                                  sars_cov2 == "negatif" &
                                  tdr_palu == "negatif", na.rm = TRUE),
            all_neg_prev = round(100*all_neg_count/sum(!is.na(influenza) & !is.na(sars_cov2) & !is.na(tdr_palu)), 1)
  ) %>% 
  pivot_longer(everything()) %>% 
  mutate(type = word(name, -1, sep = "_"), 
         name = str_remove_all(name, c("_count|_prev"))) %>% 
  pivot_wider(names_from = "type", values_from = "value") %>% 
  mutate(name = case_match(name,
                           "flu" ~ "Grippe", 
                           "covid" ~ "COVID", 
                           "covid_flu" ~ "Grippe et COVID",
                           "mal" ~ "Paludisme", 
                           "mal_flu" ~ "Grippe et Palu", 
                           "all_neg" ~ "Tous négatifs")) %>% 
  rename(Maladie = name, 
         Nombre = count, 
         `Prevalance (%)` = prev)

lab_graph <- tibble(dz = factor(c("Malaria", "Influenza A", "Influenza A", 
                                  "COVID", "COVID", "Negative", "Negative"),
                                levels = c("Malaria", 
                                           "Influenza A", 
                                           "COVID",
                                           "Negative")), 
       value = c(49, 35, 27, 61, 14, 75, 20), 
       color = factor(c(1,2, 1, 2, 1, 2, 1)))


a <- ggplot(lab_graph)+
  geom_col(aes(x = dz, y = value, fill = color))+
  scale_fill_manual(values = c("grey35", "#FFFFFF00"))+
  theme_minimal()+
  labs(x = "Laboratory Results", 
       y = "Count")+
  theme(legend.position = "none")

ggsave(plot = a, 
       filename = here::here("mb_presentation","lab_results.png"), 
       dpi = 700, 
       width = 13.2, 
       height = 6.4,
       units = "in")


care_seeking <- rio::import("case_mgmt.xlsx") %>% 
  mutate(across(c(2:5), ~as.numeric(str_remove_all(str_replace_all(.x, ",", "."), "\\(|\\)")))) %>% 
  select(-perc_act) %>% 
  pivot_longer(cols = c(2:4)) %>% 
  filter(name %in% c("perc_sought_care", "perc_tested")) %>% 
  mutate(color = factor(case_when(province == "Kwango" ~ 1, 
                           .default = 2))) %>% 
  split(.$name) %>% 
  map2(list("Care seeking (0--5 with fever, %)", 
            "Malaria testing (0--5 with fever, %)"), ~.x %>% 
        ggplot()+
        geom_col(aes(x = reorder(province, value), y = value, fill = color), 
                 color = "grey20")+
        scale_fill_manual(values = c("cadetblue", "antiquewhite3"))+
        theme_minimal()+
        labs(x = "Province", 
             y = .y)+
         theme(legend.position = "none", 
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  )

filenames <- map(list("care_seeking", "testing"), 
    ~paste0(here::here("mb_presentation", .x), ".png"))


walk2(care_seeking, filenames, ~ggsave(filename = .y, plot = .x,
dpi = 700, 
width = 13.2, 
height = 6.4, 
units = "in")
)

pat_pres <- tibble(sympt = c(rep("Cough",3), rep("Rhinorrhea", 3)), 
                   type = c(rep(c("point", "lower", "upper"), 2)), 
                   value = c(0.65, 0.5, 0.9, 0.59, 0.4, 0.9)
) %>% 
  pivot_wider(names_from = "type", values_from = "value") %>% 
  ggplot()+
  geom_point(aes(x = sympt, y = point), 
             size = 3)+
  geom_errorbar(aes(x = sympt, ymin = lower, ymax = upper, width = 0.2))+
  geom_hline(yintercept = 1, linetype = "dotdash")+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 2))+
  labs(y = "aOR recieving RDT", 
       x = element_blank())

ggsave(filename = here::here("mb_presentation", "presenting_symp.png"), 
       plot = pat_pres,
       dpi = 700, 
       width = 13.2, 
       height = 6.4, 
       units = "in")

data_6 <- rio::import("data_6.xlsx")


mal <- filter(data_6, tdr1 == "positif" & cough_fever != "both")
resp <- filter(data_6, cough_fever == "both" & tdr1 == "negatif")
resp_mal <- filter(data_6, tdr1 == "positif" & cough_fever == "both")
unk <- filter(data_6, tdr1 == "unknown")

temp <- filter(data_6, cough_fever == "both" | tdr1 == "positif")
other <- filter(data_6, !n %in% temp$n)


all_data <- list(data_6, 
                 mal, 
                 resp, 
                 resp_mal, 
                 unk,
                 other)

all_weeks <- tibble(week = unique(data_6$year_week_factor))

all_epicurves <- map(all_data, 
                     ~.x %>% 
                       group_by(year_week_factor) %>% 
                       summarise(count = n(), 
                                 death_count = sum(decede_non == "decede", na.rm = TRUE)) %>% 
                       ungroup() %>% 
                       full_join(all_weeks, by = c("year_week_factor" = "week")) %>% 
                       mutate(across(c(count, death_count), ~replace_na(.x, 0)))
)


epicurves <- map2(all_epicurves, list("grey",
                                           "firebrick4", 
                                           "cadetblue4", 
                                           "blueviolet",
                                           "darkslategrey",
                                           "wheat4"),
                  function(x, y){
                    max <- max(all_epicurves[[1]]$count, na.rm = TRUE)
                    max_d <- max(all_epicurves[[1]]$death_count, na.rm = TRUE)
                    
                    cases <- sum(x$count, na.rm = TRUE)
                    deaths <- sum(x$death_count, na.rm = TRUE)
                    
                    a <-ggplot(x)+
                      geom_col(aes(x = year_week_factor, y = count), 
                               color = "black", 
                               fill = y)+
                      coord_cartesian(ylim = c(0, max+20))+
                      scale_x_discrete(breaks = unique(data_6$year_week_factor), 
                                       labels = unique(data_6$year_week_factor))+
                      labs(x = element_blank(), 
                           y = "Count", 
                           subtitle = paste0("Total number of cases = ", cases))+
                      theme(axis.text.x = element_blank())
                    
                    b <- ggplot(x)+
                      geom_col(aes(x = year_week_factor, y = death_count), 
                               fill = y, 
                               color = "black")+
                      coord_cartesian(ylim = c(0, max_d+5))+
                      labs(x = "Week", 
                           y = "Deaths", 
                           subtitle = paste0("Total number of deaths = ", deaths, 
                                             " (CFR = ", round(100*deaths/cases, 1), "%)"))+
                      theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                                       hjust = 1))
                    
                    cowplot::plot_grid(a,
                                       NULL,
                                       b,
                                       ncol = 1,
                                       rel_heights = c(1,-0.15,1),
                                       align = "hv", 
                                       axis = "tblr")
                    
                  })


filenames <- map(list("epi_all", "epi_mal_only", 
                      "epi_resp_only", "epi_resp_mal", 
                      "epi_unknown", "epi_other"), 
                 ~paste0(here::here("mb_presentation", .x), ".png"))


walk2(epicurves, filenames, ~ggsave(filename = .y, plot = .x,
                                       dpi = 700, 
                                       width = 13.2, 
                                       height = 6.4, 
                                       units = "in")
)

all_hf <- data_6 %>% 
  distinct(etablissement_recu, year_week_factor)

unique(data_6$etablissement_recu)
unique(filter(data_6, decede_non == "decede")$etablissement_recu)

heatmap_hf_a <- data_6 %>% 
  filter(decede_non == "decede") %>% 
  group_by(etablissement_recu, year_week_factor) %>% 
  summarise(deaths = n()) %>% 
  ungroup() %>% 
  full_join(all_hf) %>% 
  mutate(deaths = case_when(is.na(deaths) ~ 0, 
                            .default = deaths)) %>% 
  complete(etablissement_recu, year_week_factor, fill = list(deaths = 0)) 

total_deaths <- heatmap_hf_a %>% 
  group_by(etablissement_recu) %>% 
  summarise(total = sum(deaths)) %>% 
  ungroup() %>% 
  mutate(etablissement_recu1 = fct_reorder(factor(etablissement_recu), total, 
                                           .desc = TRUE)) %>% 
  arrange(total)


heatmap_hf <- heatmap_hf_a %>% 
  mutate(etablissement_recu = factor(etablissement_recu, 
                                     levels = unique(total_deaths$etablissement_recu1))) %>% 
  ggplot()+
  geom_tile(aes(x = year_week_factor, y = etablissement_recu, fill = deaths))+
  scale_fill_viridis_c(option = "B")+
  labs(x = "Weeks", 
       y = element_blank(), 
       fill = "Deaths")+
  theme(axis.text.x = element_text(angle = 90), vjust = 1, 
        hjust = 1)


total_deaths %>% 
  mutate(total = -1*total) %>% 
  ggplot()+
  geom_col(aes(y = total, x = desc(etablissement_recu1)))+
    coord_flip()+
  
