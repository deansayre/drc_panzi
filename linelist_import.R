rm(list = ls())


if (!require("pacman")) install.packages("pacman")

pacman::p_load(rio, 
               Hmisc,
               stringdist,
               tidyverse)

pacman::p_install_gh("deansayre/Rtesunate")
pacman::p_install_gh("const-ae/ggupset")
pacman::p_install_gh("moodymudskipper/powerjoin")


data <- rio::import(here::here("linelist", "LL_GPM_PANZI_23_01_2025_AS_RAJ.xlsx")) %>% 
  Rtesunate::act_clean() %>% 
  rename(n = 1) %>% 
  mutate(id = row_number())

lab_1 <- rio::import(here::here("linelist", "lab_linelist.xlsx")) %>% 
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

hf_dhis2 <- rio::import("dhis2_hf_names.xlsx") %>% 
  mutate(hf = case_when(str_detect(`kg Panzi Zone de Santé`, "Aire") ~ NA_character_, 
                                   .default = `kg Panzi Zone de Santé`),
         `kg Panzi Zone de Santé` = case_when(!is.na(hf) ~ NA_character_, 
                                              .default = `kg Panzi Zone de Santé`)
  ) %>% 
  fill(`kg Panzi Zone de Santé`, .direction = "down") %>% 
  filter(!is.na(hf))

rio::export(hf_dhis2, file = "hf_all_dhis2.xlsx")
         

######################################### number missing observations ##########

b <- map(names(data), 
         function(.x){x = sym(.x)
         data %>% 
           filter(is.na({{x}}))
         }
)

missing_obs <- map(b, ~nrow(.x)) %>% 
  as_vector() %>% 
  as_tibble() %>% 
  add_column(Variable = names(data), 
             .before = 1) %>% 
  rename(`Observations Missing` = value) 


rm(b)

######################################## description/distributions #############


data_1a <- data %>% 
  mutate(tratement = str_replace_all(tratement, c("vit_c" = "vitc",
                                                  "vitce" = "vitc_vite",
                                                  "vit_ce" = "vitc_vite",
                                                  "vitace" = "vitc_vite",
                                                  "vita_c" = "vitc",
                                                  "vitac" = "vita_vitc", 
                                                  "vit_b12" = "vitb12",
                                                  "vit_b6" = "vitb6",
                                                  "at_60" = "at60", 
                                                  "para_injct" = "para-inject", 
                                                  "paracetamol_injct" = "para-inject",
                                                  "parainject" = "para-inject",
                                                  "para_inject" = "para-inject",
                                                  "para_inj" = "para-inject",
                                                  "acide_folique" = "acide-folique", 
                                                  "para" = "paracetamol", 
                                                  "artesunate_inj" = "artesunate-inj")
                                     )
  )# there will be an issue with entries containing multiple words as 
   # single entry when go to change pull individuals from these lists


data_char <- data_1a %>% 
  select(where(is.character)) %>% 
  select(-c(nom_et_postnom))

entered_values <- map(names(data_char), 
                      function(x){data_char %>% 
                          pull({{x}}) %>% 
                          unique()
                      }) %>% 
  set_names(names(data_char)) %>% 
  map(~as.data.frame(unlist(.x))
      )%>% 
  list_rbind(names_to = "variable") %>% 
  rename(entry = 2) %>% 
  arrange(variable, entry)


rio::export(entered_values, file = "entered_free_text.xlsx")

# spelling changes made and saved to separate file 'entered_free_text_with_spelling_change.xlsx'

spelling_changes <- rio::import("entered_free_text_with_spelling_change.xlsx") %>% 
  filter(!is.na(change_to))

data1 <- data_1a %>% 
  linelist::clean_variable_spelling(wordlists = spelling_changes,
                                    from = 2, 
                                    to = 3, 
                                    spelling_vars = 1)

data2 <- data1 %>%  
  mutate(age_unit = word(age, sep = "_", -1), 
         age1 = str_remove_all(age, paste0("_", age_unit)), 
         age2 = as.numeric(str_replace_all(age1, "_", "\\.")),
         age_years = case_when(age1 == "2ans" ~ 2, 
                               age1 == "4" ~ 4, 
                               age1 == "8_ans" ~ 8, 
                               age1 == "3mois" ~ 3/12,
                               age1 == "1omois" ~ 1/12, 
                               age1 == "18mois" ~ 18/12, 
                               age1 == "7mois" ~ 7/12, 
                               age1 == "1an" ~ 1,
                               age1 == "1ans" ~ 1,
                               age1 == "8ans" ~ 8,
                               age1 == "4_1_2" ~ 4.5, 
                               age1 == "3_ans_11" ~ 3+11/12,
                               age1 == "1_1_2" ~ 1.5,
                               age1 == "2_1_2" ~ 2.5,
                               age1 == "4_1_2" ~ 4.5,
                               age1 == "9mois" ~ 9/12,
                               age1 == "38a" ~ 38,
                               age_unit %in% c("m", "mois", "mos") ~ age2/12, 
                               age_unit %in% c("semaine", "semaines") ~ age2/52,
                               age_unit %in% c("ans", "asns", "an", "nas") ~ age2, 
                               .default = 999), 
         week = tsibble::yearweek(date_de_notification),
         paludisme_1 = case_when(paludisme == "palu" ~ "oui", 
                                 .default = paludisme), 
         tdr1 = case_when(tdr == "non.rensegne" ~ NA, 
                          .default = tdr)
         )


####################################  search for duplicates using fuzzy matching
#### NOT complete as of 26 January, 2025. Response leadership to weigh in ######

# ensure Id provided is unique identifier
#  Hmisc::describe(data2$n)

data_dup <- data2 %>% 
  dplyr::select(date_de_notification, 
                name = nom_et_postnom,
                as, 
                sexe,
                age, 
                n) %>% 
  mutate(n = as.numeric(n)) %>% 
  full_join(., ., by = character()) %>% 
  filter(n.x < n.y) %>% 
  mutate(
    name.dist = stringdist(name.x, name.y), 
    as_same = as.x == as.y, 
    duration = lubridate::time_length(interval(date_de_notification.x, 
                                               date_de_notification.y), 
                                      "day")) %>% 
  filter(name.dist < 3) %>% 
  select(
    name.dist, 
    name.x, name.y, 
    sexe.x, sexe.y,
    date_de_notification.x, date_de_notification.y, 
    jours = duration, 
    as_same, as.x, as.y, 
    n.x, n.y) %>% 
  mutate(id_to_discard = NA) %>% 
  arrange(name.dist, jours, as_same)
  



# for manual examination
rio::export(data_dup, "doublants_possibles.xlsx")

doublants_vrai <- rio::import("doublants_possibles_vrai.xlsx")

data3 <- data2 %>% 
  filter(!n %in% doublants_vrai$id_to_discard)

################################################################################


############################################## clinical presentation ###########


# meets case definition? revision of case definition?


sympt_specific_out <- function(x, db){
  y <- sym(x)
  a <- db %>% 
    filter({{y}} == "oui") %>% 
    summarise(nombre = n(), 
              tpr = round(100*sum(tdr1 == "positif", na.rm = TRUE)/(sum(tdr1 == "negatif", 
                                                                        na.rm = TRUE) + 
                                                                      sum(tdr1 == "positif",
                                                                          na.rm = TRUE)),1),
              taux_mortalite = round(100*sum(evolution == "decede", na.rm = TRUE)/(sum(evolution != "decede", 
                                                                                       na.rm = TRUE)+sum(evolution == "decede", na.rm = TRUE)),2))
  
  return(a)
  }


a <- map(names(data3[14:30]), ~sympt_specific_out(.x, data3)) %>% 
  set_names(names(data3[14:30])) %>% 
  list_rbind(names_to = "presentation")

rio::export(a, "outcome_par_symptom.xlsx")

a_flex <- a %>% 
  mutate(presentation = str_to_title(str_replace_all(presentation, "_", " "))) %>% 
  arrange(-nombre) %>% 
  rename("Symptômes" = presentation, 
         "Nombre de Cas" = nombre, 
         "Taux de positivité - TDR (%)" = tpr, 
         "Taux de mortalité (%)" = taux_mortalite) %>% 
  flextable::flextable() %>% 
  flextable::colformat_num(big.mark = "", digits = 1) %>% 
  flextable::autofit()

flextable::save_as_docx(a, path = paste0(here::here("linelist", 
                                                    "output", 
                                                    "symptom_outcome_all.docx")))

outcome_by_presentation <- data3 %>% 
  select(n, 14:30) %>% 
  pivot_longer(-n) %>% 
  filter(value == "oui") %>% 
  group_by(n) %>% 
  mutate(list_sympt = list(name)) %>% 
  ungroup() %>% 
  left_join(select(data3, n, tdr1, evolution), 
            by = "n") %>% 
  group_by(list_sympt) %>% 
  summarise(count = n(), 
            tpr = round(100*sum(tdr1 == "positif", na.rm = TRUE)/(sum(tdr1 == "negatif", 
                                                                      na.rm = TRUE) + 
                                                                    sum(tdr1 == "positif",
                                                                        na.rm = TRUE)),1),
            taux_mortalite = round(100*sum(evolution == "decede", na.rm = TRUE)/(sum(evolution != "decede", 
                                                                                    na.rm = TRUE)+sum(evolution == "decede", na.rm = TRUE)),1)
  ) %>% 
  arrange(-count, -taux_mortalite, -tpr)


missing_one <- outcome_by_presentation %>% 
  rowwise() %>% 
  filter(!all(c("fievre", "toux") %in% list_sympt)) 
  
missing_both <- outcome_by_presentation %>% 
    rowwise() %>% 
    filter(!any(c("fievre", "toux") %in% list_sympt)) 
  
symptom_combos <- map(list(outcome_by_presentation, missing_one, missing_both),
    ~.x %>% rowwise()%>% 
      mutate(list_sympt =  str_to_title(paste(list_sympt, collapse = ", ")), 
             list_sympt = str_replace_all(list_sympt, "_", " ")) %>% 
      rename(presentation_clinique = list_sympt, 
             tdr_positivite_pourcent = tpr, 
             nombre = count) %>% 
      ungroup()
)
    
symptom_combos_flex <- symptom_combos %>% 
  map(~.x %>% 
        rename("Symptômes" = presentation_clinique, 
               "Nombre de Cas" = nombre, 
               "Taux de positivité - TDR (%)" = tdr_positivite_pourcent, 
               "Taux de mortalité (%)" = taux_mortalite) %>% 
        flextable::flextable() %>% 
        flextable::colformat_num(big.mark = "", digits = 1) %>% 
        flextable::autofit()
      )

map2(symptom_combos_flex, 
     list("symptom_outcome_all", "symptom_outcome_missing_fever_and_or_cough", 
          "symptom_outcome_missing_both_fever_and_cough"), 
     function(x, y){flextable::save_as_docx(x, path = paste0(here::here("linelist", 
                                                                        "output"), "/", y, ".docx"))
     })


datatx <- data3 %>% 
  select(n, tratement) %>% 
  mutate(tx_list = str_split(tratement, "_")) %>% 
  filter(!is.na(tx_list)) %>% 
  unnest_longer(tx_list)

datatx_unique <- datatx %>% 
  select(tx_list) %>% 
  distinct(tx_list) %>% 
  arrange(tx_list)

rio::export(datatx_unique, "linelist/output/tratement_sp.xlsx")

treatment_spelling <- rio::import(here::here("linelist", 
                                             "output", 
                                             "tratement_sp_edit.xlsx")) %>% 
  filter(!is.na(change_to))

datatx_sp <- datatx %>% 
  left_join(treatment_spelling, by = "tx_list") %>% 
  mutate(tx_list_sp = case_when(!is.na(change_to)~ change_to, 
                                .default = tx_list)) %>% 
  select(n, tx_list = tx_list_sp) %>% 
  filter(tx_list != "rm") %>% 
  group_by(n) %>% 
  arrange(tx_list, .by_group = TRUE) %>% 
  ungroup() %>% 
  filter(!str_detect(tx_list, "^\\d")) 

treatment_list <- datatx_sp %>% 
  group_by(n) %>% 
  summarise(tx = list(tx_list)) %>%
  rowwise() %>% 
  mutate(tx1 = paste(unlist(tx), collapse = "_"))

unique(datatx_sp$tx_list)

data4 <- left_join(data3, treatment_list, by = "n") %>% 
  select(-tratement) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(any_artemisinins_a = list(map(c("act", 
                                          "artesunate",
                                          "aretesunate.rectal", 
                                          "asaq", 
                                          "artemether"),
                                        function(x) str_detect(tx1, fixed(x)))),
         any_artemisinins = any(any_artemisinins_a == TRUE), 
         any_abx_a = list(map(c("amoxicilline", 
                                "azithromycine", 
                                "ceftriaxone", 
                                "erytromycine",
                                "bactrim",
                                "genta"),
                              function(x) str_detect(tx1, fixed(x)))),
         any_abx = any(any_abx_a == TRUE), 
         complications = str_replace_all(complications, "broncho_pulmonaire", 
                                         "broncho.pulmonaire"))

################################################################################
#############################################  complications ###################


data_comp <- data4 %>% 
  select(n, complications) %>% 
  mutate(list = str_split(complications, "_")) %>% 
  filter(!is.na(complications)) %>% 
  unnest_longer(list)


data_comp_unique <- data_comp %>% 
  select(list) %>% 
  distinct(list) %>% 
  arrange(list)

rio::export(data_comp_unique, "linelist/output/complications_sp.xlsx")

complications_spelling <- rio::import(here::here("linelist", 
                                             "output", 
                                             "complications_sp_edit.xlsx")) %>% 
  filter(!is.na(change_to))



data_comp_sp <- data_comp %>% 
  left_join(complications_spelling, by = "list") %>% 
  mutate(comp_list_sp = case_when(!is.na(change_to)~ change_to, 
                                .default = list)) %>% 
  select(n, list = comp_list_sp) %>% 
  filter(list != "rm") %>% 
  group_by(n) %>% 
  arrange(list, .by_group = TRUE) %>% 
  ungroup() %>% 
  filter(!str_detect(list, "^\\d")) 

comp_list <- data_comp_sp %>% 
  group_by(n) %>% 
  summarise(comp = list(list)) %>%
  rowwise() %>% 
  mutate(tx1 = paste(unlist(comp), collapse = "_"))

unique(data_comp_sp$list)

data5 <- left_join(data4, comp_list, by = "n")




##############################################################################
##################################################  basic charts  ############

names(data5)

temp <- data5 %>% 
  distinct(etablissement_recu, as)

rio::export(temp, "unique_facilities.xlsx")



data5_a <- data5 %>% 
  mutate(decede_non = case_when(is.na(evolution) ~ NA_character_, 
                                evolution == "decede" ~ "decede", 
                                .default = "non_decede"), 
         age_cat1 = cut(age_years, c(0, 2, 5, 15, Inf)),
         age_cat2 = cut(age_years, c(0, 5, Inf)),
         duration_symp_consult = lubridate::time_length(interval(date_debut_des_signes,
                                                                 date_de_consultation), 
                                                        "days"), 
         consult_pre_dec01 = case_when(date_de_consultation < "2024-12-01" ~ "pre_12_01", 
                                       date_de_consultation >= "2024-12-01" ~ "post_12_01", 
                                       is.na(date_de_consultation) ~ NA_character_, 
                                       .default = "error"), 
         year_week_factor = factor(paste(week), levels = unique(data2$week)))



prop_var <- list("age_cat1",
                 "age_cat2", 
                 "sexe", 
                 "as",
                 "fievre",
                 "toux", 
                 "consult_pre_dec01")

demo_prop_list <- map(prop_var, function(x){
           y <- sym(x)
           a <- data5_a %>%
             group_by(decede_non, {{y}}) %>% 
             summarise(count = n()) %>% 
             pivot_wider(names_from = "decede_non", 
                         values_from = "count") %>% 
             rename(category = 1)

         return(a)
         }
         ) %>% 
  set_names(prop_var) %>% 
  list_rbind(names_to = "element") %>% 
  mutate(across(c(decede, non_decede), ~replace_na(.x, 0))) %>% 
  mutate(total = rowSums(select(., decede, non_decede)), 
         cfr = round(100*decede/total, 1))
  


 ######  focus on location #########

case_loc_a <- data5_a %>% 
  group_by(decede_non, as, etablissement_recu) %>% 
  summarise(nombre = n(), 
            reported_cases = list(as.Date(date_de_consultation)),
            first_case = map(reported_cases, ~ min(.x)), 
            last_case = map(reported_cases, ~ max(.x)), 
            cases_with_rdt_results = sum(!is.na(tdr1)), 
            cases_with_pos_rdt = sum(tdr1 == "positif", na.rm = TRUE), 
            no_rdt = sum(is.na(tdr1)), 
            any_art = sum(any_artemisinins == TRUE)) %>% 
  ungroup() %>% 
  mutate(any_art = replace_na(any_art, 0), 
         tpr = cases_with_pos_rdt/cases_with_rdt_results) %>% 
  select(-reported_cases) %>% 
  pivot_wider(names_from = decede_non, 
              values_from = c(nombre, 
                              first_case, last_case, 
                              cases_with_rdt_results, 
                              cases_with_pos_rdt,
                              tpr,
                              no_rdt, 
                              any_art), 
              names_glue = "{decede_non}_{.value}")
  
### needs updating below with new variables; as well as adding totals  

case_loc <- case_loc_a %>% 
  relocate(as, etablissement_recu, 
           non_decede_nombre, non_decede_first_case, 
           non_decede_last_case, 
           decede_nombre, decede_first_case, decede_last_case) %>% 
  mutate(across(contains("nombre"), ~replace_na(.x, 0)), 
         cfr = round(100*decede_nombre/(decede_nombre+non_decede_nombre),1))



### TO DO... graphs by facility of reported cases (panel a), 
### rdts performed (panel b), tpr (panel c), and Case fatality rate (panel d)
### by week

hf_graphics_a <- data5_a %>% 
  group_by(etablissement_recu, year_week_factor) %>% 
  summarise(number = n(), 
            deaths = sum(evolution == "decede", na.rm = TRUE), 
            rdt = sum(!is.na(tdr1), na.rm = TRUE),
            perc_tested = rdt/number,
            rdt_pos = sum(tdr1 == "positif", na.rm = TRUE), 
            anti_palu = sum(any_artemisinins == TRUE, na.rm = TRUE), 
            tpr = 100*rdt_pos/rdt, 
            cfr = 100*deaths/(deaths + number), 
            deaths_mal = sum(evolution == "decede" & tdr1 == "positif", na.rm = TRUE),
            deaths_mal_tx = sum(evolution == "decede" & tdr1 == "positif" & any_artemisinins == TRUE, na.rm = TRUE),
            cfr_mal_only = 100*deaths_mal/rdt_pos, 
            cfr_mal_tx = 100*deaths_mal_tx/rdt_pos
  ) %>% 
  ungroup() 
  
a <- names(hf_graphics_a)[!names(hf_graphics_a) %in% c("etablissement_recu", 
                                                  "year_week_factor")]

b <- unlist(rep(list(0), length(a))) %>% 
  as.list() %>% 
  set_names(a)


hf_graphics <- hf_graphics_a %>%  
  complete(etablissement_recu, year_week_factor, fill = b)

  
  

graphs <- hf_graphics %>% 
  split(.$etablissement_recu) %>% 
  map(~.x %>% 
        arrange(year_week_factor)) %>% 
  map(function(x){
    case_count <- x %>% 
      ggplot()+
      geom_col(aes(x = year_week_factor, y = number))+
      theme_minimal()
    
    rdt_performed <- x %>% 
      ggplot()+
      geom_col(aes(x = year_week_factor, y = rdt))+
      theme_minimal()
    
    tpr <- x %>% 
      ggplot()+
      geom_path(aes(x = year_week_factor, y = tpr, group = 1))+
      theme_minimal()
    
    cfr <- x %>% 
      ggplot()+
      geom_path(aes(x = year_week_factor, y = cfr, group = 1))+
      theme_minimal()
    
    
    
    panels <- cowplot::plot_grid(case_count, rdt_performed,
                                 tpr, cfr, ncol = 1)
    return(panels)
  }
      
        )


hf_graphics %>% 
  ggplot()+
  geom_point(aes(x = perc_tested, y = cfr, 
                 group = etablissement_recu, 
                 color = year_week_factor, 
                 size = number))+
  theme_minimal()

### splitting week to show weekly performance by HF

scatter_by_week <- hf_graphics %>% 
  split(.$year_week_factor) %>% 
  map(~.x %>% 
        ggplot()+
        geom_point(aes(x = perc_tested, y = cfr, 
                 group = etablissement_recu, 
                 color = etablissement_recu, 
                 size = number))+
  theme_minimal()
  )




geo_codes_dhis2 <- rio::import("C:/Users/omp2/OneDrive - CDC/mgd/drc/geoFeatures_2025-01-19.rds")

zds <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  filter(levelName == "Zone de Sante" & str_detect(name, "Panzi")) %>% 
  sf::st_as_sf()

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

tmap::tmap_mode("view")
tmap::tm_shape(fs)+
  tmap::tm_dots()


temp <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  filter(str_detect(parentName, "Panzi")) %>% 
  filter(level ==4) %>% 
  sf::st_as_sf()

temp2 <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  mutate(child_graph = word(parentGraph, -1, sep = "/")) %>% 
  filter(child_graph %in% temp$id) %>% 
  sf::st_as_sf()

dhis2_names <- sf::st_drop_geometry(temp2) %>% 
  distinct(name, parentName)
  
rio::export(dhis2_names, "dhis2_names.xlsx")

map <- tmap::tm_shape(temp)+
  tmap::tm_borders()+
  tmap::tm_text("name")+
  tmap::tm_shape(zds)+
  tmap::tm_borders()+
  tmap::tm_shape(temp2)+
  tmap::tm_dots()

tmap::tmap_save(map, filename = "aire_de_sante_panzi_labeled.png")

temp1 <- geo_codes_dhis2 %>% 
  sf::st_drop_geometry() %>% 
  filter(str_detect(parentName, "sk"))



# there is another Aire de Sante called Panzi in the 
  
  deces <- data4 %>% 
  filter(evolution == "decede")
  
deces_characteristics <- deces %>% 
  group_by()



pas_de_fievre <- outcome_by_presentation_out 
  filter(!str_detect(presentation_clinique,"fievre"))

nombre_pas_fievre <- sum(pas_de_fievre$nombre)
nombre_deces_pas_fievre <- pas_de_fievre %>% 
  mutate(deces = ceiling(nombre*taux_mortalite/100)) %>% 
  summarise(n=sum(deces, na.rm = TRUE)) %>% 
  pull()

taux_mortalite_sans_fievre <- nombre_deces_pas_fievre/nombre_pas_fievre

pas_de_fievre_pas_de_toux <- pas_de_fievre%>% 
  filter(!str_detect(presentation_clinique,"toux"))


nombre_pas_fievre_pas_toux <- sum(pas_de_fievre_pas_de_toux$nombre)
nombre_deces_pas_fievre_toux <- pas_de_fievre_pas_de_toux %>% 
  mutate(deces = ceiling(nombre*taux_mortalite/100)) %>% 
  summarise(n=sum(deces, na.rm = TRUE)) %>% 
  pull()

taux_mortalite_sans_fievre_toux <- nombre_deces_pas_fievre_toux/
  nombre_pas_fievre_pas_toux


# Suggest revision of inclusion criteria and removal of those not meeting new or
# existing criteria (e.g., vomitissement seulement, etc.)



upset_all <- data3 %>% 
  select(n, 14:30) %>% 
  pivot_longer(-n) %>% 
  filter(value == "oui") %>% 
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) %>% 
  group_by(n) %>% 
  summarise(list_sympt = list(name)) %>% 
  ggplot(aes(x=list_sympt)) +
  geom_bar() +
  ggupset::scale_x_upset(n_intersections = 30)+
  theme_minimal()+
  labs(x = "Symptomes cliniques", 
       y = "Nombre")

deces_presentation <- data3 %>% 
  filter(evolution == "decede") %>% 
  select(n, 14:30) %>% 
  pivot_longer(-n) %>% 
  filter(value == "oui") %>% 
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) %>% 
  group_by(n) %>% 
  summarise(list_sympt = list(name)) %>% 
  ungroup()

deces_presentaton1 <- deces_presentation %>% 
  group_by(list_sympt) %>% 
  summarise(morts = n()) %>% 
  arrange(-morts)
  
upset_deces <- deces_presentation %>% 
  ggplot(aes(x=list_sympt)) +
  geom_bar(fill = "firebrick4") +
  ggupset::scale_x_upset(n_intersections = 50)+
  theme_minimal()+
  labs(x = "Symptomes cliniques", 
       y = "Nombre")



non_fever <- data3 %>% 
  filter(fievre != "oui") %>% 
  select(n, 14:30) %>% 
  pivot_longer(-n) %>% 
  filter(value == "oui") %>% 
  group_by(n) %>% 
  summarise(list_sympt = list(name)) %>% 
  ggplot(aes(x=list_sympt)) +
  geom_bar() +
  ggupset::scale_x_upset(n_intersections = 30)



## Merge in laboratory data

normalize_string <- function(text) {
  str_split(text, "_") %>% 
    unlist() %>% 
    sort() %>% 
    paste(collapse = "_")
}

# Normalize the 'text' columns in both data frames
linelist_normalized <- data3 %>%
  mutate(normalized_name = map_chr(nom_et_postnom, normalize_string))

lab_normalized <- lab %>%
  mutate(normalized_text = map_chr(noms, normalize_string))

data4_a <- powerjoin::power_left_join(linelist_normalized, lab_normalized, 
                           by = c(~ stringdist::stringdist(.x$normalized_name, 
                                                                     .y$normalized_text) < 5,
                                  ~ lubridate::time_length(interval(.x$date_de_notification, .y$date_de_prelevement),
                                                           "day") > -3),
                           keep = "both"
                           )%>% 
  mutate(lab_age_ans = as.numeric(age_ans), 
         lab_age_mois = as.numeric(age_mois)/12, 
         age_lab = ifelse(!is.na(lab_age_ans), lab_age_ans, lab_age_mois), 
         meme_sex = sexe.x == sexe.y, 
         age_difference = age_years - age_lab, 
         jour_not_prelev = lubridate::time_length(interval(date_de_notification, date_de_prelevement),
                                       "day"),
         name_dist = stringdist::stringdist(nom_et_postnom, noms))


temp <- data4_a %>% 
  filter(!is.na(id.y)) %>% 
  select(c(n, id.y, normalized_name, normalized_text, name_dist, date_de_notification, 
           date_de_prelevement,jour_not_prelev, 
           age_years, age_lab, age_difference, 
           sexe.x, sexe.y, meme_sex)) 

dupes4 <- data4_a %>% 
  janitor::get_dupes(n) %>% 
  select(c(n, nom_et_postnom, age_years, sexe.x, date_de_notification,
           id.y, noms, age_ans, age_mois, sexe.y, age_lab, date_de_prelevement)) %>% 
  mutate(meme_sex = sexe.x == sexe.y, 
         age_difference = age_years - age_lab, 
         jour_not_prelev = lubridate::time_length(interval(date_de_notification, 
                                                           date_de_prelevement),
                                "day")) %>% 
  group_by(n) %>% 
  arrange(age_difference, meme_sex, .by_group = TRUE) %>% 
  mutate(name_dist = stringdist::stringdist(nom_et_postnom, noms)) %>% 
  relocate(n, id.y, nom_et_postnom, noms, name_dist, 
           age_years, age_lab, age_difference, 
           sexe.x, sexe.y, meme_sex) %>% 
  mutate(remove = NA) # creates blank column to be filled in Excel

rio::export(dupes4, "lab_linelist_match_dupes.xlsx")


 
incorhence <- data1 %>% 
  mutate(, 
         mal_tdr = case_when(paludisme_1 == "non" & tdr1 == "positif" ~ "palu_non_tdr_pos", 
                             paludisme_1 == "oui" & tdr1 == "negatif" ~ "palu_oui_tdr_neg", 
                             paludisme_1 == "oui" & is.na(tdr1) ~ "palu_oui_tdr_vide",
                             .default = NA_character_)) %>% 
  filter(mal_tdr %in% c("palu_non_tdr_pos", "palu_oui_tdr_neg", "palu_oui_tdr_vide"))



#rio::export(incorhence, "tdr_palu_clinique_incoherence.xlsx")

# data starting with must change depending on 
a <- data2 %>% 
  mutate(tdr1 = case_when(tdr == "non.rensegne" ~ NA, 
                          .default = tdr), 
         week = factor(paste0(week), 
                       levels = c(unique(data2$week)))
         )%>% 
  group_by(week) %>% 
  summarise(flu_d = sum(grippe == "oui" & evolution == "decede", na.rm = TRUE), 
            flu_non_d = sum(grippe == "oui" & evolution != "decede", na.rm = TRUE), 
            tdr_weekly_d = sum(tdr1 == "positif" & evolution == "decede", na.rm = TRUE), 
            tdr_weekly_non_d = sum(tdr1 == "positif" & evolution != "decede", na.rm = TRUE), 
            palu_clinique_d = sum(paludisme_1 == "oui" & evolution == "decede", na.rm = TRUE),
            palu_clinique_non_d = sum(paludisme_1 == "oui" & evolution != "decede", na.rm = TRUE)
            ) %>% 
  ungroup() %>% 
  mutate(flu_d_cumul = cumsum(flu_d), 
         flu_non_d_cumul = cumsum(flu_non_d),
         palu_clinique_d_cumul = cumsum(palu_clinique_d), 
         palu_clinique_non_d_cumul = cumsum(palu_clinique_non_d), 
         tdr_weekly_d_cumul = cumsum(tdr_weekly_d), 
         tdr_weekly_non_d_cumul = cumsum(tdr_weekly_non_d))


b <- data2 %>% 
  mutate(tdr1 = case_when(tdr == "non.rensegne" ~ NA, 
                          .default = tdr),
         week = factor(paste0(week), 
                       levels = c(unique(data2$week)))) %>% 
  group_by(week) %>% 
  summarise(flu_d = sum(grippe == "oui" & tdr1 == "negatif" & evolution == "decede", 
                        na.rm = TRUE), 
            flu_non_d = sum(grippe == "oui" & tdr1 == "negatif" & evolution != "decede", 
                            na.rm = TRUE), 
            tdr_weekly_d = sum(tdr1 == "positif" & grippe == "non" & evolution == "decede", 
                               na.rm = TRUE), 
            tdr_weekly_non_d = sum(tdr1 == "positif" & grippe == "non" & evolution != "decede", 
                                   na.rm = TRUE), 
            autres_d = sum(grippe == "non" & tdr1 == "negatif" & evolution == "decede", 
                           na.rm = TRUE),
            autres_non_d = sum(grippe == "non" & tdr1 == "negatif" & evolution != "decede", 
                               na.rm = TRUE),
            deux_d = sum(grippe == "oui" & tdr1 == "positif" & evolution == "decede", 
                         na.rm = TRUE),
            deux_non_d = sum(grippe == "oui" & tdr1 == "positif" & evolution != "decede", 
                             na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(flu_d_cumul = cumsum(flu_d), 
         flu_non_d_cumul = cumsum(flu_non_d), 
         tdr_weekly_d_cumul = cumsum(tdr_weekly_d), 
         tdr_weekly_non_d_cumul = cumsum(tdr_weekly_non_d), 
         autres_d_cumul = cumsum(autres_d),
         autres_non_d_cumul = cumsum(autres_non_d),
         deux_d_cumul = cumsum(deux_d),
         deux_non_d_cumul = cumsum(deux_non_d)
  )

rio::export(list(data2,b),file = "excel_for_michael.xlsx")

grippe_panels <- map(list(a, b), function(x){

grippe <- x %>% 
  select(week, starts_with("flu"))%>% 
  pivot_longer(-week) %>% 
  filter(!str_detect(name, "cumul")) %>% 
  mutate(name = factor(case_when(name == "flu_d" ~ "Décédé", 
                           name == "flu_non_d" ~ "Non décédé"), 
                       levels = c("Non décédé", 
                                  "Décédé"))) %>% 
ggplot()+
  geom_col(aes(x= week, y = value, fill = name, group = name), 
           colour = "grey35", 
           position = "stack")+
  scale_fill_manual(values = c("cadetblue1","cadetblue4"))+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 1000))+
  labs(x = element_blank(), y = "Cas", title = "Grippe", fill = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "top")

grippe_cumul <- x %>% 
  select(week, starts_with("flu"))%>% 
  pivot_longer(-week) %>% 
  filter(str_detect(name, "cumul"))%>% 
  mutate(name = factor(case_when(name == "flu_d_cumul" ~ "Décédé", 
                          name == "flu_non_d_cumul" ~ "Non décédé"), 
         levels = c("Non décédé", 
                    "Décédé"))) %>% 
  ggplot()+
  geom_col(aes(x= week, y = value, fill = name, group = name), 
           colour = "grey35")+
  scale_fill_manual(values = c("cadetblue1","cadetblue4"))+
  theme_minimal()+
  labs(x = "Semaine Epidémioloque", y = "Cas Cumul", title = element_blank())+
  coord_cartesian(ylim = c(0, 5000))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

panel_grippe <- cowplot::plot_grid(grippe, NULL, grippe_cumul, 
                                   ncol = 1, 
                                   rel_heights = c(1, -0.15, 1),
                                   align = "hv", 
                                   axis = "lrtb")

return(panel_grippe)
}
)

map2(grippe_panels, map(list("grippe_all", "grippe_only"),~paste0(here::here(),"/", .x, ".png")),
     ~ggsave(filename = .y, 
             plot = .x, 
             dpi = 700, 
             width = 8, 
             height = 11, 
             units = "in"))

mortalite_grippe <- map(list(a,b), function(x){
taux_grippe <- x %>% 
  select(week, starts_with("flu"))%>% 
  pivot_longer(-week) %>% 
  pivot_wider(names_from = "name", 
              values_from = "value") %>% 
  mutate(`Par Semaine` = 100*flu_d/(flu_d+flu_non_d), 
         Cumul = 100*flu_d_cumul/(flu_d_cumul + flu_non_d_cumul)) %>% 
  select(c(week, `Par Semaine`,Cumul)) %>% 
  pivot_longer(-week)

taux_grippe_graph <- ggplot(taux_grippe)+
  geom_line(aes(x = week, y = value, group = name, color = name), 
            linewidth = 1)+
  scale_color_manual(values = c("cadetblue4", "cadetblue2"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y = "Taux de Mortalité", 
       x = "Semaine Epidémiologique", 
       color = "")+
  theme(legend.position = "top")

return(taux_grippe_graph)
})

map2(mortalite_grippe, map(list("grippe_all_mortalite", "grippe_only_mortalie"),
                           ~paste0(here::here(),"/", .x, ".png")),
     ~ggsave(filename = .y, 
             plot = .x, 
             dpi = 700, 
             width = 11, 
             height = 8, 
             units = "in"))






tdr_panels <- map(list(a, b), function(x){
  
  
tdr <- x %>% 
  select(week, starts_with("tdr"))%>% 
  pivot_longer(-week) %>% 
  filter(!str_detect(name, "cumul"))%>% 
  mutate(name = factor(case_when(name == "tdr_weekly_d" ~ "Décédé", 
                                 name == "tdr_weekly_non_d" ~ "Non décédé"), 
                       levels = c("Non décédé", 
                                  "Décédé"))) %>% 
  ggplot()+
  geom_col(aes(x= week, y = value, fill = name, group = name), 
            colour = "grey35")+
  scale_fill_manual(values = c("firebrick1","firebrick4"))+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 1000))+
  labs(x = element_blank(), y = "Cas", title = "TDR Positif", 
       fill = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "top")

tdr_cumul <- x %>% 
  select(week, starts_with("tdr"))%>% 
  pivot_longer(-week) %>% 
  filter(str_detect(name, "cumul"))%>% 
  mutate(name = factor(case_when(name == "tdr_weekly_d_cumul" ~ "Décédé", 
                                 name == "tdr_weekly_non_d_cumul" ~ "Non décédé"), 
                       levels = c("Non décédé", 
                                  "Décédé"))) %>% 
  ggplot()+
  geom_col(aes(x= week, y = value, fill = name, group = name), 
           colour = "grey35")+
  scale_fill_manual(values = c( "firebrick1","firebrick4"))+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 5000))+
  labs(x = "Semaine Epidémioloque", y = "Cas Cumul", title = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")


panel_tdr <- cowplot::plot_grid(tdr, NULL, tdr_cumul, 
                                   ncol = 1, 
                                   rel_heights = c(1, -0.15, 1),
                                   align = "hv", 
                                   axis = "lrtb")

return(panel_tdr)
})

map2(tdr_panels, map(list("tdr_all", "tdr_only"),~paste0(here::here(),"/", 
                                                         .x, 
                                                         ".png")),
     ~ggsave(filename = .y, 
             plot = .x, 
             dpi = 700, 
             width = 8, 
             height = 11, 
             units = "in"))



###############################################################################


mortalite_tdr <- map(list(a,b), function(x){

taux_tdr <- x %>% 
  select(week, starts_with("tdr"))%>% 
  pivot_longer(-week) %>% 
  #  filter(!str_detect(name, "cumul")) %>% 
  pivot_wider(names_from = "name", 
              values_from = "value") %>% 
  mutate(`Par Semaine` = 100*tdr_weekly_d/(tdr_weekly_d+tdr_weekly_non_d), 
         Cumul = 100*tdr_weekly_d_cumul/(tdr_weekly_d_cumul + tdr_weekly_non_d_cumul)) %>% 
  select(c(week, `Par Semaine`,Cumul)) %>% 
  pivot_longer(-week)

taux_tdr_graph <- ggplot(taux_tdr)+
  geom_line(aes(x = week, y = value, group = name, color = name), 
            linewidth = 1)+
  scale_color_manual(values = c("firebrick4", "firebrick2"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y = "Taux de Mortalité", 
       x = "Semaine Epidémiologique", 
       color = "")+
  theme(legend.position = "top")

})


map2(mortalite_tdr, map(list("tdr_all_mortalite", "tdr_only_mortalie"),
                           ~paste0(here::here(),"/", .x, ".png")),
     ~ggsave(filename = .y, 
             plot = .x, 
             dpi = 700, 
             width = 11, 
             height = 8, 
             units = "in"))









palu_clinique <- a %>% 
  filter(name == "Paludisme par examen clinique") %>% 
  mutate(cumul = cumsum(value)) %>% 
  ggplot()+
  geom_col(aes(x= week, y = value), fill = "firebrick1", colour = "grey35")+
  theme_minimal()+
  coord_cartesian(ylim = c(0, 1000))+
  labs(x = "Semaine Epidémioloque", y = "Cas", title = "Paludisme par examen clinique")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

palu_clinique_cumul <- a %>% 
  filter(name == "palu_clinique_cumul") %>% 
  ggplot()+
  geom_col(aes(x= week, y = value), fill = "firebrick1", colour = "grey35")+
  theme_minimal()+
  labs(x = "Semaine Epidémioloque", y = "Cas", 
       title = "Paludisme par examen clinique")+
  coord_cartesian(ylim = c(0, 5000))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


taux_palu_clinique <- a %>% 
  select(week, starts_with("palu_clinique"))%>% 
  pivot_longer(-week) %>% 
  #  filter(!str_detect(name, "cumul")) %>% 
  pivot_wider(names_from = "name", 
              values_from = "value") %>% 
  mutate(`Par Semaine` = 100*palu_clinique_d/(palu_clinique_d+palu_clinique_non_d), 
         Cumul = 100*palu_clinique_d_cumul/(palu_clinique_d_cumul + palu_clinique_non_d_cumul)) %>% 
  select(c(week, `Par Semaine`,Cumul)) %>% 
  pivot_longer(-week)

taux_palu_clinique_graph <- ggplot(taux_palu_clinique)+
  geom_line(aes(x = week, y = value, group = name, color = name), 
            linewidth = 1)+
  scale_color_manual(values = c("wheat4", "wheat2"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y = "Taux de Mortalité", 
       x = "Semaine Epidémiologique", 
       color = "")+
  theme(legend.position = "top")









tot <- data1 %>% 
  group_by(week) %>% 
  summarise(total_d = sum(evolution == "decede", na.rm = TRUE),
            total_non_d = sum(evolution != "decede", na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cumul = cumsum(total_d), 
         cumul_non_d = cumsum(total_non_d)) %>% 
  mutate(week = factor(paste0(week), 
                       levels = c(unique(data1$week))))

tot_weekly <- tot %>% 
  pivot_longer(-week) %>% 
  filter(!str_detect(name, "cumul")) %>% 
  mutate(name = factor(case_when(name == "total_d" ~ "Décédé", 
                                 name == "total_non_d" ~ "Non décédé"),
                       levels = c("Non décédé", 
                                  "Décédé"))) %>% 
  ggplot()+
  geom_col(aes(x= week, y = value, group = name, fill = name), 
           colour = "grey35")+
    scale_fill_manual(values = c("grey", "black"))+
  theme_minimal()+
  labs(x = element_blank(), y = "Cas", title = "Nombre Total des Cas", 
       fill = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "top")

tot_cumul <- tot%>% 
  pivot_longer(-week) %>% 
  filter(str_detect(name, "cumul")) %>% 
  mutate(name = factor(case_when(name == "cumul" ~ "Décédé", 
                                 name == "cumul_non_d" ~ "Non décédé"),
                       levels = c("Non décédé", 
                                  "Décédé"))) %>% 
  ggplot()+
    geom_col(aes(x= week, y = value, group = name, fill = name), 
             colour = "grey35")+
    scale_fill_manual(values = c("grey", "black"))+
    theme_minimal()+
    labs(x = "Semaine Epidémioloque", 
         y = "Cas Cumul", 
         title = element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none")

total_n <- cowplot::plot_grid(tot_weekly, NULL, tot_cumul, 
                   ncol = 1, 
                   rel_heights = c(1,-0.15, 1), 
                   axis = "tblr", 
                   align = "hv")


ggsave(total_n, filename = paste0(here::here("total_count_courbe.png")), 
       dpi = 700, 
       height = 11, 
       width = 8, 
       units = "in")


tot_taux_mortalite <- tot %>% 
  mutate(`Par Semaine` = 100*total_d/(total_d+total_non_d), 
         Cumul = 100*cumul/(cumul + cumul_non_d)) %>% 
  select(c(week, `Par Semaine`,Cumul)) %>% 
  pivot_longer(-week)

tot_taux_mortalite_graph <- ggplot(tot_taux_mortalite)+
  geom_line(aes(x = week, y = value, group = name, color = name), 
            linewidth = 1)+
  scale_color_manual(values = c("black", "grey50"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y = "Taux de Mortalité", 
       x = "Semaine Epidémiologique", 
       color = "")+
  theme(legend.position = "top")

ggsave(tot_taux_mortalite_graph, filename = paste0(here::here("total_mortalite_courbe.png")), 
       dpi = 700, 
       height = 8, 
       width = 11, 
       units = "in")



           
  
#### doublants ################################################################  




petite_graphique <- data1 %>% 
  mutate(age_cat = cut(age_years, c(0,2,5,15, Inf))) %>% 
  group_by(as) %>% 
  summarise(prop_fever = sum(fievre == 'oui', na.rm = TRUE)/n(), 
            prop_cough = sum(toux  == 'oui', na.rm = TRUE)/n(), 
            prop_rhinorrhee = sum(rhinorrhee == 'oui', na.rm = TRUE)/n(), 
            prop_asthenie_physique = sum(asthenie_physique == 'oui', 
                                         na.rm = TRUE)/n()) %>% 
  ungroup() %>% 
  pivot_longer(-as) %>% 
  ggplot()+
  geom_col(aes(x = as, y = value, group = name, fill = name),
           position = "dodge")+
  scale_fill_manual(values = c("grey", "cadetblue4", 
                               "firebrick4", "blueviolet")
  )+
  theme_minimal()+
  labs(x = "Aire de Sante", 
       y = "Proportion")+
  theme(axis.text = element_text(angle = 90))

