###=== data preparation- combining farm fields together ===###

#--- load the packages ---#
library(gtools)
library(plm)
library(here)
library(plyr)
library(gam)

#--- loading the data ---#

field_data <- jsonlite::fromJSON(
  file.path(
    here::here("Data", "CommonData"),
    "field_parameter.json"
  ),
  flatten = TRUE
) %>%
  data.table() %>%
  .[, field_year := paste(farm, field, year, sep = "_")]

#--- filter the corn 2022 data ---#
field_year_ls <- field_data[year == 2023 & crop == 'corn', ]$field_year



###=========== combine all the data together =========###

data_all <- data.table()

for (i in 1:length(field_year_ls)) {
  
print(i)

  #6, 11
ffy <- field_year_ls[66]

if (file.exists(here::here("Data", "Growers", ffy, "Analysis-Ready", "analysis_data.rds"))){
  data <- readRDS(here::here("Data", "Growers", ffy, "Analysis-Ready", "analysis_data.rds")) %>% 
    rename(replace = c("urea_rate" = "n_rate"))
  
  
  
  data$field <- ffy
  # data$field <- strsplit(ffy, "_")[[1]][2]
  data$year <- strsplit(ffy, "_")[[1]][3]
  
  data_all <- rbind.fill(data_all, data)
  }
nrow(data_all)

data_all$field %>% unique()

data_all$field[data_all$field == "Gingerich_Gingerich1_2023"] <- "Gingerich_Field1_2023"

# saveRDS(data_all, "/Users/qianqiandu/Library/CloudStorage/Box-Box/Field_fixed_effects_EndogeneityTest/Shared/Data/Data_for_analysis/data2023.rds")

}

#--- check if the loop works ---#
data_all$field %>% unique()


names(data_all)

nrow(data_all)

###========= Clean 2023 corn data ========###

corn_2023 <- readRDS(here::here("Shared", "Data", "Data_for_analysis", "data2023.rds")) %>% 
  dplyr::select(year, field, yield, n_rate, elev, slope, curv, tpi, clay, sand, silt, water_storage, geometry)

set <- c("farm", "field", "year", "yield", "obs_id", "yield_vol", "s_rate", "n_rate", "slope", "aspect", "curv", "twi","elev","clay", "sand","water_storage", "tpi")


data_select <- data_all %>% 
  dplyr::select(set) %>% 
  data.table()

nrow(data_select)


##############==== Get the analysis ready data ====############

#--- get the field-year combinations ---#
data_2021 <- readRDS("/Users/qianqiandu/Library/CloudStorage/Box-Box/Field_fixed_effect_2/Shared/Data/Data_for_analysis/data2021.rds")

data_2022 <- readRDS("/Users/qianqiandu/Library/CloudStorage/Box-Box/Field_fixed_effect_2/Shared/Data/Data_for_analysis/data2022.rds")

all_data <- rbind(data_2021, data_2022)

ffy_list <- all_data$field %>% unique()

field_year_ls_corn <- field_data[farm =="CLC" & year == 2022 & crop == "corn"]$field_year

ffy = "Hord_F104_2022"

# input = "S"

# result_soy_seed <- data.table()

get_results <- function(ffy, input){
  if(file.exists(here::here("Reports", "Growers", ffy, "analysis_results.rds"))){
    main_results <- readRDS(here::here("Reports", "Growers", ffy, "analysis_results.rds")) %>% 
      filter(input_type == input) %>%
      mutate(opt_ur = as.vector(opt_ur[[1]]),
             prof_diff_ur_gc = as.vector(prof_diff_ur_gc[[1]]),
             prof_diff_vr_ur = as.numeric(prof_diff_vr_ur[[1]]),
             trial = ffy) %>%
      dplyr::select(trial, gc_rate, opt_ur, control, treat_ls, prof_diff_ur_gc, prof_diff_vr_ur, top_var)
    
  }
  
  result_soy_seed <- rbind.fill(result_soy_seed, main_results)
  
  result_soy_seed
  
  return(main_results)
}


###=========== clean the combined data =========###

data_select_yield <- data_select[is.na(yield) == TRUE, ] %>% 
  .[, -"yield"] %>% 
  setnames("yield_vol", "yield")



hist(data_select_yield$yield)

nrow(data_select_yield)


data_select_yield[farm == c("SoutheastResearchAndExtensionCenter", "Rendel", "DuraClub"), ]


data_select_try <- data_select[is.na(yield) == FALSE, -c("yield_vol", "twi")]


###=========== save the data for analysis =========###

# write_rds(data_all, "/Users/qianqiandu/Library/CloudStorage/Box-Box/Field_fixed_effect-2/Shared/Data/Data\ for\ analysis/all_data_22.rds")

check_data <- readRDS("/Users/qianqiandu/Library/CloudStorage/Box-Box/Field_fixed_effect-2/Shared/Data/Data\ for\ analysis/all_data_30.rds")

nrow(check_data)

identical(check_data, data_all)

data_all$field %>% unique()

filter(data_all, field == "Pukwana")


check_data$field %>% unique()

see <- readRDS("/Users/qianqiandu/Library/CloudStorage/Box-Box/DIFM_HQ/Data/Growers/Rendel_BostonWest_2022/Analysis-Ready/analysis_data.rds")
nrow(see)

###=========== try GAM model =========###

data_select

gam_model <- gam(yield ~ lo(n_rate) + lo(slope, aspect, curv, elev, clay, sand, water_storage, tpi), data = data_select, na = na.gam.replace)







###===== Yield response regression with pooled OLS ======###

pooled_ols <- lm(yield_vol ~ n_rate + I(n_rate^(1/2)) + slope + aspect+ tpi, data = data_all)
summary(pooled_ols)

###===== Yield response regression with OLS and individual fixed effect ======###

ols_dum <- plm(yield_vol ~ n_rate + I(n_rate^(1/2)) + factor(field)-1 +slope + aspect+ tpi, data = data_all)

summary(ols_dum)

###=== pooled ols ===###

# ols <- plm(yield_vol ~ n_rate, data=data_all)
summary(fixed)


### fixed effect ###
fixed_effect <- plm(yield_vol ~ n_rate + slope + aspect+ tpi, data=data_all, index = "farm", model="within")
summary(fixed_effect)

###===== Hausman text ======###

phtest(fixed_effect, pooled_ols)

# the p-value is < 0.05 then the fixed effects model is a better choice

