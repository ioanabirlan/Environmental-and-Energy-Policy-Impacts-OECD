
install.packages("plm")
library(plm)
install.packages("plm")
install.packages("openxlsx")
library(plm)
library(openxlsx)


america_df <- read.xlsx("America Panele.xlsx", sheet = 1)
europe_df <- read.xlsx("Europe Panel.xlsx", sheet = 1)


install.packages("plm")
install.packages("openxlsx")
library(plm)
library(openxlsx)


america_df$Country <- "America"
europe_df$Country <- "Europe"


clean_column_names <- function(df) {
  names(df) <- gsub("[[:punct:]]", "_", names(df))
  names(df) <- gsub(" ", "_", names(df))
  return(df)
}

america_df <- clean_column_names(america_df)
europe_df <- clean_column_names(europe_df)


print(names(america_df))
print(names(europe_df))

dependent_var <- "Production_based_CO2_emissions"
independent_vars <- c("Total_primary_energy_supply", "Renewable_energy_supply___total_energy_supply",
                      "Municipal_waste_recycled_or_composted___treated_waste", "Mortality_from_exposure_to_ambient_PM2_5",
                      "Welfare_costs_of_premature_mortalities_from_exposure_to_ambient_PM2_5__GDP_equivalent", 
                      "Development_of_environment_related_technologies___all_technologies",
                      "Relative_advantage_in_environment_related_technology", 
                      "Environmentally_related_taxes___GDP",
                      "Terrestrial_protected_area___land_area", 
                      "Population_density__inhabitants_per_km2")

fe_formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+")))


america_df <- pdata.frame(america_df, index = c("Country", "Year"))
europe_df <- pdata.frame(europe_df, index = c("Country", "Year"))


fe_model_america <- plm(fe_formula, data = america_df, model = "within")
summary(fe_model_america)


fe_model_europe <- plm(fe_formula, data = europe_df, model = "within")
summary(fe_model_europe)


fe_formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+")))

america_df <- pdata.frame(america_df, index = c("Country", "Year"))
europe_df <- pdata.frame(europe_df, index = c("Country", "Year"))

fe_model_america <- plm(fe_formula, data = america_df, model = "within")
summary(fe_model_america)


fe_model_europe <- plm(fe_formula, data = europe_df, model = "within")
summary(fe_model_europe)

re_model_america <- plm(fe_formula, data = america_df, model = "random")
summary(re_model_america)


re_model_europe <- plm(fe_formula, data = europe_df, model = "random")
summary(re_model_europe)


hausman_test_america <- phtest(fe_model_america, re_model_america)
print(hausman_test_america)

hausman_test_europe <- phtest(fe_model_europe, re_model_europe)
print(hausman_test_europe)
