from ehrql import create_dataset 
from ehrql.tables.tpp import patients, practice_registrations, vaccinations, apcs, ons_deaths
from ehrql import codelist_from_csv  



dataset = create_dataset()  
  
# Define a date range for inclusion  
start_date = "2019-01-01"  
end_date = "2022-03-31"  
  
# Identify patients who had an active registration anytime in this period  
has_registration = (  
    practice_registrations  
    .where(practice_registrations.start_date <= end_date)  
    .where((practice_registrations.end_date.is_null()) | (practice_registrations.end_date >= start_date))  
    .exists_for_patient()  
)  
  
# Define study population  
dataset.define_population(has_registration)  
dataset.configure_dummy_data(  
    population_size=2500,  
    additional_population_constraint = (
    patients.sex.is_in(["male", "female", "intersex"])
))
  
# Define index date for age calculation (e.g., last day of study period)  
index_date = end_date  
  
# Define variables  
dataset.sex = patients.sex  
dataset.age = patients.age_on(index_date)  
  
# Add COVID vaccination variable  
latest_vaccine = (  
    vaccinations  
    .where(vaccinations.target_disease == "SARS-2 CORONAVIRUS")  
    .sort_by(vaccinations.date)  
    .last_for_patient()  
)  
dataset.has_covid_vaccine = latest_vaccine.exists_for_patient()


# Load COVID ICD-10 codelist  
covid_icd10_codes = codelist_from_csv("codelists/opensafely-covid-identification.csv", column="icd10_code")  
  
# Filter hospital admissions for COVID  
covid_admissions = apcs.where(  
    apcs.primary_diagnosis.is_in(covid_icd10_codes)  
)  
  
# Add COVID admission variables  
dataset.has_covid_admission = covid_admissions.exists_for_patient() 
dataset.underlying_cause_death = ons_deaths.underlying_cause_of_death.is_in(covid_icd10_codes)
from ehrql import when

dataset.status = when(dataset.underlying_cause_death).then("dead").otherwise("alive")

