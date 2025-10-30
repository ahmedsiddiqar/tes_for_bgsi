from ehrql import create_dataset  
from ehrql.tables.tpp import patients, practice_registrations, vaccinations, apcs  
from ehrql import codelist_from_csv  



dataset = create_dataset()  
  
# Define a date range for inclusion  
start_date = "2020-01-01"  
end_date = "2021-03-31"  
  
# Identify patients who had an active registration anytime in this period  
has_registration = (  
    practice_registrations  
    .where(practice_registrations.start_date <= end_date)  
    .where((practice_registrations.end_date.is_null()) | (practice_registrations.end_date >= start_date))  
    .exists_for_patient()  
)  
  
# Define study population  
dataset.define_population(has_registration)  
dataset.configure_dummy_data(population_size=1000)  
  
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
dataset.latest_covid_vaccine_date = latest_vaccine.date

# Load COVID ICD-10 codelist  
covid_icd10_codes = codelist_from_csv("codelists/opensafely-covid-identification.csv", column="icd10_code")  
  
# Filter hospital admissions for COVID  
covid_admissions = apcs.where(  
    apcs.primary_diagnosis.is_in(covid_icd10_codes)  
)  
  
# Add COVID admission variables  
dataset.has_covid_admission = covid_admissions.exists_for_patient()  
