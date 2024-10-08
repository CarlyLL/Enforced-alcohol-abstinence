# Enforced-alcohol-abstinence
## About this project
Data and codes used in my ADRUK Data First Fellowship as detailed [here](https://www.adruk.org/our-work/browse-all-projects/adr-uk-research-fellows-the-first-users-of-the-data-first-probation-and-criminal-justice-linked-datasets-new63fdc70162868654419743) and in the following [OSF pre-registration](https://osf.io/2bzf3).


## Guide to the R scripts
### R Code excerpt 1: Linking probation data files
This code reads in and appends the probation data flat files and does the same for the requirement files. It then creates a wide version of the requirement type and appends this to the flat file records.

### R Code excerpt 2: Appending Home Office Offence codes
This code recodes crime types in the Probation data into Home Office offence categories.

The 2022 Home Office offence classification file as at November 2022 (downloaded from [GOV.UK](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjTu-6npPf-AhUHJ8AKHQwFABoQFnoECAgQAQ&url=https%3A%2F%2Fassets.publishing.service.gov.uk%2Fgovernment%2Fuploads%2Fsystem%2Fuploads%2Fattachment_data%2Ffile%2F1118266%2Foffence-group-classification-june-2022.xlsx&usg=AOvVaw2Zr1iluDWlyp0qFLxloIfR) (offence-group-classification-june-2022.csv)) was used as a lookup file from which to match mo_codes in the Probation data file.

### R Code excerpt 3: Classifying alcohol-defined offences
This code classifies crime types in the Probation data based on whether they are alcohol-defined (namely, those crimes defined by the presence of involvement of alcohol supply and/or consumption) offences or not.

To discern between alcohol-defined and non-alcohol defined offences, the approach taken was to identify a subset of offence codes within the published Home Office offence classification (2022) based on the following stem words: "drink", "drunk", "alcohol", "influence of" and "intox" appearing in the text (Detailed_offence) accompanying the relevant offence codes (Offence_code). 

### R Code excerpt 4: Probation data analysis
To be added in due course. 

### R Code excerpt 5: Probation and magistrates person-event data linkage
This code details the steps taken to link person-events using the probation and magistrates' court records and create proxy reoffending measures (re-representations before the court).

### R Code excerpt 6: Linked Probation and criminal courts data analysis
To be added in due course. 

