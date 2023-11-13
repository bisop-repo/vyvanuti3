# Abakyses

## Serious course, BA12

Outcome: Infection, leading to hospitalization with covid as a primary reason (PrimPricinaHospCOVID==1) such that, by 30 days from the infection, either oxygen therapy (Kyslik) or Ventilation/ECMO (UPV_ECMO) happens.

Infections taken into account: BA12 (determined by the discrimination analysis or having emerged from 2022-01-31 to 2022-05-23)

Start date (T=0): 2022-01-01 

End date: 2023-08-16 minus 30 days (for all the infections to have "fair" chances to develop into serious outcome)

Data: whole population

Mothod: Cox, SeriousCovidProxy ~ Immunity + DCCI + AgeGr + Sex


## Serious course, BA45+

Outcome: see BA12

Infections taken into account: BA45 (determined by the discrimination analysis or having emerged from 2022-08-01 to 2022-10-24) or later variants (having emerged from 2022-10-25)

Start date: 2022-07-01 

End date: 2023-08-16 minus 30 days 

Data: whole population

Mothod: Cox, SeriousCovidProxy ~ Immunity + DCCI + AgeGr + Sex

## Long covid BA12 within infected

Outcome: Long covid diagnosis by 183 days from infection

Infections taken into account: BA12 (see above)

Start date: 2021-12-01 

End date: 2023-03-31 minus 183 days 

Data: infected by BA12 variant

Mothod: Logistic regression, LongCovid ~ Immunity + DCCI + AgeGr + Sex


š# Remarks

In Cox analysis input files, records with covariates given which there are no events, are excluded (without change of results)


# Running the Code

## Prerequisities

CMake, C++, R (sith packages listed in workscript/Script.R), pdfLatex


## Instalation

```
cd Vyvanuti
mkdir data
cd convertool
git clone https://github.com/cyberklezmer/orpp
mkdir bin
cd bin
cmake .. 
make
cd ..
cd ..
```

Next, the input data has to be copied into 
```
repository root/data/data_20230816.csv
```
The source data (version as of comes from 16 April 2023) is avalable upon approval of Institute of Health information and Statistics of the Czech Republic (https://www.uzis.cz/). 


## Running the analyses

```
./all
```

Typically the run taks several hours and large RAM (more than 16GB) is needed. The code creates the results needed for the main text (v3/report/results; however, they have to be further semi-manually preprocessed using Fig1.ods, Figure 1 has been creatd manually, Figure 2 using appropriate PNG files from v3/report/results, Table 1 semi-manually using script in suppscripts/graph) and the supplementary material (v3/report/report.pdf). 


