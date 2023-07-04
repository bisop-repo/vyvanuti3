# Abakyses

## Serious course, BA12

Outcome: Infection, leading to hospitalization with covid as a primary reason (PrimPricinaHospCOVID==1) such that, by 30 days from the infection, either oxygen therapy (Kyslik) or Ventilation/ECMO (UPV_ECMO) happens.

Infections taken into account: BA12 (determined by the discrimination analysis or having emerged from 2022-01-31 to 2022-05-23)

Start date (T=0): 2022-01-01 

End date: 2022-08-21 minus 30 days (for all the infections to have "fair" chances to develop into serious outcome)

Data: whole population

Mothod: Cox, SeriousCovidProxy ~ Immunity + AgeGr + Sex


## Serious course, BA45+

Outcome: see BA12

Infections taken into account: BA45 (determined by the discrimination analysis or having emerged from 2022-08-01 to 2022-10-24) or later variants (having emerged from 2022-10-25)

Start date: 2022-07-01 

End date: 2023-04-21 minus 30 days 

Data: whole population

Mothod: Cox, SeriousCovidProxy ~ Immunity + AgeGr + Sex

# Long covid BA12 within population

Outcome: An infection followed by a long covid diagnosis by 183 days

Infections taken into account: BA12 (see above)

Start date: 2021-12-01 

End date: 2022-09-30 minus 183 days 

Data: whole population

Mothod: Cox, LongCovid ~ Immunity + AgeGr + Sex


# Long covid BA12 within infected

Outcome: Long covid diagnosis by 183 days from infection

Infections taken into account: BA12 (see above)

Start date: 2021-12-01 

End date: 2022-09-30 minus 183 days 

Data: infected by BA12 variant

Mothod: Logistic regression, LongCovid ~ Immunity + DCCI + AgeGr + Sex


# Remarks

In Cox analysis input files, records with covariates given which there are no events, are excluded (without change of results)
