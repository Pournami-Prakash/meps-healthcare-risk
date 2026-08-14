# Variable Dictionary

This project uses MEPS HC-243, the 2022 Full Year Consolidated Data File.

The prospective workflow also uses MEPS HC-245, the Panel 24 Four-Year Longitudinal Data File, to predict 2022 outcomes from 2021 features.

## Survey Design

- `PERWT22F`: person-level full-year weight
- `VARSTR`: variance estimation stratum
- `VARPSU`: variance estimation primary sampling unit

## Outcomes

- `TOTEXP22`: total health care expenditure in 2022
- `high_cost`: derived flag for people at or above the weighted 90th percentile of `TOTEXP22`
- `DLAYCA42`: delayed medical care due to cost, Round 4/2
- `DLAYDN42`: delayed dental care due to cost, Round 4/2
- `DLAYPM42`: delayed prescription medicine due to cost, Round 4/2
- `PYUNBL42`: unable to pay family medical bills
- `cost_related_barrier`: derived flag equal to 1 if any of the cost-delay or bill-payment variables are yes

For yes/no access variables, MEPS uses:

- `1`: yes
- `2`: no
- negative values: missing, refused, not ascertained, inapplicable, or don't know

## Demographic and Socioeconomic Features

- `AGE22X`: age
- `SEX`: sex
- `RACETHX`: edited/imputed race and ethnicity
- `EDUCYR`: years of education
- `REGION22`: census region
- `POVCAT22`: poverty category
- `INSURC22`: full-year insurance coverage status
- `UNINS22`: uninsured all year

## Health and Utilization Features

- `ADGENH42`: self-reported general health
- `HIBPDX`: hypertension diagnosis flag
- `CHDDX`: coronary heart disease diagnosis flag
- `MIDX`: heart attack diagnosis flag
- `EMPHDX`: emphysema diagnosis flag
- `CANCERDX`: cancer diagnosis flag
- `ARTHDX`: arthritis diagnosis flag
- `ASTHDX`: asthma diagnosis flag
- `OBTOTV22`: office-based provider visits
- `OPTOTV22`: outpatient visits
- `ERTOT22`: emergency room visits
- `IPDIS22`: inpatient discharges
- `DVTOT22`: dental visits
- `HHTOTD22`: home health days

## Expenditure Fields Retained for Reporting

These are not used as predictors for the high-cost model because they can leak the outcome.

- `TOTSLF22`: total self/family payments
- `RXEXP22`: total prescription medicine expenditure
- `RXSLF22`: prescription medicine self/family payments

## Prospective Longitudinal Fields

The prospective model uses Year 3 variables as prior-year predictors and Year 4 variables as next-year outcomes.

Prior-year predictors:

- `AGEY3X`: age as of 12/31/2021
- `POVCATY3`: 2021 poverty category
- `INSURCY3`: 2021 full-year insurance coverage status
- `TOTEXPY3`: 2021 total health care expenditure
- `RXEXPY3`: 2021 prescription medicine expenditure
- `RXSLFY3`: 2021 prescription medicine self/family payments
- `OBTOTVY3`, `OPTOTVY3`, `ERTOTY3`, `IPDISY3`: 2021 utilization measures
- `DLAYCA6`, `DLAYDN6`, `DLAYPM6`, `PYUNBL6`: prior cost-related access or affordability barriers

Next-year outcomes:

- `TOTEXPY4`: 2022 total health care expenditure
- `RXSLFY4`: 2022 prescription medicine self/family payments
- `high_cost_next`: derived flag for top-decile 2022 total expenditure
- `high_rx_oop_next`: derived flag for top-decile 2022 prescription out-of-pocket burden

Longitudinal design:

- `LONGWT`: longitudinal person weight
- `VARSTR`: variance estimation stratum
- `VARPSU`: variance estimation primary sampling unit

## Prescribed Medicines Event Fields

The deeper pharmacy workflow uses the 2021 and 2022 Prescribed Medicines event files:

- HC-229A: 2021 Prescribed Medicines event file
- HC-239A: 2022 Prescribed Medicines event file

Important event-level fields:

- `DUPERSID`: person identifier for linking event records to the longitudinal person file
- `RXNDC`: National Drug Code
- `RXDRGNAM`: drug name
- `RXDAYSUP`: days supplied
- `TC1`, `TC1S1`, `TC1S1_1`, and related fields: therapeutic class fields
- `RXXP21X`, `RXXP22X`: total prescription payment in the event file
- `RXSF21X`, `RXSF22X`: self/family prescription payment in the event file

Derived person-level Rx event features:

- `rx_event_count_prior`
- `rx_unique_drug_count_prior`
- `rx_unique_therapeutic_classes_prior`
- `rx_total_days_supply_prior`
- `rx_total_paid_event_prior`
- `rx_oop_paid_event_prior`
- `rx_oop_share_event_prior`
- `rx_max_single_oop_event_prior`
- `high_rx_oop_event_next`
- `high_rx_complexity_event_next`
