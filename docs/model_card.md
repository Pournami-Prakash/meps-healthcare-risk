# Model Card

## Intended Use

This analysis uses public MEPS data. The model is intended to support:

- Care management prioritization
- Benefit navigation
- Affordability and access support
- Customer engagement segmentation
- Descriptive health-services research

The model is not intended for:

- Denying coverage
- Raising premiums or prices
- Reducing access to care
- Making individual clinical decisions
- Replacing professional judgment

## Current Model Type

The primary model is now prospective: it uses 2021 features from MEPS Panel 24 longitudinal data to predict 2022 outcomes for the same people.

Primary prospective targets:

- Next-year high medical expenditure, defined as 2022 total health care expenditure at or above the weighted 90th percentile
- Next-year high prescription out-of-pocket burden, defined from the 2022 Prescribed Medicines event file as prescription self/family payment at or above the weighted 90th percentile

The project includes:

- Survey-weighted descriptive and regression analysis
- Logistic, random forest, and gradient boosting classification models
- Calibration, lift, threshold, subgroup, and action-segment analysis
- Log-scale annual expenditure regression as a secondary modeling task

## Temporal Leakage Handling

The original same-year workflow used 2022 utilization variables to predict 2022 high expenditure. That was useful as a same-year risk stratification baseline but carried temporal leakage concerns.

The upgraded prospective workflow addresses this by using prior-year features from 2021 to predict 2022 outcomes. Prior-year utilization, spending, diagnoses, insurance status, cost barriers, and demographics are used as predictors; 2022 expenditure and prescription out-of-pocket burden are held out as future outcomes.

This structure is closer to payer risk adjustment and outreach modeling, where past claims and member attributes are used to predict future cost or pharmacy burden.

Remaining caveat:

- MEPS is survey data, not production claims data.
- The prospective sample is smaller than the cross-sectional full-year file.
- The Rx target uses prescribed-medicines event-file aggregates, including fill count, unique drugs, therapeutic-class count, days supplied, total Rx spend, out-of-pocket Rx spend, and maximum single-fill out-of-pocket cost. It does not yet model individual drug names as high-dimensional features.

## Fairness and Reliability Checks

The workflow exports subgroup metrics by:

- Income category
- Race or ethnicity
- Insurance status
- Age group
- Chronic condition burden

The subgroup audit includes observed rates, average predicted risk, calibration error, AUC, recall, and precision.

The prospective workflow also exports an illustrative mitigation table that compares global-threshold recall with group-specific thresholds for income and race or ethnicity. This should be treated as a demonstration of mitigation thinking, not a production fairness policy.

## Threshold Strategy

The project evaluates thresholds as operational choices rather than treating 0.50 as a default. Lower thresholds may be appropriate for broad outreach, while higher thresholds may be appropriate when intervention capacity is limited.

A simple cost-sensitive table uses placeholder assumptions:

- Medical-cost outreach cost per targeted person: `$50`
- Medical-cost value per captured true positive: `$1,000`
- Pharmacy navigation cost per targeted person: `$25`
- Pharmacy value per captured true positive: `$250`

These values are illustrative only and should be replaced by business-validated assumptions in a real deployment.

## Recommended Action Segments

The dashboard exports translate risk scores into operationally meaningful segments:

- High cost risk plus cost/access barrier: affordability navigation plus care management outreach
- High cost risk without reported barrier: care management outreach
- Lower risk plus cost/access barrier: benefit education and access support
- Lower risk without reported barrier: monitor

## Data Sources

- MEPS HC-243: 2022 Full Year Consolidated Data File
- MEPS HC-245: Panel 24 Four-Year Longitudinal Data File
- MEPS HC-229A: 2021 Prescribed Medicines event file
- MEPS HC-239A: 2022 Prescribed Medicines event file

Primary source page: https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp
