# Trait-Based Screening of Extinction Risk in Texas Bats

This project explores whether ecological traits can be used to determine extinction risk among bat species native to Texas.

## Data
- IUCN Red List (extinction risk labels)
- EltonTraits (body mass, diet breadth)
- 15 focal bat species native to Texas

## Methods
- Logistic regression
- Trait-based predictors
- Leave-One-Out Cross-Validation (LOOCV)
- Threshold sensitivity analysis (0.50 vs. 0.25)

## Results
The model demonstrates moderate ability to rank species by extinction risk, with higher utility as a screening tool rather than as a strict classifier. Lowering the threshold improves sensitivity at the cost of increased false positives.

## Key Files
- `analysis/logistic_model.R` – model fitting and validation
- `figures/loocv_risk_by_species_singlepanel.png` – results figure
- `results/results_summary.csv` – performance summary

## Notes
This project was completed as an independent, self-designed analysis to demonstrate end-to-end data  workflow and model evaluation with limited data.