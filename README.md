# SubnationalBrazil-Fiscal-Assymmetry-Neural-IRF

**Official repository for the paper:** "Investigating fiscal crisis transmission in Brazilian subnational governments (2000-2023) via System GMM, Hierarchical BVAR, and Neural IRF to model spending rigidity and asymmetries."

## Overview
This project investigates how fiscal crises are transmitted within Brazilian states. It specifically focuses on the "insolvency trap" caused by the rigidity of current expenditures and the sacrifice of public investment during economic downturns.

## Repository Structure & Scripts

**1. Data Preparation:** Automated retrieval of subnational fiscal data via STN API, including HP filter for output gap estimation, subnational terms of trade, and construction of per capita gross dummies.
**2. Pre-estimation Tests:** Unit root/stationarity tests, cointegration tests, and structural break analysis.
**3. System GMM:** Estimation of structural parameters using the Two-Step System GMM estimator to address endogeneity and fiscal inertia.
**4. Hierarchical BVAR:** Estimation of short and medium-term dynamics using Bayesian priors for subnational data.
**5. Neural IRF:** Application of Deep Learning to capture non-linearities and threshold effects in fiscal crisis transmission.
**6. Simulation IRF:** Uses System GMM parameters to simulate response asymmetries and construct IRFs.
**7. Graphics:** data visualizations, and fiscal stress maps.


## Methodology
The empirical strategy follows a three-step triangulation:
* **System GMM:** Identification of structural parameters and baseline elasticities.
* **Hierarchical BVAR:** Modeling of adjustment dynamics and medium-term trajectories.
* **Neural IRF:** Application of Deep Learning (ReLU activation) to capture non-linearities and fiscal asymmetries.
* **Simulation IRF:** Uses parameters estimated via System GMM to simulate response asymmetries and construct IRFs.


## Key Features
* **Threshold Detection:** Using Neural Networks to simulate institutional fiscal triggers.
* **Asymmetry Analysis:** Comparing positive vs. negative output gap shocks.
* **Subnational Focus:** Comprehensive data from all 27 Brazilian states (2000-2023).

> **Note:** This is a partial code for a **Working Paper** intended for submission to the journal. Please do not cite without permission. For inquiries, contact the author.
