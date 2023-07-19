# Gavi Vaccination Impacts: A Propensity Score Study

Final Project from an Advanced Causal Inference Class. Coded in R

short description: A causal analysis of the Gavi Vaccination Program using propensity score matching.  Former causal analysis from a 2019 paper used regression discontinuity design, so it could only focus on countries near the graduation threshold. Propensity score matching should allow for more analysis


## Summary

In 2000, a pooling of public and private donors created a partnership called the Global Alliance for Vaccines and Immunisation (aka the GaviAlliance), whose mission was to increase immunization rates in poor countries. Gavi began to support low-income countries by providing them with bulk purchases of vaccines for diphtheria, pertussis and tetanus (DPT), hepatitis B, pneumococcal disease, rotavirus, and Haemophilus influenza type B. The alliance used a strict cut-off to determine eligibility of aid, which researchers from the Center for Global Development exploited to create a causal estimate using a regression discontinuity design. They found Gavi’s provision of vaccines to have little or no impact on vaccination rates for hepatitis B and DPT or mortality rates. There is some evidence of positive impact for the other, newer and more expensive, vaccines, but they are small and statistically insignificant. However, the regression discontinuity design only produces a local average treatment effect, which cannot be generalized to countries far from the $1000 GNI cutoff. The following projects attempts to produce more relevant estimates to the population of interest – poor countries, which on average have low vaccination rates. Due to reproducibility issues, only the effect of Gavi on mortality rates were examined. A doubly-robust propensity score design is used. The sample size was very small, and balance was not achieved in the matching step – although this was compensated for with the doubly robust design. Still, consistent near-zero and very uncertain results were found, with little power.

## Research Design

The original regression discontinuity design only includes countries that were close to the $1000 GNI per capita threshold. Therefore, the population of the study does not align with the countries they are most concerned about. The true population of interest are lower income countries, that have more need and less resources for vaccination, that have also received Gavi aid. The purpose of the following propensity score study design is to create a causal estimate of the impact of the Gavi program on mortality rates on these participating countries, which will include those further from this $1000 GNI per capita cut-off. Like the authors of the above study, I think that it’s worth investigating whether the impact of the Gavi program will be pronounced further from the threshold. Unfortunately, the dose-response model from the original study could not be replicated with the materials that the authors provided. Only outcomes of mortality remained useable, and only the first phase of Gavi rollout is examined, in comparison to 5 years of data before the implementation of the program. The research question is as follows: does participation in the Gavi Vaccination program have any effect on mortality rates?

## The Data

Data is constructed from five sources, covering the years 2003-2013. They are the WHO, UNICEF, World Bank, Gavi and IHME.  All data are available annually except for mortality rates. Variables include the income eligibility for the study, whether or not they received treatment, immunization coverage, income classification (a World Bank income group classification), population, Gavi-purchased doses, vaccine wastage, mortality rate in five year increments, disease-specific mortality in 1990/2005/2010, health system financing, and non-income eligibility – which was created using vaccination coverage rates. The eligibility is established by a threshold of $1000 Gross National Income. Countries below this GNI are eligible to receive aid from Gavi, creating a natural regression discontinuity design.  However, the population of inference is not the population of interest, as borderline countries should have better mortality outcomes than those with lower GNIs, as illustrated in Figure 1. 



### Citations

Sarah Dykstra, Amanda Glassman, Charles Kenny, Justin Sandefur,
Regression discontinuity analysis of Gavi's impact on vaccination rates,
Journal of Development Economics,
Volume 140,
2019,
Pages 12-25,
ISSN 0304-3878,
https://doi.org/10.1016/j.jdeveco.2019.04.005.
(http://www.sciencedirect.com/science/article/pii/S0304387819305309)
Abstract: Since 2001, an aid consortium known as Gavi has accounted for over half of vaccines purchased in the 75 eligible countries with an initial GNI below $1,000 per capita. Regression discontinuity estimates suggest most aid for cheap, existing vaccines like hepatitis B and DPT was inframarginal: for instance, hepatitis B doses sufficient to vaccinate roughly 75% of infants raised vaccination rates by single-digit margins. These results are driven by middle-income countries near the eligibility threshold, and do not preclude larger gains for the poorest countries, global externalities via vaccine markets, or impacts on newer vaccines such as pneumococcal or rotavirus for which income eligibility rules were relaxed.
Keywords: Aid; Vaccination; Immunization; Fungibility; Regression discontinuity

