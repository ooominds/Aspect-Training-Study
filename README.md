# Analysis Code for Error-Correction Learning Study

This repository contains the code for the analyses reported in:

> Mackiewicz, J., Milin, P., & Divjak, D. (2025). Error-correction 
learning of second language verbal morphology: Associating imperfect 
contingencies in naturalistic frequency 
distributions. *Language Learning* (forthcoming).

## Scripts

- `01_data_prep_for_BayesABtest.R`: Prepares the data for comparisons
of total success rates between the two treatment groups, per training
day and grammaticality. The counts should be input manually in the JASP
interface.
- `02_posttest_analyses.R`: Analyses of post-test results by modelling 
individual responses (correct/incorrect) with Generalised Linear 
Mixed-Effects Models for binary outcomes, and by using Log-Linear 
Models to analyse multiway contingency tables for group-level 
tendencies.  
- `03_posttest_sdt.R`: Implements the Signal Detection Theory (SDT) 
framework to analyse participants’ response strategies.  
- `04_by_participant_SDT.R`: Applies the SDT framework at the 
individual level, providing a more fine-grained view of response 
strategies.  

## Data

The data are available from the University of Birmingham 
Institutional Research Archive (UBIRA): 
[https://edata.bham.ac.uk/1267/](https://edata.bham.ac.uk/1267/).  

---

For further inquiries, please contact: **ooominds@ooominds.org**  
