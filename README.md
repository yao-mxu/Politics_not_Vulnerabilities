## Politics_not_Vulnerabilities
### Hosting R code used for previous versions of R
### September 16, 2022

If you are using an older version of R and the OSF deposited code does not compile figures in ``Politics, not Vulnerability: Republicans Discriminated Against Chinese-born Americans Throughout the COVID-19 Pandemic'' correctly, please try the code deposited here in this repository.

#### Issue 1: Stargazer output error

Solution: Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2

Copyright: Alex Knorre, https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53


#### Issue 2: ggplot legend color and shape mismatch

Solution: create master legend separately and then annoate the figures
