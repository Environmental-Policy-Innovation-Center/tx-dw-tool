![](epic-logo-transparent.png)

# Texas Community Water System Prioritization Tool

This repository contains the code for the [Texas Community Water System Prioritization Tool](https://tx-app.policyinnovation.info/). This application was developed to assist in prioritizing advocacy and technical assistance for community water systems in Texas. Known generally as a screening tool, the data and insights generated from this tool are to be taken in conjunction with research and local knowledge to inform outreach and not a sole source of information. This tool can be used to identify utilities based on a user determined set of characteristics. Keep in mind, these data are a small component of utility operations and drinking water user experience. Generally speaking, utilities are working to balance quality water, low rates, and financial stability, all while staying within regulatory compliance. This balancing act can be difficult for under-resourced utilities

# To Run This Application 
## Rstudio
#### [Install CRAN](https://cran.r-project.org/)
#### [Install R Studio](https://www.rstudio.com/products/rstudio/download/)
Open tx-dw-tool.app.R and click 'Run App'
## Shinyapps.io 
To publish your own application, use the whirlpool icon in Rstudio to publish via [Shinyapps.io](https://www.shinyapps.io/). 
You can see a step by step process of this [here](https://www.r-bloggers.com/2021/05/push-button-publishing-for-shiny-apps/).
## Docker (for Mac)
Download and install [Docker](https://www.docker.com/).
Open a terminal
cd to the working directory
`docker build --tag [tx-app] .`
Grab a cup off coffee while it compiles.
Type `docker images` to see built containers and verify your [tx-app] built succesfully. 
Type `docker run -p 2000:2000 [tx-app]` to run the application. 

# Important links 
The repository for the data collection and analysis that feeds into this tool is available [here](https://github.com/Environmental-Policy-Innovation-Center/TX-drinking-water). For important context and descriptions of data used within this repository, please review our [data dictionary](https://docs.google.com/spreadsheets/d/1bzNPxhL-l6DeGElhG1c70Of8DGAQasMDUuX3rPHVe2A/edit#gid=0) and [methodology](https://docs.google.com/document/d/1va2Iq2oJxnqiwgNHD4bWpXKxdWbq-TYoYkosj1oz_JU/edit).

# Information and Feedback:
-   Got feedback? Take our [survey](https://forms.gle/Xjbeur68qukaRmFo7) and visit our [public log](https://docs.google.com/document/d/1MvfLFHDhTKoyLuk-cEPwFj8LPZTtdzPLBrkbhbuU38Y/edit)!
-   To learn more about how to use this tool, visit our vignettes [coming soon!].

# Attribution and License:
Developed in partnership with [Cynthia & George Mitchell](https://cgmf.org/p/home.html) and [T.L.L Temple](https://tlltemple.foundation/) foundations by [Environmental Policy Innovation Center (EPIC)](https://www.policyinnovation.org/). EPIC makes no assurances to the accuracy of the data. All underlying code, methods, and data are available under a Creative Commons License.
