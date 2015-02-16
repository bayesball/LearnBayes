\name{chemotherapy}
\alias{chemotherapy}
\docType{data}
\title{Chemotherapy treatment effects on ovarian cancer}
\description{
Edmunson et al (1979) studied the effect of different chemotherapy
treatments following surgical treatment of ovarian cancer.
}
\usage{
chemotherapy
}
\format{
  A data frame with 26 observations on the following 5 variables.
  \describe{
  \item{patient}{patient number}
  \item{time}{survival time in days following treatment}
  \item{status}{indicates if time is censored (0) or actually observed (1)}
  \item{treat}{control group (0) or treatment group (1)}
  \item{age}{age of the patient}
  }
}
\source{Edmonson, J., Felming, T., Decker, D., Malkasian, G., Jorgensen, E.,
Jefferies,
J.,Webb, M., and Kvols, L. (1979), Different chemotherapeutic sensitivities
and host factors affecting prognosis in advanced ovarian carcinoma versus
minimal residual disease, Cancer Treatment Reports, 63, 241-247.
}

\keyword{datasets}
