\name{ll}
\alias{ll}
\title{
Fit Loglinear model with and without adjustment for TRD.
}
\description{
Use input data, either sample of case-trios ('sample') or count data for 15/16-MFC-genotype categories ('counts') to fit loglinear model with (Model 2) and without (Model 1) adjustment  for TRD, model type could be genotype relative risk ('grr'), dominant ('dom'), gene-dosage ('gd'), genotype relative risk with imprinting variable ('grri'), dominant with imprinting variable ('domi'), or gene-dosage with imprinting variable ('gdi'). Note that the models fit only complete trios with no missing data.
}
\usage{
ll(data, data.type, tmother, tfather, model.type)
}
\arguments{
  \item{data}{
input data can be
(1) case- and/or control-trios dataset with first column as disease status (1=case, 0=control), the second, third and forth columns as maternal, paternal and child genotypes. The fifth column, if entered as part of the input data, is the parent-of-origin indicator of disease allele in a heterozygous child (a value of 1 indicates the mother transmitted the disease allele and 0 indicates the father transmitted). The parent-of-origin indicator is only meaningful for a heterozygous child.
(2) a count dataset for the 15 or 16-MFC-genotype categories obtainable from the function strata.cnt(..), with the latter having 2 categories (111M & 111F) for triply heterozygous trios.
}
  \item{data.type}{
type of data: 'sample' or 'counts'
}
  \item{tmother}{
transmission ratio of minor alleles from mother to child
}
  \item{tfather}{
transmission ratio of minor alleles from father to child
}
  \item{model.type}{
'grr', 'dom', 'gd', 'grri', 'domi', or 'gdi'
}
}
\value{
\item{Summary table}{Summary statistics of fitting the loglinear model}}
\details{
If the following warning messages occur, increase the population and sample size.
Warning messages:
glm.fit: fitted rates numerically 0 occurred
}

\references{
Huang LO, Infante-Rivard C, Labbe A, (2015). Modeling sex-of-parent-specific Transmission Ratio Distortion and imprinting effect in loglinear model using case-trios.(Working manuscript)

Huang LO, Infante-Rivard C, Labbe A, (2015). Adjustment for Transmission Ratio Distortion in the analysis of case-parent trios using a loglinear model.(Working manuscript)

Weinberg CR, Wilcox AJ, Lie RT (1998) A log-linear approach to case-parent-triad data: assessing effects of disease genes that act either directly or through maternal effects and that may be subject to parental imprinting. American Journal of Human Genetics 62: 969-78.
}
\author{
Lam Opal Huang
}
\seealso{
\code{\link{rtrios}}
\code{\link{strata.cnt}}
}
\examples{
trios=rtrios(100000,500,0.1,0.2,0.3,1,0.5,0.5,0.1)
out=ll(trios$case,'sample',0.5,0.5,'grr')

cnt=strata.cnt(trios$case)
out=ll(cnt,'counts',0.5,0.5,'grr')

}
