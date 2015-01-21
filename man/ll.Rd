\name{ll}
\alias{ll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Loglinear model with and without adjustment for TRD.
}
\description{
Use input data, either sample of cases ('sample') or count data for 15-MFC-genotype categories ('counts') to fit loglinear model with (Model 2) and without adjustment (Model 1) for TRD, model type could be genotype relative risk ('grr'), dominant ('dom') or gene-dosage ('gd').
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ll(data, data.type, r, model.type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
input data to loglinear model, can be a sample of trios data with first column as disease status (1=case, 0=control),second column as mother genoypte, third father, fourth child, or a count dataset for the 15-MFC-genotype categories.
}
  \item{data.type}{
type of data: 'sample' or 'counts'
}
  \item{r}{
transmission ratio of minor alleles
}
  \item{model.type}{
'grr', 'dom' or 'gd'
}
}
%\details{
%}
\value{
\item{sumRaw}{Raw summary statistics of loglinear model}
\item{sumFormat}{Formatted summary statistics of loglinear model}}
\references{
Huang LO, Labbe A, Infante-Rivard C (2014). Adjustment for Transmission Ratio Distortion in the analysis of case-parent trios using a loglinear model.(Working manuscript)

Weinberg CR, Wilcox AJ, Lie RT (1998) A log-linear approach to case-parent-triad data: assessing effects of disease genes that act either directly or through maternal effects and that may be subject to parental imprinting. American Journal of Human Genetics 62: 969-78. 
}
\author{
Lam Opal Huang
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rtrios}}
\code{\link{strata.cnt}}
}
\examples{
trios=rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
out=ll(trios$case,'sample',0.5,'grr')

cnt=strata.cnt(trios$case)
out=ll(cnt,'counts',0.5,'grr')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
