\name{prev}
\alias{prev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate prevalence of disease
}
\description{
Use 3 penetrance factors for genotype 0, 1 and 2 (additive model), tranmission ratio of minor allele from both parents, and minor allele frequency (MAF) to calculate the disease prevalence.
}
\usage{
prev(f0, f1, f2, rmother, rfather, maf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f0}{
penetrance for individuals with genotype 0 (additive model)
}
  \item{f1}{
penetrance for individuals with genotype 1 (additive model)
}
  \item{f2}{
penetrance for individuals with genotype 2 (additive model)
}
  \item{rmother}{
transmission ratio of minor allele from mother to child
}
  \item{rfather}{
transmission ratio of minor allele from father to child
}
  \item{maf}{
minor allele frequency
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{d}{disease prevalence}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Lam Opal Huang
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
d=prev(0.1,0.2,0.3,0.5,0.5,0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
