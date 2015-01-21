\name{findr}
\alias{findr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find transmission ratio of minor alleles.
}
\description{
Use the input trios data to calculate the transmission ratio of the minor allele from parents to offspring for both case- and control-trios population.
}
\usage{
findr(sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
  A sample of case- and control-trios with first column as the disease status (1=case, 0=control), second column as mother genotype, third father genotype and fourth child genotype.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{r1}{  transmission ratio of minor allele in case-trios}
\item{r2}{  transmission ratio of minor allele in control-trios}
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

\seealso{
\code{\link{rtrios}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
trios=rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
findr(rbind(trios[[1]],trios[[2]]))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ findratio }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
