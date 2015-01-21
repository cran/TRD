\name{rtrios}
\alias{rtrios}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate simulated popoulation of case- and control-trios
}
\description{
Using specified penetrance factors, tranmission ratio of minor allele and minor allele frequency (MAF) to generate a simulated population of case- and contrl-trios.
}
\usage{
rtrios(n, ssize, f0, f1, f2, rmother, rfather, maf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
simulated population size
}
  \item{ssize}{
sample size
}
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
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{case}{case-trios}
\item{control}{control-trios}
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
\code{\link{ll}}
\code{\link{strata.cnt}}
\code{\link{gcount}}
\code{\link{tdt.cnt}}
\code{\link{tdt}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

trios = rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
