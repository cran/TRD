\name{tdt.cnt}
\alias{tdt.cnt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count transmission of allele in McNemar Table 
}
\description{
Gives the diagnoal counts of McNemar Table for heterozygous transmission.
}
\usage{
tdt.cnt(sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
  case- or control-trios dataset with first column as disease status (1=case, 0=control), second column as maternal genotype, third paternal genotype, and fourth child genotype.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{b}{transmission count of minor over major allele in heterozygous parents}
\item{c}{transmission count of major over minor allele in heterozygous parents}
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Spielman RS, McGinnis RE, Ewens WJ (1993) Transmission test for linkage disequilibrium: the insulin gene region and insulin-dependent diabetes mellitus (IDDM). American Journal of Human Genetics 52: 506. 
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
\code{\link{tdt}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
trios=rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
case.cnt=tdt.cnt(trios$case)
ctrl.cnt=tdt.cnt(trios$control)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
