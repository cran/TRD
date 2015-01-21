\name{tdt}
\alias{tdt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute TDT and augmented TDT on trios
}
\description{
Compute TDT on case- and control-trios, and compute augmented TDT on case-trios using transmission ratio of minor allele from control-trios.
}
\usage{
tdt(sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
case- and control-trios dataset with first column as disease status (1=case, 0=control), second column as maternal genotype, third paternal genotype, and fourth child genotype.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{

\item{case statistics }{TDT statistics for case-trios
}
\item{control statistics }{TDT statistics for control-trios
}
\item{Adjusted-case statistics }{augmented TDT statistics for case-trios
}
\item{case p-value }{TDT p-value for case-trios
}
\item{control p-value }{TDT p-value for control-trios
}
\item{Adjusted-case p-value }{augmented TDT p-value for case-trios
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Labbe A, Huang LO, Infante-Rivard C (2013). Transmission Ratio Distortion: A Neglected Phenomenon with Many Consequences in Genetic Analysis and Population Genetics:Naumova AK and Greenwood CMT (eds.), Epigenetics and Complex Traits: 265-85, Springer.

Spielman RS, McGinnis RE, Ewens WJ (1993) Transmission test for linkage disequilibrium: the insulin gene region and insulin-dependent diabetes mellitus (IDDM). American Journal of Human Genetics 52: 506. 

}
\author{
Lam Opal Huang}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rtrios}}
\code{\link{tdt.cnt}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
trios=rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
tdt(rbind(trios[[1]],trios[[2]]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
