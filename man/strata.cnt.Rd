\name{strata.cnt}
\alias{strata.cnt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count trios for each of 15 strata with mother-father-child genotypes
}
\description{
Count trios with mother-father-child genotypes as 222,212,122,211,121,201,021,112,111,110,101,100,011,010,000
}
\usage{
strata.cnt(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{trios data with first column as disease status (1=case, 0=control),second column as mother genoypte, third father, fourth child.
%%     ~~Describe \code{case} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{cnt.mfc}{count of stratum with genotypes mfc}
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

\seealso{
\code{\link{rtrios}}
\code{\link{ll}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

trios=rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
cnt=strata.cnt(trios$case)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
