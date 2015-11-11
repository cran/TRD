\name{tdt}
\alias{tdt}
\title{
Compute TDT and adjusted TDT on case- and/or control-trios
}
\description{
Compute TDT on case- and control-trios, and adjusted TDT on case-trios using transmission ratio of minor allele calculated from control-trios [Labbe et al. 2013].  When control-trios are not supplied in the input data, only TDT on case-trios is computed. Similarly, when case-trios are not supplied, only TDT on control-trios is computed. The adjusted TDT uses both case- and control-trios, and takes into account of TRD. Note that trios with missing data are discarded.
}
\usage{
tdt(sample)
}
\arguments{
  \item{sample}{
case- and/or control-trios dataset with first column as disease status (1=case, 0=control), the second, third and forth columns as maternal, paternal and child genotypes. The fifth column, if entered as part of the input data, is the parent-of-origin indicator of disease allele in a heterozygous child (a value of 1 indicates the mother transmitted the disease allele and 0 indicates the father transmitted). The parent-of-origin indicator is only meaningful for a heterozygous child.
}
}
\value{

\item{case statistics }{TDT statistics for case-trios
}
\item{control statistics }{TDT statistics for control-trios
}
\item{Adjusted-case statistics }{Adjusted TDT statistics for case-trios using transmission ratio of minor allele in control-trios
}
\item{case p-value }{TDT p-value for case-trios
}
\item{control p-value }{TDT p-value for control-trios
}
\item{Adjusted-case p-value }{Adjusted TDT p-value for case-trios using transmission ratio of minor allele in control-trios
}}
\author{
Lam Opal Huang
}
\references{
Labbe A, Huang LO, Infante-Rivard C (2013). Transmission Ratio Distortion: A Neglected Phenomenon with Many Consequences in Genetic Analysis and Population Genetics:Naumova AK and Greenwood CMT (eds.), Epigenetics and Complex Traits: 265-85, Springer.

Spielman RS, McGinnis RE, Ewens WJ (1993) Transmission test for linkage disequilibrium: the insulin gene region and insulin-dependent diabetes mellitus (IDDM). American Journal of Human Genetics 52: 506.
}
\seealso{
\code{\link{rtrios}}
}
\examples{
trios=rtrios(100000,500,0.1,0.2,0.3,1,0.5,0.5,0.1)
tdt(trios$case)
tdt(trios$ctrl)
tdt(rbind(trios$case,trios$ctrl))
}
