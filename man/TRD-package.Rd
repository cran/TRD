\name{TRD-package}
\alias{TRD-package}
\alias{TRD}
\docType{package}
\title{
Transmission Ratio Distortion
}
\description{
Transmission Ratio Distortion (TRD) is a genetic phenomenon where two alleles from either parent are not transmitted to the offspring at the Mendelian 1:1 ratio. Occurrence of TRD in general population can lead to false inflation or attenuation of association signals in case populations. Therefore, it is necessary to adjust for TRD in model fitting of case populations. This package uses models such as loglinear model (Weinberg 1998), augmented loglinear model (Huang 2014), and tests such as TDT (Spielman 1993), augmented TDT (Labbe 2013), on simulated or real datasets. This package has a simulation function which generates a population that is under the influence of TRD, and samples a sub-population, which can serve as the dataset to fit a loglinear model or to perform a TDT. Real dataset with the same data structure as described in the help file of the function 'll' can also be used in the loglinear model and the TDT functions. No real dataset is included in this package.}
\details{
\tabular{ll}{
Package: \tab TRD\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-21\cr
License: \tab 3.1\cr
}
}
\author{
Lam Opal Huang

Maintainer: Lam Opal Huang <opal.huang@mail.mcgill.ca>
}
\references{
Huang LO, Labbe A, Infante-Rivard C (2014). Adjustment for Transmission Ratio Distortion in the analysis of case-parent trios using a loglinear model.(Working manuscript)

Labbe A, Huang LO, Infante-Rivard C (2013). Transmission Ratio Distortion: A Neglected Phenomenon with Many Consequences in Genetic Analysis and Population Genetics:Naumova AK and Greenwood CMT (eds.), Epigenetics and Complex Traits: 265-85, Springer.

Huang LO, Labbe A, Infante-Rivard C (2012). Transmission ratio distortion: review of concept and implications for genetic association studies. Human Genetics 132: 245-63.

Weinberg CR, Wilcox AJ, Lie RT (1998) A log-linear approach to case-parent-triad data: assessing effects of disease genes that act either directly or through maternal effects and that may be subject to parental imprinting. American Journal of Human Genetics 62: 969-78. 

Spielman RS, McGinnis RE, Ewens WJ (1993) Transmission test for linkage disequilibrium: the insulin gene region and insulin-dependent diabetes mellitus (IDDM). American Journal of Human Genetics 52: 506. 

}