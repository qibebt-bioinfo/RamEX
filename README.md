# RamEx

![Version](https://img.shields.io/badge/Version-1.0-brightgreen)
![Release date](https://img.shields.io/badge/Release%20date-Jan.%2010%2C%202022-brightgreen)



## Contents

- [Introduction](#introduction)
- [System Requirement and dependency](#system-requirement-and-dependency)
- [Installation guide](#installation-guide)
- [Usage](#usage)
- [Example dataset](#example-dataset)


# Introduction

A ramanome represents a single-cell-resolution metabolic phenome that is information-rich, revealing functional heterogeneity among cells, and universally applicable to all cell types.Ramanome Explorer (RamEx, Fig.1) is a toolkit for comprehensive and efficient analysis and comparison of ramanomes. Results from the multidimensional analysis are visualized via intuitive graphics. Implemented via R and C++, RamEx is fully extendable and supports cross-platform use.By providing simple-to-use modules for computational tasks that otherwise would require substantial programming experience and algorithmic skills, RamEx should greatly facilitate the computational mining of ramanomes.
![Fig. 1. The software architecture of RamEx.](http://bioinfo.single-cell.cn/images/RamEx.png)  
<div align=center>Fig.1. The software architecture of RamEx. RamEx integrates five core modules (data governance and preprocessing, real-time ramanome depth analysis, microbiome heterogeneity index analysis, IRCA, and RBCS) to support a complete ramanome-analysis pipeline.</div>



# System Requirement and dependency

## Hardware Requirements

RamEx only requires a standard computer with >1GB RAM to support the operations defined by a user.

## Software Requirements

RamEx only requires a C++ compiler (e.g. g++) to build the source code. Most Linux releases have g++ already been installed in the system. In Mac OS, to install the compiler, we recommend to install the Xcode application from the App store.

# Installation guide

## Automatic Installation (recommended)

At present, RamEx provides a fully automatic installer for easy installation.

**a. Download the package**
```
git clone https://github.com/qibebt-bioinfo/RamEx.git	
```

**b. Install by installer**
```
cd RamEx
source install.sh
```

The package should take less than 1 minute to install on a computer with the specifications recommended above.

The example dataset could be found at “examples” folder. Check the “examples/Readme” for details about the demo run.

## Manual Installation

If the automatic installer fails, RamEx can still be installed manually.

**a. Download the package**
```
git clone https://github.com/qibebt-bioinfo/RamEx.git	
```

**b. Configure the environment variables (the default environment variable configuration file is “~/.bashrc”)**
```
export RamEX=Path to RamEx
export PATH="$PATH:$RamEX/bin/"
source ~/.bashrc
```
**c. Compile the source code**
```
cd RamEx
make
```
# Usage
The RamEx consists of three modules: QC（pretreatment）IRCA and RBCS. Currently the RamEx requires all CAST-R, Horiba and Renishaw files as txt object.

**I. QC（pretreatment）** 

Ramanome analysis starts with data management and preprocessing of SCRS
In the QC step, RamEx provides several functions: initialization, read data,  smooth, quanlity control, baseline, normalization and visual variance analysis, including PCA,t-SNE and PLS-DA. 

Currently the RamEx requires files as txt object. All spectrum data stored in few subfolders of the working path by group. 

***Initialization***

The initialization function makesure the working directory and creat the ouput directry.

***Read data***

For CAST-R and Horiba spectrum data, RamEx reads data from the subfolders of the working path by group. And for Renishaw mapping data, RamEx can read mapping data from the working directory and creat some subfolders by group. Then, RamEx can read them like CAST-R and Horiba spectrum data.

***Preprocessing(smooth; quanlity control; baseline; normalization)***

All data can be preprocessed by combining the provided functions as desired.

***Visual variance analysis(PCA,t-SNE and PLS-DA)***

Low-dimensional spatial visualization provides us with an intuitive understanding of ramanome. To run this module you can use the command "RamEx-QC" as below:

```
RamEx-QC ...
```



**II.  Intra-Ramanome Correlation Analysis (IRCA)**

To produce the IRCN from a ramanome data point, spectral range, spectral resolution, and spectral normalization are first derived or performed via the data preprocessing module. Then the Pearson correlation coefficients (PCC; ρ) of all possible pairwise combinations of Raman peak are calculated from a ramanome. Key network parameters are calculated by the “igraph” package in R to probe the global features of an IRCN. All Raman peaks are used for deriving the global IRCN, while only characteristic marker Raman peaks are used for the simplified versions of IRCN (Local IRCN) to facilitate visualization. 


Furthermore, RamEx derives three graphic signatures that depict and characterize the links among key SCRS features: metabolite profile (MP), metabolite interaction (MI) and metabolite conversion (MC). MP depicts the mean SCRS of a ramanome. MI, also a network derived via inherent variations of SCRS in a ramanome, is constructed based on the matrix of all pairwise Raman peaks with significant correlations (P < 0.05). In contrast, MC is a network consisting of only those pairwise Raman peaks with strongly negative correlation (ρ ≤–0.6, P < 0.05). To compare the networks, hierarchical cluster analysis (HCA) was performed with Ward’s algorithm. To run this module you can use the command "RamEx-IRCA-**" as below:

```
RamEx-IRCA-** ...
```


**III.  Raman Barcode of Cellular-response to Stress(RBCS)**

We have proposed RBCS, which represents the time- or condition-specific response of ramanome to stimuli. RBCS contains a series of Raman peaks that are more responsive to stimuli. The dynamic changes of these Raman signals constitute a specific identification code that can be used to quickly distinguish and identify specific cellular responses of each stimulus. Based on stoichiometric methods, the RBCS obtained from the analysis and comparison of Raman profiles under all stimuli cannot only distinguish the cellular responses of different stimuli, but also provide information on their cytotoxicity (i.e., cell mortality). To run this module you can use the command "RamEx-RBCS" as below: 

```
RamEx-RBCS ...
```

# Example dataset


Two investigated species of S. mutans (Sm) and S. sanguinis (Ss) were performed a complete transition of three typical phases in bacterial growth: lag (0–2 h), log (4–8 h), stationary (12–24 h), which was deemed worthwhile to be investigated via Raman microspectroscopy. We ensured that the majority of individual spectra were taken over single cells and preparation conditions were consistent between samples.



# Reference
1. Lee, K. S. et al.,  Raman microspectroscopy for microbiology. _Nature Reviews Methods Primers_, 2021.
2. Xu, J. et al., Emerging Trends for Microbiome Analysis: From Single-Cell Functional Imaging to Microbiome Big Data. _Engineering_, 2017.

