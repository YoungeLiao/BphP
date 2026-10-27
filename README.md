# BphP: Application of ModuleMin and Reproducible Figure Generation
This repository accompanies <u>our study</u> investigating infrared-responsive microbial communities and bacteriophytochrome-like sensory systems. It provides a case study demonstrating the application of [ModuleMin](https://github.com/YoungeLiao/ModuleMin), together with the scripts and associated data used to generate the figures presented in the manuscript.

Rather than duplicating the general-purpose light-responsive genetic element mining workflow implemented in ModuleMin, this repository focuses on the application of that workflow to the study and on the reproducibility of the manuscript's computational analyses and visualizations.

## Repository overview

The repository serves two complementary purposes:

- **Case study of ModuleMin:** Demonstrates the application of ModuleMin to the identification and analysis of bacteriophytochrome (BphP)-related candidates in the context of infrared-responsive microbial communities. The underlying general-purpose mining workflow is documented in the [ModuleMin repository](https://github.com/YoungeLiao/ModuleMin).
- **Reproducible figure generation:** Provides the scripts and associated data used to generate the figures in the accompanying manuscript, supporting transparent inspection and reproduction of the reported computational results and visualizations.

## Repository structure

```
BphP/
├── scr/          # Analysis scripts
├── Rscript/      # Figure generation, statistical analysis, and 
├── data/         # Data used in analyses and figure generation
├── README.md
└── LICENSE
```

Please refer to the individual scripts and their comments for details on specific analyses, input requirements, and output files.

## Reproducing the manuscript figures

The scripts and data in this repository support the generation of manuscript figures. To reproduce a figure:

1. Clone or download this repository.
2. Identify the relevant script in `scr/` or `Rscript/`.
3. Locate the corresponding input data in `data/` or the data paths specified by the script.
4. Check the script for software dependencies, input formats, and file paths, and update local paths where necessary.
5. Run the script in an environment containing the required dependencies.

Because different figures may rely on different analytical steps and software packages, consult the corresponding scripts for figure-specific instructions. Software versions and parameters should be recorded when reproducing the analyses.

## Data and code availability

The scripts and associated data provided here support the computational analyses and visualization of the accompanying manuscript. Data files should be interpreted in conjunction with their corresponding scripts and the relevant descriptions in the manuscript and Supplementary Information.

Where the analyses use externally sourced sequence data or public reference databases, the relevant sources and accession identifiers should be reported in the manuscript or associated data documentation.

This repository complements, rather than replaces, the data availability information and any dedicated public data repositories cited in the manuscript.

## Relationship to ModuleMin

[ModuleMin](https://github.com/YoungeLiao/ModuleMin) provides the broader computational framework for mining and analyzing light-responsive genetic elements. BphP illustrates an application of this framework to a specific research question and collects the associated analytical and visualization resources used in the accompanying study.

For details of the general-purpose mining workflow, please refer to the ModuleMin repository.

## Citation

If you use this repository, please cite the associated publication once its bibliographic information becomes available.

**Manuscript:** *Infrared sensing enhances inorganic carbon assimilation and retention in saline microbiomes*.

**Code repository:** Liao, Y. *BphP: Application of ModuleMin and Reproducible Figure Generation*. GitHub. https://github.com/YoungeLiao/BphP

For reproducibility, please cite the specific repository release or archived version used in your analysis, when available.

## License

This repository is distributed under the MIT License. See [LICENSE](https://chatgpt.com/c/LICENSE) for details.

## Contact

For questions about the code or its application, please open an issue in this repository.
