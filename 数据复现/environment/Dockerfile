# hash:sha256:9b8f32a3b6c83c9011c7e29f7dd89f90a6a15a5e60be39a0deb54cba1e13c25b
FROM registry.codeocean.com/codeocean/py-r:python3.10.12-R4.3.2-JupyterLab4.0.10-RStudiorstudio-server-2023.12.0-369-ubuntu22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN conda install -y \
        matplotlib==3.8.2 \
        numpy==1.26.4 \
    && conda clean -ya

RUN Rscript -e 'remotes::install_version("UpSetR", "1.4.0")' \
    && Rscript -e 'remotes::install_version("ade4", "1.7-22")' \
    && Rscript -e 'remotes::install_version("cowplot", "1.1.3")' \
    && Rscript -e 'remotes::install_version("data.table", "1.14.8")' \
    && Rscript -e 'remotes::install_version("dplyr", "1.1.4")' \
    && Rscript -e 'remotes::install_version("future.apply", "1.11.2")' \
    && Rscript -e 'remotes::install_version("ggplot2", "3.5.2")' \
    && Rscript -e 'remotes::install_version("ggpmisc", "0.6.0")' \
    && Rscript -e 'remotes::install_version("ggpointdensity", "0.1.0")' \
    && Rscript -e 'remotes::install_version("ggpubr", "0.6.0")' \
    && Rscript -e 'remotes::install_version("lattice", "0.22-6")' \
    && Rscript -e 'remotes::install_version("lessR", "4.3.9")' \
    && Rscript -e 'remotes::install_version("magrittr", "2.0.3")' \
    && Rscript -e 'remotes::install_version("patchwork", "1.2.0")' \
    && Rscript -e 'remotes::install_version("pbapply", "1.7-2")' \
    && Rscript -e 'remotes::install_version("permute", "0.9-7")' \
    && Rscript -e 'remotes::install_version("readxl", "1.4.3")' \
    && Rscript -e 'remotes::install_version("scales", "1.4.0")' \
    && Rscript -e 'remotes::install_version("treemapify", "2.5.6")' \
    && Rscript -e 'remotes::install_version("vegan", "2.6-6.1")'
