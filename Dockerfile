FROM opencpu/base
RUN R -e 'remotes::install_github("resplab/cfmortality")'
RUN R -e 'remotes::install_github("resplab/cfmortalityPrism")'
RUN echo "opencpu:opencpu" | chpasswd
