
BIRTHS = ../data/gl_report_births.rda

NONBIRTHS = ../data/gl_report_popn.rda \
            ../data/gl_report_deaths.rda \
            ../data/gl_report_immig.rda \
            ../data/gl_report_emig.rda

SYSMOD = ../data/gl_sysmod_mean_births.rda \
         ../data/gl_sysmod_mean_deaths.rda \
         ../data/gl_sysmod_mean_immig.rda \
         ../data/gl_sysmod_mean_emig.rda

DATAMOD = ../data/gl_cover_ratio_popn.rda \
          ../data/gl_cover_sd_popn.rda \
          ../data/gl_cover_ratio_immig.rda \
          ../data/gl_cover_sd_immig.rda \
          ../data/gl_cover_ratio_emig.rda \
          ../data/gl_cover_sd_emig.rda

.PHONY: all
all: ../data/results_greenland.rda

$(BIRTHS) : births.R
	Rscript $<

$(NONBIRTHS) &: nonbirths.R
	Rscript $<

$(SYSMOD) &: sysmod.R $(BIRTHS) $(NONBIRTHS)
	Rscript $<

$(DATAMOD) &: datamod.R $(NONBIRTHS)
	Rscript $<

../data/results_greenland.rda: results_greenland.R $(SYSMOD) $(DATAMOD)
	Rscript $<


## Clean

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
