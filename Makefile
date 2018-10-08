EXTENSION = setrank
PGFILEDESC = "tf*idf ranking"

MODULE_big = setrank
DATA = setrank--1.0.sql
OBJS = idf.o setrank.o
REGRESS = setrank

ifdef USE_PGXS
PGXS := $(shell pg_config --pgxs)
include $(PGXS)
else
subdir = contrib/setrank
top_builddir = ../..
include $(top_builddir)/src/Makefile.global
include $(top_srcdir)/contrib/contrib-global.mk
endif


