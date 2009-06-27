# $Id: Makefile,v 1.20 2007/09/14 12:21:47 heinz Exp $

DISTNAME = orbsim
BINFILES = orbsim
SBINFILES = 
LIBFILES =
DOCFILES = 
OTHERFILES =
SRCFILES = $(wildcard *.ml *.mli *.mll *.mly) Makefile Makefile.ocaml
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES = 
MODULES = 
LIBRARIES = bigarray lablgtk cairo cairo_lablgtk
DISTVERSION = 001
#  $(shell cat VERSION)

LIBDIRS = +cairo +lablgtk2
BUILDSUBDIRS = 

INSTALL_SUFFIX = .opt
INSTALL_PREFIX = /
INSTALL_PRERULE =

include Makefile.ocaml

installmetoo:
	install -D -m 755 compsxml2groups $(INSTALL_PREFIX)/bin/compsxml2groups
	install -D -m 755 ausinit $(INSTALL_PREFIX)/etc/rc.d/init.d/aus
	install -D -m 755 auscron $(INSTALL_PREFIX)/etc/cron.daily/aus
