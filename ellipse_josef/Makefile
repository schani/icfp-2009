# $Id$

DISTNAME = 
BINFILES = $(DISTNAME)
SBINFILES =
LIBFILES = ellipsen 
LIBCOMPS-ellipsen =  hohmann  bielliptic newtime correction ellipse
DOCFILES = COPYING LICENSE CHANGELOG VERSION 
OTHERFILES = $(DISTNAME).spec
SRCFILES = $(shell echo *.ml *.mli *.mll *.mly) Makefile
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES =
MODULES = hohmann bielliptic newtime correction ellipse 
LIBRARIES =
BINARIES = $(BINFILES) $(SBINFILES)
DISTVERSION = $(shell cat VERSION)

LIBDIRS =
BUILDSUBDIRS = 
REBUILDSUBDIRS = 

INSTALL_PREFIX = /

include Makefile.ocaml
