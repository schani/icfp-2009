# $Id$

DISTNAME = sim 
BINFILES = $(DISTNAME)
SBINFILES =
LIBFILES = vm
LIBCOMPS-vm =   basic_reader osf_reader basic_writer instructions vm speculate emp_dumper kurden_approximator russen_approximator solve_hohman solve_meetandgreet solve_bielyptic simulation
DOCFILES = COPYING LICENSE CHANGELOG VERSION 
OTHERFILES = $(DISTNAME).spec
SRCFILES = $(shell echo *.ml *.mli *.mll *.mly) Makefile
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES =
MODULES =  basic_reader osf_reader basic_writer instructions vm speculate emp_dumper kurden_approximator russen_approximator solve_hohman solve_meetandgreet solve_bielyptic simulation
LIBRARIES = ../ellipse_josef/ellipsen
BINARIES = $(BINFILES) $(SBINFILES)
DISTVERSION = $(shell cat VERSION)

LIBDIRS =  ../ellipse_josef
BUILDSUBDIRS = ../ellipse_josef/
REBUILDSUBDIRS = 

INSTALL_PREFIX = /

include Makefile.ocaml
