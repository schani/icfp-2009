# $Id$

DISTNAME = sim 
BINFILES = $(DISTNAME)
SBINFILES =
LIBFILES = vm
LIBCOMPS-vm = vec basic_reader osf_reader basic_writer instructions vm speculate emp_dumper kurden_approximator russen_approximator solve_hohman solve_meetandgreet solve_bielyptic solve_eccentric simulation
DOCFILES = COPYING LICENSE CHANGELOG VERSION 
OTHERFILES = $(DISTNAME).spec
SRCFILES = $(shell echo *.ml *.mli *.mll *.mly) Makefile
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES =
MODULES =  vec basic_reader osf_reader basic_writer instructions vm speculate emp_dumper kurden_approximator russen_approximator solve_hohman solve_meetandgreet solve_bielyptic solve_eccentric simulation
LIBRARIES = ../ellipse_josef/ellipsen
BINARIES = $(BINFILES) $(SBINFILES)
DISTVERSION = $(shell cat VERSION)

LIBDIRS =  ../ellipse_josef
BUILDSUBDIRS = ../ellipse_josef/
REBUILDSUBDIRS = 

INSTALL_PREFIX = /

include Makefile.ocaml
