#!/usr/bin/python

import struct
import sys

addr = -1
while 1:
	addr += 1
	if (addr & 1) == 0:
		val_s = sys.stdin.read(8)
		if val_s == "":
			sys.exit()

		s = sys.stdin.read(4)
		if s == "":
			sys.exit()
	else:
		s = sys.stdin.read(4)
		if s == "":
			sys.exit()

		val_s = sys.stdin.read(8)
		if val_s == "":
			sys.exit()

	i = struct.unpack("i", s)[0]
	op1 = (i & 0xf0000000) >> 28

	val = struct.unpack("d", val_s)[0]

	print "%d (%f)" % (addr, val),

	if op1 == 0:
		op2 = (i & 0xf000000) >> 24
		imm = (i & 0xffc000) >> 14
		cmpop = (i & 0xf00000) >> 20
		r1 = (i & 0x3fff)

		if op2 == 0:
			print "Noop"
		elif op2 == 1:
			if cmpop == 0:
				print "Cmpz.LTZ %d" % r1
			elif cmpop == 1:
				print "Cmpz.LEZ %d" % r1
			elif cmpop == 2:
				print "Cmpz.EQZ %d" % r1
			elif cmpop == 3:
				print "Cmpz.GEZ %d" % r1
			elif cmpop == 4:
				print "Cmpz.GTZ %d" % r1
			else:
				raise Exception("Invalid immediate (%d) in Cmpzl" % imm)
		elif op2 == 2:
			print "Sqrt %d" % r1
		elif op2 == 3:
			print "Copy %d" % r1
		elif op2 == 4:
			print "Input %d" % r1
		else:
			raise Exception("Invalid opcode2 %d" % op2)
	else:
		r1 = (i & 0xfffc000) >> 14
		r2 = (i & 0x3fff)

		if op1 == 1:
			print "Add %d, %d" % (r1, r2)
		elif op1 == 2:
			print "Sub %d, %d" % (r1, r2)
		elif op1 == 3:
			print "Mult %d, %d" % (r1, r2)
		elif op1 == 4:
			print "Div %d, %d" % (r1, r2)
		elif op1 == 5:
			print "Output %d, %d" % (r1, r2)
		elif op1 == 6:
			print "Phi %d, %d" % (r1, r2)
		else:
			raise Exception("Invalide opcode1 %d" % op1)
