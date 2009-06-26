#!/usr/bin/python

# This takes a obf binary as input, disassembles it 
# and outputs the result.
#
# Later it will also output the binary as Mark's
# high-level simulator language.
#

import struct
import sys

# Abstract superclass - not pythonic but lets leave it there ...
class CodeSink:
	def no_op(self, mem):
		pass

	# op is 'add', 'sub', 'mult', or 'div'
	def alu_op(self, mem, op, r1, r2):
		pass

	def output_op(self, mem, r1, r2):
		pass

	def phi_op(self, mem, r1, r2):
		pass

	def noop_op(self, mem):
		pass

	# op is 'ltz', 'lez', 'eqz', 'gez', or 'gtz'
	def cmpz_op(self, mem, op, imm, r1):
		pass

	def sqrt_op(self, mem, r1):
		pass

	def copy_op(self, mem, r1):
		pass

	def input_op(self, mem, r1):
		pass



class CodeDisassembler(CodeSink):
	def __init__(self):
		print "init"
		self.addr = 0

	def print_addr_and_mem(self, mem):
		print "%d (%f)" % (self.addr, val),
		self.addr += 1

	def no_op(self, mem):
		self.print_addr_and_mem(mem)
		print "Noop"

	# op is 'add', 'sub', 'mult', or 'div'
	def alu_op(self, mem, op, r1, r2):
		self.print_addr_and_mem(mem)
		print "%s %d, %d" % (op, r1, r2)

	def output_op(self, mem, r1, r2):
		self.print_addr_and_mem(mem)
		print "Output %d, %d" % (r1, r2)

	def phi_op(self, mem, r1, r2):
		self.print_addr_and_mem(mem)
		print "Phi %d, %d" % (r1, r2)

	# op is 'ltz', 'lez', 'eqz', 'gez', or 'gtz'
	def cmpz_op(self, mem, op, r1):
		self.print_addr_and_mem(mem)

		if op == 0:
			print "Cmpz.LTZ %d" % r1
		elif op == 1:
			print "Cmpz.LEZ %d" % r1
		elif op == 2:
			print "Cmpz.EQZ %d" % r1
		elif op == 3:
			print "Cmpz.GEZ %d" % r1
		elif op == 4:
			print "Cmpz.GTZ %d" % r1
		else:
			raise Exception("Invalid immediate (%d) in Cmpzl" % op)
		pass

	def sqrt_op(self, mem, r1):
		self.print_addr_and_mem(mem)

		print "Sqrt %d" % r1

	def copy_op(self, mem, r1):
		self.print_addr_and_mem(mem)
		print "Copy %d" % r1

	def input_op(self, mem, r1):
		self.print_addr_and_mem(mem)
		print "Input %d" % r1

if sys.argv[1] == "-d":
	backend = CodeDisassembler()
else:
	backend = CodeCollector() # todo: 

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

#	print "%d (%f)" % (addr, val),

	if op1 == 0:
		op2 = (i & 0xf000000) >> 24
		imm = (i & 0xffc000) >> 14
		cmpop = (i & 0xf00000) >> 20
		r1 = (i & 0x3fff)

		if op2 == 0:
			backend.no_op(val)
		elif op2 == 1:
			backend.cmpz_op(val, cmpop, r1)
		elif op2 == 2:
			backend.sqrt_op(val, r1)
		elif op2 == 3:
			backend.copy_op(val, r1)
		elif op2 == 4:
			backend.input_op(val, r1)
		else:
			raise Exception("Invalid opcode2 %d" % op2)
	else:
		r1 = (i & 0xfffc000) >> 14
		r2 = (i & 0x3fff)

		if op1 == 1:
			backend.alu_op(val, "Add", r1, r2)
		elif op1 == 2:
			backend.alu_op(val, "Sub", r1, r2)
		elif op1 == 3:
			backend.alu_op(val, "Mult", r1, r2)
		elif op1 == 4:
			backend.alu_op(val, "Div", r1, r2)
		elif op1 == 5:
			backend.output_op(val, r1, r2)
		elif op1 == 6:
			backend.phi_op(val, r1, r2)
		else:
			raise Exception("Invalide opcode1 %d" % op1)
