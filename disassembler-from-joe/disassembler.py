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
	def cmpz_op(self, mem, op, r1):
		pass

	def sqrt_op(self, mem, r1):
		pass

	def copy_op(self, mem, r1):
		pass

	def input_op(self, mem, r1):
		pass

	def finish(self):
		pass

class CodeDisassembler(CodeSink):
	def __init__(self):
		print "init"
		self.addr = 0

	def print_addr_and_mem(self, mem):
		print "%d (%f)" % (self.addr, val),
		self.addr += 1

	def noop_op(self, mem):
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
		print "Cmpz.%s %d" % (op, r1)

	def sqrt_op(self, mem, r1):
		self.print_addr_and_mem(mem)

		print "Sqrt %d" % r1

	def copy_op(self, mem, r1):
		self.print_addr_and_mem(mem)
		print "Copy %d" % r1

	def input_op(self, mem, r1):
		self.print_addr_and_mem(mem)
		print "Input %d" % r1

class CodeCollector (CodeSink):
	def __init__(self):
		self.code = []
		self.mem = []

	def alu_op(self, mem, op, r1, r2):
		self.mem.append(mem)
		self.code.append(('alu', op, r1, r2))

	def output_op(self, mem, r1, r2):
		self.mem.append(mem)
		self.code.append(('output', r1, r2))

	def phi_op(self, mem, r1, r2):
		self.mem.append(mem)
		self.code.append(('phi', r1, r2))

	def noop_op(self, mem):
		self.mem.append(mem)
		self.code.append(('noop', ))

	# op is 'ltz', 'lez', 'eqz', 'gez', or 'gtz'
	def cmpz_op(self, mem, op, r1):
		self.mem.append(mem)
		self.code.append(('cmpz', op, r1))

	def sqrt_op(self, mem, r1):
		self.mem.append(mem)
		self.code.append(('sqrt', r1))

	def copy_op(self, mem, r1):
		self.mem.append(mem)
		self.code.append(('copy', r1))

	def input_op(self, mem, r1):
		self.mem.append(mem)
		self.code.append(('input', r1))

class Decompiler (CodeCollector):
	def __init__(self):
		CodeCollector.__init__(self)

	def var_or_const(self,addr):
		op = self.code[addr]
		if op[0] == 'noop':
			return self.mem[addr]
		elif op[0] == 'cmpz' or op[0] == 'output':
			raise Exception('Schweinerei')
		else:
			return 'v%d' % addr

	def decompile(self):
		#print 'have %d insns' % len(self.code)
		addr = 0
		while addr < len(self.code):
			op = self.code[addr]
			#print 'decompiling insn %d: %s' % (addr, op)
			if op[0] == 'alu':
				print '%s = %s %s %s' % \
				      (self.var_or_const(addr), self.var_or_const(op[2]), \
				       op[1], self.var_or_const(op[3]))
			elif op[0] == 'output':
				print 'output %s' % self.var_or_const(op[1])
			elif op[0] == 'phi':
				raise Exception('Schweinerei')
			elif op[0] == 'noop':
				pass
			elif op[0] == 'cmpz':
				phi = self.code[addr+1]
				if phi[0] != 'phi':
					raise Exception('Schweinerei')
				print 'if %s %s 0:' % (self.var_or_const(op[2]), op[1])
				print '    %s = %s' % (self.var_or_const(addr+1), self.var_or_const(phi[1]))
				print 'else:'
				print '    %s = %s' % (self.var_or_const(addr+1), self.var_or_const(phi[2]))
				addr += 1
			elif op[0] == 'sqrt':
				print '%s = sqrt %s' % (self.var_or_const(addr), self.var_or_const(op[1]))
			elif op[0] == 'copy':
				print '%s = %s' % (self.var_or_const(addr), self.var_or_const(op[1]))
			elif op[0] == 'input':
				print '%s = port[%s]' % (self.var_or_const(addr), op[1])
			else:
				raise Exception('Unknown opcode %s' % op[0])
			addr += 1

	def finish(self):
		self.decompile()

if len(sys.argv) > 1 and sys.argv[1] == "-d":
	backend = CodeDisassembler()
else:
	backend = Decompiler()

addr = -1
while 1:
	addr += 1
	if (addr & 1) == 0:
		val_s = sys.stdin.read(8)
		if val_s == "":
			backend.finish()
			sys.exit()

		s = sys.stdin.read(4)
		if s == "":
			backend.finish()
			sys.exit()
	else:
		s = sys.stdin.read(4)
		if s == "":
			backend.finish()
			sys.exit()

		val_s = sys.stdin.read(8)
		if val_s == "":
			backend.finish()
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
			backend.noop_op(val)
		elif op2 == 1:
			if cmpop == 0:
				cmpop = "LTZ"
			elif cmpop == 1:
				cmpop = "LEZ"
			elif cmpop == 2:
				cmpop = "EQZ"
			elif cmpop == 3:
				cmpop = "GEZ"
			elif cmpop == 4:
				cmpop = "GTZ"
			else:
				raise Exception("Invalid immediate (%d) in Cmpz" % op)
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
