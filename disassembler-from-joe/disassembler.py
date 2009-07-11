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

def op_args(op):
	if op[0] == 'alu':
		return [op[2], op[3]]
	elif op[0] == 'output' or op[0] == 'cmpz':
		return [op[2]]
	elif op[0] == 'phi':
		return [op[1], op[2]]
	elif op[0] == 'noop' or op[0] == 'input':
		return []
	elif op[0] == 'sqrt' or op[0] == 'copy':
		return [op[1]]
	else:
		raise Exception('Unknown opcode %s' % op[0])

def alu_op_symbol(name):
	if name == 'add':
		return '+'
	elif name == 'sub':
		return '-'
	elif name == 'mult':
		return '*'
	elif name == 'div':
		return '/'
	else:
		raise Exception('Unknown alu op %s' % name)

def cmp_op_name(name):
	if name == 'ltz':
		return '<'
	elif name == 'lez':
		return '<='
	elif name == 'eqz':
		return '=='
	elif name == 'gez':
		return '>='
	elif name == 'gtz':
		return '>'
	else:
		raise Exception('Unknown cmp op %s' % name)

def var_name(addr):
	return 'v%s' % addr

class Decompiler (CodeCollector):
	def __init__(self):
		CodeCollector.__init__(self)

	def build_uses(self):
		self.uses = [[] for i in range(len(self.code))]
		for addr in range(len(self.code)):
			op = self.code[addr]
			args = op_args(op)
			for arg in args:
				self.uses[arg].append(addr)

	def var_or_const(self, addr, need_parens = False):
		op = self.code[addr]
		if op[0] == 'noop':
			return self.mem[addr]
		elif op[0] == 'cmpz' or op[0] == 'output':
			raise Exception('Schweinerei')
		elif not self.need_print_insn(addr):
			rhs = self.op_rhs(addr)
			if need_parens:
				return '(%s)' % rhs
			else:
				return rhs
		else:
			return var_name(addr)

	def op_rhs(self, addr):
		op = self.code[addr]
		if op[0] == 'alu':
			return '%s %s %s' % \
			      (self.var_or_const(op[2], True), alu_op_symbol(op[1]), \
			       self.var_or_const(op[3], True))
		elif op[0] == 'output':
			return '%s' % self.var_or_const(op[2])
		elif op[0] == 'sqrt':
			return 'sqrt %s' % self.var_or_const(op[1], True)
		elif op[0] == 'copy':
			return self.var_or_const(op[1])
		elif op[0] == 'input':
			return 'port[%s]' % op[1]
		elif op[0] == 'phi':
			return var_name(addr)
		elif op[0] == 'cmpz' or op[0] == 'noop':
			raise Exception('Schweinerei')
		else:
			raise Exception('Unknown opcode %s' % op[0])

	def need_print_insn(self, addr):
		op = self.code[addr]
		if op[0] == 'output' or op[0] == 'cmpz' or op[0] == 'phi':
			return True
		elif len(self.uses[addr]) == 0:
			return False
		elif len(self.uses[addr]) == 1:
			return self.uses[addr][0] <= addr
		else:
			return True

	def decompile(self):
		#print 'have %d insns' % len(self.code)
		addr = 0
		while addr < len(self.code):
			op = self.code[addr]
			#print 'decompiling insn %d: %s' % (addr, op)
			if not self.need_print_insn(addr):
				pass
			elif op[0] == 'alu':
				print '%s = %s' % (var_name(addr), self.op_rhs(addr))
			elif op[0] == 'output':
				print 'port[%s] = %s' % (op[1], self.op_rhs(addr))
			elif op[0] == 'phi':
				raise Exception('Schweinerei')
			elif op[0] == 'noop':
				pass
			elif op[0] == 'cmpz':
				phi = self.code[addr+1]
				if phi[0] != 'phi':
					raise Exception('Schweinerei')
				print 'if %s %s 0:' % (self.var_or_const(op[2], True), cmp_op_name(op[1]))
				print '    %s = %s' % (var_name(addr+1), self.var_or_const(phi[1]))
				print 'else:'
				print '    %s = %s' % (var_name(addr+1), self.var_or_const(phi[2]))
				addr += 1
			elif op[0] == 'sqrt':
				print '%s = %s' % (var_name(addr), self.op_rhs(addr))
			elif op[0] == 'copy':
				print '%s = %s' % (var_name(addr), self.op_rhs(addr))
			elif op[0] == 'input':
				print '%s = %s' % (var_name(addr), self.op_rhs(addr))
			else:
				raise Exception('Unknown opcode %s' % op[0])
			addr += 1

	def finish(self):
		self.build_uses()
		self.decompile()

class Compiler (CodeCollector):
	def __init__(self, language):
		CodeCollector.__init__(self)
		self.language = language
		if language == 'C':
			self.state_prefix = 'state->'
		elif language == 'Java':
			self.state_prefix = ''
		else:
			raise Exception('Unsupported target language %s' % language)

	def gen_struct(self):
		self.port_max = -1
		self.inputs = set([])
		self.vars = set([])
		for addr in range(len(self.code)):
			op = self.code[addr]
			if op[0] == 'input':
				self.inputs.add(op[1])
			if op[0] == 'output':
				self.port_max = max(self.port_max, op[1])
			elif  op[0] == 'cmpz':
				pass
			else:
				self.vars.add(addr)

		if self.language == 'C':
			print 'typedef struct {'
		for input in self.inputs:
			print 'double input_%d;' % input
		if self.language == 'C':
			print '} machine_inputs_t;'
		else:
			print 'public void setInput (int port, double value) {'
			for input in self.inputs:
				print 'if (port == %d) input_%d = value;' % (input, input)
			print '}'

		if self.language == 'C':
			print 'typedef struct {'

		for addr in self.vars:
			print 'double %s;' % var_name(addr)
		if self.language == 'C':
			print 'double output[%d];' % (self.port_max+1)
		else:
			print 'double output[];'
			print 'public double getOutput (int port) { return output[port]; }'
		print 'int num_timesteps_executed;'

		if self.language == 'C':
			print 'machine_inputs_t inputs;'
			print '} machine_state_t;'

			print 'void compare_inputs (machine_inputs_t *old, machine_inputs_t *new, compare_init_func_t init, set_new_value_func_t set, gpointer user_data) {'
			print 'guint32 count = 0;'
			for input in self.inputs:
				print 'if (old->input_%d != new->input_%d) ++count;' % (input, input)
			print 'init(count, user_data);'
			for input in self.inputs:
				print 'if (old->input_%d != new->input_%d) set(%d, new->input_%d, user_data);' % (input, input, input, input)
			print '}'

	def input_var(self, input):
		if self.language == 'C':
			return 'state->inputs.input_%d' % input
		else:
			return 'input_%d' % input

	def mem_var(self, addr):
		if self.language == 'C':
			return 'state->%s' % var_name(addr)
		else:
			return var_name(addr)

	def gen_inits(self):
		if self.language == 'C':
			print 'void init_machine (machine_state_t *state) {'
			for addr in self.vars:
				print 'unsigned char %s_mem[] = {' % var_name(addr)
				for c in struct.pack("d", self.mem[addr]):
					print '0x%02X,' % ord(c)
				print '};'
				print 'state->%s = *(double*)%s_mem;' % (var_name(addr), var_name(addr))
		else:
			print 'public Machine () {'
			for addr in self.vars:
				print '%s = Double.longBitsToDouble' % var_name(addr),
				print '(0x%02X%02X%02X%02X%02X%02X%02X%02XL);' % tuple(map(ord,reversed(struct.pack("d", self.mem[addr]))))
			print 'output = new double[%d];' % (self.port_max+1)
		for input in self.inputs:
			print '%s = 0.0;' % self.input_var(input)
		print 'for (int i = 0; i < %d; ++i) %soutput[i] = 0.0;' % (self.port_max+1, self.state_prefix)
		print '%snum_timesteps_executed = 0;' % self.state_prefix
		print '}'

	def gen_loop(self):
		if self.language == 'C':
			print 'void timestep (machine_state_t *state) {'
		else:
			print 'public void timestep () {'
		addr = 0
		while addr < len(self.code):
			op = self.code[addr]
			#print 'decompiling insn %d: %s' % (addr, op)
			if op[0] == 'alu':
				if op[1] == 'div':
					print '%s = (%s == 0.0) ? 0.0 : (%s / %s);' % (self.mem_var(addr), self.mem_var(op[3]), self.mem_var(op[2]), self.mem_var(op[3]))
				else:
					print '%s = %s %s %s;' % (self.mem_var(addr), self.mem_var(op[2]), alu_op_symbol(op[1]), self.mem_var(op[3]))
			elif op[0] == 'output':
				print '%soutput[%d] = %s;' % (self.state_prefix, op[1], self.mem_var(op[2]))
			elif op[0] == 'phi':
				raise Exception('Schweinerei')
			elif op[0] == 'noop':
				pass
			elif op[0] == 'cmpz':
				phi = self.code[addr+1]
				if phi[0] != 'phi':
					raise Exception('Schweinerei')
				print 'if (%s %s 0.0)' % (self.mem_var(op[2]), cmp_op_name(op[1]))
				print '    %s = %s;' % (self.mem_var(addr+1), self.mem_var(phi[1]))
				print 'else'
				print '    %s = %s;' % (self.mem_var(addr+1), self.mem_var(phi[2]))
				op = phi
				addr += 1
			elif op[0] == 'sqrt':
				if self.language == 'C':
					print '%s = sqrt(%s);' % (self.mem_var(addr), self.mem_var(op[1]))
				else:
					print '%s = Math.sqrt(%s);' % (self.mem_var(addr), self.mem_var(op[1]))
			elif op[0] == 'copy':
				print '%s = %s;' % (self.mem_var(addr), self.mem_var(op[1]))
			elif op[0] == 'input':
				print '%s = %s;' % (self.mem_var(addr), self.input_var(op[1]))
			else:
				raise Exception('Unknown opcode %s' % op[0])
			#if op[0] != 'noop' and op[0] != 'cmpz' and op[0] != 'output':
			#print 'printf("%%d %d %%.8f\\n", state->num_timesteps_executed, state->%s);' % (addr, var_name(addr))
			addr += 1
		print '++%snum_timesteps_executed;' % self.state_prefix
		print '}'

	def finish(self):
		if self.language == 'Java':
			print 'package at.ac.tuwien.complang.icfp2009;'
			print 'public class Machine {'
		self.gen_struct()
		self.gen_inits()
		self.gen_loop()
		if self.language == 'Java':
			print '}'

def to_binary(i):
	s = ''
	for n in range(32):
		if i & 1:
			s += '1'
		else:
			s += '0'
		i = i >> 1
	return s[::-1]

if len(sys.argv) > 1 and sys.argv[1] == "-d":
	backend = CodeDisassembler()
elif len(sys.argv) > 1 and sys.argv[1] == "-c":
	backend = Compiler('C')
elif len(sys.argv) > 1 and sys.argv[1] == "-j":
	backend = Compiler('Java')
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

	#print to_binary(i)

	val = struct.unpack("d", val_s)[0]

#	print "%d (%f)" % (addr, val),

	if op1 == 0:
		op2 = (i & 0xf000000) >> 24
		imm = (i & 0xffc000) >> 14
		cmpop = (i >> 21) & 0x7
		r1 = (i & 0x3fff)

		if op2 == 0:
			backend.noop_op(val)
		elif op2 == 1:
			if cmpop == 0:
				cmpop = "ltz"
			elif cmpop == 1:
				cmpop = "lez"
			elif cmpop == 2:
				cmpop = "eqz"
			elif cmpop == 3:
				cmpop = "gez"
			elif cmpop == 4:
				cmpop = "gtz"
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
			backend.alu_op(val, "add", r1, r2)
		elif op1 == 2:
			backend.alu_op(val, "sub", r1, r2)
		elif op1 == 3:
			backend.alu_op(val, "mult", r1, r2)
		elif op1 == 4:
			backend.alu_op(val, "div", r1, r2)
		elif op1 == 5:
			backend.output_op(val, r1, r2)
		elif op1 == 6:
			backend.phi_op(val, r1, r2)
		else:
			raise Exception("Invalid opcode1 %d" % op1)
