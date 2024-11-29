
from strings_with_arrows import *

#############
# Constants #
#############

#constants for token types, TT stands for Token Type
TT_INT		= 'INT'
TT_FLOAT    = 'FLOAT'
TT_PLUS     = 'PLUS'
TT_MINUS    = 'MINUS'
TT_MUL      = 'MUL'
TT_DIV      = 'DIV'
TT_LPAREN   = 'LPAREN'
TT_RPAREN   = 'RPAREN'
TT_EOF		= 'EOF'

#for detectig if a character is a digit.
DIGITS = '0123456789'

#################
3. # Error Classes #
#################

class Error:
	def __init__(self, pos_start, pos_end, error_name, details): #takes in error name and details
		self.pos_start = pos_start
		self.pos_end = pos_end
		self.error_name = error_name
		self.details = details
	
	def as_string(self): #shows the error information as a string
		result  = f'{self.error_name}: {self.details}\n' #includes the error name and details
		result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end) #this inserts an arrow pointing at the problem.
		return result

#super(). calls the superclass' method and extends it. 

class IllegalCharError(Error): #subclass of error, standard error for when lexer finds a character it doesn't support.
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Illegal Character', details)

class InvalidSyntaxError(Error): #error for whenever someone puts in math (or other syntax) wrong.
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

class RTError(Error):
	def __init__(self, pos_start, pos_end, details, context):
		super().__init__(pos_start, pos_end, 'Runtime Error', details)
		self.context = context

	def as_string(self):
		result  = self.generate_traceback()
		result += f'{self.error_name}: {self.details}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

	def generate_traceback(self):
		result = ''
		pos = self.pos_start
		ctx = self.context

		while ctx:
			result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
			pos = ctx.parent_entry_pos
			ctx = ctx.parent

		return 'Traceback (most recent call last):\n' + result

   ##################
5. # Position Class #
   ##################

class Position:
	def __init__(self, idx, ln, col, fn, ftxt): # takes in an index, a line number, a column number, a file name, and the file contents.
		self.idx = idx
		self.ln = ln
		self.col = col
		self.fn = fn
		self.ftxt = ftxt

	def advance(self, current_char=None):
		self.idx += 1 #updates index and column
		self.col += 1

		if current_char == '\n': #checks if character is a new line character, because then it resets column and increases line.
			self.ln += 1
			self.col = 0

		return self

	def copy(self): #makes a copy of the position.
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

   ###########
1. # Tokens #
   ##########

class Token:
    #initialization method, gives the token a type, and a value. also has the start and stop position of the token, so the error class can specify where it is if necessary.
	def __init__(self, type_, value=None, pos_start=None, pos_end=None):
		self.type = type_
		self.value = value

		if pos_start:
			self.pos_start = pos_start.copy()
			self.pos_end = pos_start.copy()
			self.pos_end.advance()

		if pos_end:
			self.pos_end = pos_end
	
	#representation method, controls what it looks like when printed out.
	def __repr__(self):
		if self.value: return f'{self.type}:{self.value}' #if it has a value, print type : value
		return f'{self.type}' #otherwise just print the type.

   ###############
2. # Lexer Class #
   ###############

class Lexer:
	def __init__(self, fn, text):
		self.fn = fn
		self.text = text #the text that we're processing.
		self.pos = Position(-1, 0, -1, fn, text) #keeps track of current position.
		self.current_char = None #the current character.
		self.advance()
	
	def advance(self):
		self.pos.advance(self.current_char) #moves the current position forward.
		#updates the current character. If it's reached the end of the text, it becomes none.
		self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None 

	def make_tokens(self):
		tokens = []

		while self.current_char != None:
			if self.current_char in ' \t': #ignores spaces and tabs
				self.advance()
			elif self.current_char in DIGITS: #checks if the character is a digit. uses a function to tell that numbers together are a single number.
				tokens.append(self.make_number())
			elif self.current_char == '+': #checks if it's +
				tokens.append(Token(TT_PLUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '-': #checks if it's -
				tokens.append(Token(TT_MINUS, pos_start=self.pos))
				self.advance()
			elif self.current_char == '*': #checks if it's *
				tokens.append(Token(TT_MUL, pos_start=self.pos))
				self.advance()
			elif self.current_char == '/': #checks if it's /
				tokens.append(Token(TT_DIV, pos_start=self.pos))
				self.advance()
			elif self.current_char == '(': #checks for (
				tokens.append(Token(TT_LPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_char == ')': #checks for )
				tokens.append(Token(TT_RPAREN, pos_start=self.pos))
				self.advance()
			else: #cause an error
				pos_start = self.pos.copy()
				char = self.current_char #stores the character that caused the error.
				self.advance()
				#returns an empty list for the character, so it doesn't add anything, and returns an illegal character error.
				return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

		tokens.append(Token(TT_EOF, pos_start=self.pos))
		return tokens, None

	def make_number(self):
		num_str = '' #for keeping track of string form
		dot_count = 0 #keeps track of decimal count, to tell if it's a float or intiger.
		pos_start = self.pos.copy()

		while self.current_char != None and self.current_char in DIGITS + '.': #repeats as long as it's not empty and it's a number (or decimals)
			if self.current_char == '.':
				if dot_count == 1: break #can't have 2 decimals, so leaves.
				dot_count += 1
				num_str += '.'
			else:
				num_str += self.current_char
			self.advance()

		if dot_count == 0: #handles returning whether it was a float or int.
			return Token(TT_INT, int(num_str), pos_start, self.pos)
		else:
			return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

   ################
6. # Node Classes #
   ################
#Nodes are the parts of a math expression.
class NumberNode: #just takes in the number of tokens.
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self): #returns token in a string.
		return f'{self.tok}'

class BinOpNode: #for +, -, *, and /.
	def __init__(self, left_node, op_tok, right_node): #needs a left node, the operation, and a right node to perform an operation.
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

		self.pos_start = self.left_node.pos_start
		self.pos_end = self.right_node.pos_end

	def __repr__(self): #representation method.
		return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode: #Unary operation node, for making an umber pos or neg
	def __init__(self, op_tok, node): #takes in the operation token and the node.
		self.op_tok = op_tok
		self.node = node

		self.pos_start = self.op_tok.pos_start
		self.pos_end = node.pos_end

	def __repr__(self): #representation method.
		return f'({self.op_tok}, {self.node})'

   ######################
8. # Parse Result Class #
   ######################
#checks if there's any errors
class ParseResult:
	def __init__(self):
		self.error = None #keeps track of error if it exists
		self.node = None #keeps track of the node

	def register(self, res): #takes in either other parse results or a node.
		if isinstance(res, ParseResult): #checks if it's a parse result. if it is and it has an error, copy the error over. Then return it's node.
			if res.error: self.error = res.error
			return res.node

		return res #it was a node, so return the node.

	def success(self, node): #a success takes in a node, and return it.
		self.node = node
		return self

	def failure(self, error): #failure takes an error instead.
		self.error = error
		return self

   ################
7. # Parser Class #
   ################
#the parser puts together the nodes to make a math expression work.

class Parser:
	def __init__(self, tokens): #takes in a list of tokens.
		self.tokens = tokens
		self.tok_idx = -1 #keeps track of the current index,
		self.advance() #Needs to be able to advance.

	def advance(self, ): #advances the token index and sets the current token.
		self.tok_idx += 1
		if self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]
		return self.current_tok

	def parse(self):
		res = self.expr()
		if not res.error and self.current_tok.type != TT_EOF:
			return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected '+', '-', '*' or '/'"))
		return res

	def factor(self):
		res = ParseResult()
		tok = self.current_tok #gets the current token

		if tok.type in (TT_PLUS, TT_MINUS): #checks for unary character to make number pos or neg.
			res.register(self.advance())
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))
		
		elif tok.type in (TT_INT, TT_FLOAT): #checks if token is an integer or a float
			res.register(self.advance())
			return res.success(NumberNode(tok))

		elif tok.type == TT_LPAREN: #checks if token is a left parenthesis, handles the expression inside the parenthesis.
			res.register(self.advance())
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RPAREN:
				res.register(self.advance())
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected ')'"))

		return res.failure(InvalidSyntaxError(tok.pos_start, tok.pos_end, "Expected int or float")) #returns a failure, because it didn't get an int or float.

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV)) #Gets a term by looking for factors, and then the multiplication or division operator.

	def expr(self):
		return self.bin_op(self.term, (TT_PLUS, TT_MINUS)) #gets an expression by looking for terms, and then the addition or subtraction operator.

	def bin_op(self, func, ops): #Looks for the specified function (either factor or term), and then looks for the specified operators.
		res = ParseResult()
		left = res.register(func())
		if res.error: return res

		while self.current_tok.type in ops:
			op_tok = self.current_tok
			res.register(self.advance())
			right = res.register(func())
			if res.error: return res
			left = BinOpNode(left, op_tok, right)

		return res.success(left)

	########################
11. # Runtime Result Class #
	########################

class RTResult:
	def __init__(self):
		self.value = None
		self.error = None

	def register(self, res):
		if res.error: self.error = res.error
		return res.value

	def success(self, value):
		self.value = value
		return self

	def failure(self, error):
		self.error = error
		return self

    ##########
10. # Values #
    ##########

#for storying numbers and operating on them as values.
class Number:
	def __init__(self, value):
		self.value = value
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	#comments for following few sections are same format as added_to.
	def added_to(self, other): #adds the number to something else.
		if isinstance(other, Number): #checks if the other thing is a number.
			return Number(self.value + other.value).set_context(self.context), None

	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0: #Check for divide by 0 error.
				return None, RTError(other.pos_start, other.pos_end, 'Division by zero', self.context)

			return Number(self.value / other.value).set_context(self.context), None

	def __repr__(self):
		return str(self.value)

	#################
12. # Context Class #
	#################

class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos

   #####################
9. # Interpreter Class #
   #####################

#performs the actions the code tells it to.

class Interpreter:
	def visit(self, node, context): #this dooes some meta programming stuff to visit an operation depending on the node.
		method_name = f'visit_{type(node).__name__}'
		method = getattr(self, method_name, self.no_visit_method)
		return method(node, context)

	def no_visit_method(self, node, context): #if there isn't a method availble, visit this one.
		raise Exception(f'No visit_{type(node).__name__} method defined')

	def visit_NumberNode(self, node, context): #this node for for a number
		return RTResult().success(
			Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))

	def visit_BinOpNode(self, node, context): #this node is for a binary operation
		res = RTResult()
		left = res.register(self.visit(node.left_node, context))
		if res.error: return res
		right = res.register(self.visit(node.right_node, context))
		if res.error: return res

		if node.op_tok.type == TT_PLUS: #if it's a plus, add them
			result, error = left.added_to(right)
		elif node.op_tok.type == TT_MINUS: #if it's a minus, subtract them
			result, error = left.subbed_by(right)
		elif node.op_tok.type == TT_MUL: #if it's a multiply, multiply them
			result, error = left.multed_by(right)
		elif node.op_tok.type == TT_DIV: #if it's a divide, divide them
			result, error = left.dived_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_UnaryOpNode(self, node, context): #this node is for a unary operation
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.error: return res

		error = None

		if node.op_tok.type == TT_MINUS: #if it's a minus, make the number negative by multiplying it by -1.
			number, error = number.multed_by(Number(-1))

		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))

   #################
4. # Run / Execute #
   #################

def run(fn, text): #takes in text and runs it.
	# Generate tokens
	lexer = Lexer(fn, text) #makes a new lexer and gives it the code file
	tokens, error = lexer.make_tokens() #makes the tokens from the code and gets them, along with an error if there is one.
	if error: return None, error # if there's an error, stops the program and returns the error.
	
	# Generate AST (abstract syntax tree)
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

	# Run program
	interpreter = Interpreter()
	context = Context('<program>')
	result = interpreter.visit(ast.node, context)

	return result.value, result.error
