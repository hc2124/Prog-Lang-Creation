from strings_with_arrows import *

import string

#############
# Constants #
#############

#constants for token types, TT stands for Token Type
TT_INT		= 'INT'
TT_FLOAT    = 'FLOAT'
TT_IDENTIFIER = 'IDENTIFIER'
TT_STRING = 'STRING'
TT_KEYWORD  = 'KEYWORD'
TT_PLUS     = 'PLUS'
TT_MINUS    = 'MINUS'
TT_POSITIVE = 'POSITIVE' #added seperate token types for unary operators.
TT_NEGATIVE = 'NEGATIVE'
TT_MUL      = 'MUL'
TT_DIV      = 'DIV'
TT_POW		= 'POW'
TT_EQ		= 'EQ'
TT_LPAREN   = 'LPAREN'
TT_RPAREN   = 'RPAREN'
TT_EOF		= 'EOF'

KEYWORDS = [
	'item'
]

#for detectig if a character is a digit.
DIGITS = '0123456789'

LETTERS = string.ascii_letters #for detecting if a character is a letter.
LETTERS_DIGITS = LETTERS + DIGITS #for detecting if a character is a letter or a digit.

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
	def __init__(self, pos_start, pos_end, details=''):
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
	def __init__(self, idx, ln, col, fn, ftxt, words): # takes in an index, a line number, a column number, a file name, and the file contents.
		self.idx = idx
		self.ln = ln
		self.col = col
		self.fn = fn
		self.ftxt = ftxt
		self.words = words

	def advance(self, current_char=None):
		self.idx += 1 #updates index and column
		self.col += 1

		if self.col == len(self.words): #checks if current word is the last word. If so reset it.
			self.ln += 1
			self.col = 0

		return self

	def copy(self): #makes a copy of the position.
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt, self.words)

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
   
	def matches(self, type_, value):#checks if the token is of a certain type and value.
		return self.type == type_ and self.value == value
	
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
		self.words = self.preprocessText(self.text) #splits the text into a list of words and characters.
		self.pos = Position(-1, 0, -1, fn, text, self.words) #keeps track of current position.
		self.current_word = None #the current word.
		self.advance()
  
	def preprocessText(self, text): #splits the sentence into words, and then seperates special characters.
		words = text.split() #splits the text into a list of words.
		word = 0
		while word < len(words):
			if(words[word][0].isalnum() == False and len(words[word]) > 1): #the first character isn't alphanumeric, so seperate it.
				restOfWord = words[word][1:] #seperates the first character from the rest of the word.
				words[word] = words[word][0] #updates the current word to just the first character on it's own.
				words.insert(word + 1, restOfWord) #inserts the rest of the word right after this.
			elif ((words[word][-1].isalnum() == False and words[word][-1] != '_') and len(words[word]) > 1): #variables might end in an underscore, so we need to allow that.
				restOfWord = words[word][:-1] #seperates the ) from the rest of the word.
				words.insert(word + 1, words[word][-1]) #inserts the close parentheses after this.
				words[word] = restOfWord #updates the current word to just what was there before.'
				continue #calling continue so the current word is redone, since it was changed.
			word += 1 #moves to the next word.
		return words
				
    
	def advance(self):
		self.pos.advance(self.current_word) #moves the current position forward.
		#updates the current character. If it's reached the end of the text, it becomes none.
		self.current_word = self.words[self.pos.idx] if self.pos.idx < len(self.words) else None 

	def make_tokens(self):
		tokens = []

		while self.current_word != None:
			if self.current_word in ' \t': #ignores spaces and tabs
				self.advance() #This is actually useless in new code sicne white spaces are left out.
			elif self.current_word == '(': #checks for (
				tokens.append(Token(TT_LPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_word == ')': #checks for )
				tokens.append(Token(TT_RPAREN, pos_start=self.pos))
				self.advance()
			elif self.current_word == '+': #checks if it's + 
				tokens.append(Token(TT_POSITIVE, pos_start=self.pos))
				self.advance()
			elif self.current_word == '-': #checks if it's -
				tokens.append(Token(TT_NEGATIVE, pos_start=self.pos))
				self.advance()
			elif self.current_word == 'sum': #checks if it's +
				tokens.append(Token(TT_PLUS, pos_start=self.pos))
				self.advance()
			elif self.current_word == 'difference': #checks if it's -
				tokens.append(Token(TT_MINUS, pos_start=self.pos))
				self.advance()
			elif self.current_word == 'product': #checks if it's *
				tokens.append(Token(TT_MUL, pos_start=self.pos))
				self.advance()
			elif self.current_word == 'quotient': #checks if it's /
				tokens.append(Token(TT_DIV, pos_start=self.pos))
				self.advance()
			elif self.current_word == 'equals': #checks if it's =
				tokens.append(Token(TT_EQ, pos_start=self.pos))
				self.advance()
			elif self.current_word == 'power': #checks if it's power
				tokens.append(Token(TT_POW, pos_start=self.pos))
				self.advance()
			elif self.current_word == '"':
				tokens.append(self.make_string())
			elif self.current_word[0] in DIGITS: #checks if the character is a digit. uses a function to tell that numbers together are a single number.
				#print("appended number" + self.current_word + "word list: " + str(self.words)) DEBUG DELETE ME
				tokens.append(self.make_number())
			elif self.current_word[0] in LETTERS:
				tokens.append(self.make_identifier())
			else: #cause an error
				pos_start = self.pos.copy()
				word = self.current_word #stores the character that caused the error.
				self.advance()
				#returns an empty list for the character, so it doesn't add anything, and returns an illegal character error.
				return [], IllegalCharError(pos_start, self.pos, "'" + word + "'")

		tokens.append(Token(TT_EOF, pos_start=self.pos))
		return tokens, None

	def make_number(self):
		num_str = '' #for keeping track of string form
		dot_count = 0 #keeps track of decimal count, to tell if it's a float or intiger.
		pos_start = self.pos.copy()

		
		#while current_char != None and current_char in DIGITS + '.': #repeats as long as it's not empty and it's a number (or decimals)
		for char in self.current_word: #cycles thorugh each character
			if char == '.':
				if dot_count == 1: break #can't have 2 decimals, so leaves.
				dot_count += 1
				num_str += '.'
			elif char in DIGITS:
				num_str += char
			else: #need to raise an error, it's not a number or a period.
				return IllegalCharError(pos_start, self.pos, "Illegal character '" + char + "' found in number " + "'" + self.current_word + "'")

		self.advance() #advances the current word.
		if dot_count == 0: #handles returning whether it was a float or int.
			return Token(TT_INT, int(num_str), pos_start, self.pos)
		else:
			return Token(TT_FLOAT, float(num_str), pos_start, self.pos)

	def make_string(self):
		string = ''
		pos_start =  self.pos.copy()
		escape_character = False
		self.advance()

		escape_characters = {
			'n' : '\n', #new line
			't': '\t' #tab
		}

		while self.current_word != None and (self.current_word != '"' or escape_character): # loops continues until you reach double quote
			if escape_character:
				string += escape_characters.get(self.current_char, self.current_char)
			else:
				if self.current_word == '\\':
					escape_character = True
				else:
					string += ' ' + self.current_word
			self.advance()
			escape_character = False

		self.advance()
		return Token(TT_STRING, string, pos_start, self.pos)
	
    
	def make_identifier(self):
		id_str = ''
		pos_start = self.pos.copy()
  
		#while self.current_word != None and self.current_word in LETTERS_DIGITS + '_': #repeats as long as it's not empty and it's a letter, digit, or underscore.
		for char in self.current_word: #cycles thorugh each character
			if char in LETTERS_DIGITS + '_': #ensures the characters match the conditions set forth in the first langauge.
				id_str += char #adds character to the symbol name.
			else: #if it's not a letter, digit, or underscore, it's not a valid symbol name.
				return IllegalCharError(pos_start, self.pos, "Illegal character '" + char + "' found in identifier " + "'" + self.current_word + "'")

		self.advance()
   
		tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER #checks if the symbol is one of the keywords, otherwise make it identifier (variable)
		return Token(tok_type, id_str, pos_start, self.pos)

   ################
6. # Node Classes #
   ################
#Nodes are the parts of a math expression.


class StringNode: #just takes in the number of tokens.
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self): #returns token in a string.
		return f'{self.tok}'

class NumberNode: #just takes in the number of tokens.
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self): #returns token in a string.
		return f'{self.tok}'

class VarAccessNode: #for accessing a variable value
	def __init__(self, var_name_tok):
		self.var_name_tok = var_name_tok

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.var_name_tok.pos_end

class VarAssignNode: #for assigning a variable value
	def __init__(self, var_name_tok, value_node):
		self.var_name_tok = var_name_tok
		self.value_node = value_node

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.value_node.pos_end

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
		self.advance_count = 0 #keeps track of how many times it's advanced.

	def register_advancement(self): #This register method is only for passing in advacements.
		self.advance_count += 1

	def register(self, res): #This register method is only for passing in parse results
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node

	def success(self, node): #a success takes in a node, and return it.
		self.node = node
		return self

	def failure(self, error): #failure takes an error instead.
		if not self.error or self.advance_count == 0: #raises this error that calls out lack of item if it hasn't advanced since the beginning, meaning this it the first token.
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

#######################
	def atom(self):
		res = ParseResult()
		tok = self.current_tok

		if tok.type in (TT_INT, TT_FLOAT): #checks if token is an integer or a float
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))
            
		if tok.type in (TT_STRING): #checks if token is an integer or a float
			res.register_advancement()
			self.advance()
			return res.success(NumberNode(tok))

		elif tok.type == TT_IDENTIFIER: #checks if token is an identifier
			res.register_advancement()
			self.advance()
			return res.success(VarAccessNode(tok))

		elif tok.type == TT_LPAREN: #checks if token is a left parenthesis, handles the expression inside the parenthesis.
			res.register_advancement()
			self.advance()
			expr = res.register(self.expr())
			if res.error: return res
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end, "Expected ')'"))

		return res.failure(InvalidSyntaxError(
      		tok.pos_start, tok.pos_end, "Expected int, identifier, float, '+', '-', or '('"
        )) #returns a failure, because it didn't get an int or float.
  
	def power(self):
		return self.bin_op(self.atom, (TT_POW, ), self.factor)

	def factor(self):
		res = ParseResult()
		tok = self.current_tok #gets the current token

		if tok.type in (TT_POSITIVE, TT_NEGATIVE): #checks for unary character to make number pos or neg.
			res.register_advancement()
			self.advance()
			factor = res.register(self.factor())
			if res.error: return res
			return res.success(UnaryOpNode(tok, factor))

		return self.power()

	def term(self):
		return self.bin_op(self.factor, (TT_MUL, TT_DIV)) #Gets a term by looking for factors, and then the multiplication or division operator.

	def expr(self):
		res = ParseResult()
		
		if self.current_tok.matches(TT_KEYWORD, 'item'): #Checks if the expression is the keyword vars.
			res.register_advancement()
			self.advance()
   
			if self.current_tok.type != TT_IDENTIFIER: #checks if the next token is the identifier, and if not raise an error.
				return res.failure(InvalidSyntaxError(
        			self.current_tok.pos_start, self.current_tok.pos_end, "Expected identifier"
        		))
    
			var_name = self.current_tok #assign the variable to have the name specified by the identifier. 
			res.register_advancement()
			self.advance()
	
			if self.current_tok.type != TT_EQ: #checks if the next token is the equals sign, and if not raise an error.
				return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end, "Expected '='"
				))
	
			res.register_advancement()
			self.advance()
   
			expr = res.register(self.expr()) #assign the variable to have the value specified by the expression.
			if res.error: return res
			return res.success(VarAssignNode(var_name, expr))
   
		
		node = res.register(self.bin_op(self.term, (TT_PLUS, TT_MINUS))) #gets an expression by looking for terms, and then the addition or subtraction operator.
  
		if res.error: 
			return res.failure(InvalidSyntaxError(
       			self.current_tok.pos_start, self.current_tok.pos_end,
          		"Expected 'item', int, float, identifier, '+', '-', or '('"
            )) #if there's an error, return it
		return res.success(node)	#otherwise return the node.

	def bin_op(self, func_a, ops, func_b=None): #Looks for the specified function (either factor or term), and then looks for the specified operators.
		if func_b == None:
			func_b = func_a
   
		res = ParseResult()
		left = res.register(func_a())
		if res.error: return res

		while self.current_tok.type in ops:
			op_tok = self.current_tok
			res.register_advancement()
			self.advance()
			right = res.register(func_b())
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

class Value:
	def __init__(self):
		self.set_pos()
		self.set_context()

	def set_pos(self, pos_start=None, pos_end=None):
		self.pos_start = pos_start
		self.pos_end = pos_end
		return self

	def set_context(self, context=None):
		self.context = context
		return self

	def added_to(self, other):
		return None, self.illegal_operation(other)

	def subbed_by(self, other):
		return None, self.illegal_operation(other)

	def multed_by(self, other):
		return None, self.illegal_operation(other)

	def dived_by(self, other):
		return None, self.illegal_operation(other)

	def powed_by(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_eq(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_ne(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gt(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_lte(self, other):
		return None, self.illegal_operation(other)

	def get_comparison_gte(self, other):
		return None, self.illegal_operation(other)

	def anded_by(self, other):
		return None, self.illegal_operation(other)

	def ored_by(self, other):
		return None, self.illegal_operation(other)

	def notted(self, other):
		return None, self.illegal_operation(other)

	def execute(self, args):
		return RTResult().failure(self.illegal_operation())

	def copy(self):
		raise Exception('No copy method defined')

	def is_true(self):
		return False

	def illegal_operation(self, other=None):
		if not other: other = self
		return RTError(
			self.pos_start, other.pos_end,
			'Illegal operation',
			self.context
		)
#################################################
# Begin number class
#for storying numbers and operating on them as values.
class Number(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value

	def added_to(self, other):
		if isinstance(other, Number):
			return Number(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def subbed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value - other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def multed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def dived_by(self, other):
		if isinstance(other, Number):
			if other.value == 0: #Check for divide by 0 error.
				return None, RTError(
					other.pos_start, other.pos_end,
					'Division by zero',
					self.context
				)

			return Number(self.value / other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def powed_by(self, other):
		if isinstance(other, Number):
			return Number(self.value ** other.value).set_context(self.context), None  #python power operator is **
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_eq(self, other):
		if isinstance(other, Number):
			return Number(int(self.value == other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_ne(self, other):
		if isinstance(other, Number):
			return Number(int(self.value != other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_lt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value < other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_gt(self, other):
		if isinstance(other, Number):
			return Number(int(self.value > other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_lte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value <= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def get_comparison_gte(self, other):
		if isinstance(other, Number):
			return Number(int(self.value >= other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def anded_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value and other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def ored_by(self, other):
		if isinstance(other, Number):
			return Number(int(self.value or other.value)).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def notted(self):
		return Number(1 if self.value == 0 else 0).set_context(self.context), None

	def copy(self):
		copy = Number(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def is_true(self):
		return self.value != 0
	
	def __repr__(self):
		return str(self.value)
####################################
# Begin String class

class String(Value):
	def __init__(self, value):
		super().__init__()
		self.value = value

	def added_to(self, other):
		if isinstance(other, String):
			return String(self.value + other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def multed_by(self, other):
		if isinstance(other, Number):
			return String(self.value * other.value).set_context(self.context), None
		else:
			return None, Value.illegal_operation(self, other)

	def is_true(self):
		return len(self.value) > 0

	def copy(self):
		copy = String(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy

	def __repr__(self):
		return f'"{self.value}"'

	#################
12. # Context Class #
	#################

class Context:
	def __init__(self, display_name, parent=None, parent_entry_pos=None):
		self.display_name = display_name
		self.parent = parent
		self.parent_entry_pos = parent_entry_pos
		self.symbol_table = None

   #####################
13. # Symbol table Class #
   #####################
   #To keep track of the avariable names and their values
   
class SymbolTable:
	def __init__(self):
		self.symbols = {}
		self.parent = None
  
	def get(self, name): #for getting the value of a variable
		value = self.symbols.get(name, None) #gets a value from the symbol table
		if value == None and self.parent: #checks if the value is empty. if so it needs to check it's parent., and then goes checks if it has a parent symbol table and returns it if it has one.
			return self.parent.get(name)
		return value #if it just found a vlaue though just return the value.

	def set(self, name, value): #for setting the value of a variable
		self.symbols[name] = value
  
	def remove(self, name): #for removing a variable
		del self.symbols[name]
  
  
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
            
	def visit_StringNode(self, node, context): # this node is for a string
			return RTResult().success(
				String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
			)
  
	def visit_VarAccessNode(self, node, context): #this node is for accessing a variable
		res = RTResult()
		var_name = node.var_name_tok.value
		value = context.symbol_table.get(var_name)
  
		if not value: #for in case there wasn't a value, meaning the variable wasn't defined.
					return res.failure(RTError(
						node.pos_start, node.pos_end,
						f"'{var_name}' is not defined",
						context
					))
     
		value = value.copy().set_pos(node.pos_start, node.pos_end)
		return res.success(value)
  
	def visit_VarAssignNode(self, node, context):
		res = RTResult()
		var_name = node.var_name_tok.value
		value = res.register(self.visit(node.value_node, context)) #for assining the variable.
		if res.error: return res #if there's an error, return it.
  
		context.symbol_table.set(var_name, value) #sets the variable to the value.
		return res.success(value)


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
		elif node.op_tok.type == TT_POW: #if it's a divide, divide them
			result, error = left.powed_by(right)

		if error:
			return res.failure(error)
		else:
			return res.success(result.set_pos(node.pos_start, node.pos_end))

	def visit_UnaryOpNode(self, node, context): #this node is for a unary operation
		res = RTResult()
		number = res.register(self.visit(node.node, context))
		if res.error: return res

		error = None

		if node.op_tok.type == TT_NEGATIVE: #if it's a minus, make the number negative by multiplying it by -1.
			number, error = number.multed_by(Number(-1))

		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))

   #################
4. # Run / Execute #
   #################

global_symbol_table = SymbolTable() #makes the global symbol table.
global_symbol_table.set("null", Number(0)) #sets the value of NULL to 0 for test purposes.

def run(fn, text): #takes in text and runs it.
	# Generate tokens
	lexer = Lexer(fn, text) #makes a new lexer and gives it the code file
	tokens, error = lexer.make_tokens() #makes the tokens from the code and gets them, along with an error if there is one.
	if error: return None, error # if there's an error, stops the program and returns the error.
	#print(tokens)
 
	# Generate AST (abstract syntax tree)
	parser = Parser(tokens)
	ast = parser.parse()
	if ast.error: return None, ast.error

	# Run program
	interpreter = Interpreter()
	context = Context('<program>')
	context.symbol_table = global_symbol_table #sets the symbol table to the global symbol table.
	result = interpreter.visit(ast.node, context)

	return result.value, result.error
