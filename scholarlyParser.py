from sly import Parser

import scholarlyLexer

class scholarlyParser(Parser): 
	#tokens are passed from lexer to parser 
	tokens = scholarlyLexer.tokens 

	precedence = ( 
		('left', '+', '-'), 
		('left', '*', '/'), 
		('right', 'UMINUS'), 
	) 

	def __init__(self): 
		self.env = { } 

	@('') 
	def statement(self, p): 
		pass

	@('var_assign') 
	def statement(self, p): 
		return p.var_assign 

	@('NAME "=" expr') 
	def var_assign(self, p): 
		return ('var_assign', p.NAME, p.expr) 

	@('NAME "=" STRING') 
	def var_assign(self, p): 
		return ('var_assign', p.NAME, p.STRING) 

	@('expr') 
	def statement(self, p): 
		return (p.expr) 

	@('expr "+" expr') 
	def expr(self, p): 
		return ('add', p.expr0, p.expr1) 

	@('expr "-" expr') 
	def expr(self, p): 
		return ('sub', p.expr0, p.expr1) 

	@('expr "*" expr') 
	def expr(self, p): 
		return ('mul', p.expr0, p.expr1) 

	@('expr "/" expr') 
	def expr(self, p): 
		return ('div', p.expr0, p.expr1) 

	@('"-" expr %prec UMINUS') 
	def expr(self, p): 
		return p.expr 

	@('NAME') 
	def expr(self, p): 
		return ('var', p.NAME) 

	@('NUMBER') 
	def expr(self, p): 
		return ('num', p.NUMBER)
