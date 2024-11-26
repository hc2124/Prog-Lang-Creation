import scholarlyLexer
import scholarlyExecute
import scholarlyParser

if __name__ == '__main__': 
	lexer = scholarlyLexer() 
	parser = scholarlyParser() 
	print('Scholarly Language') 
	env = {} 
	
	while True: 
		
		try: 
			text = input('Scholarly Language > ') 
		
		except EOFError: 
			break
		
		if text: 
			tree = parser.parse(lexer.tokenize(text)) 
			scholarlyExecute(tree, env)