import basicLang

while True:
		text = input('yay > ')
		result, error = basicLang.run('<stdin>', text)

		if error: print(error.as_string())
		else: print(result)