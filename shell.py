import basicLang

while True:
		text = input('scholarLang > ')
		result, error = basicLang.run('<stdin>', text)

		if error: print(error.as_string())
		else: print(result)
