#Shell program that you run to start running your scholar programs.

import ScholarLang

while True:
		text = input('scholarLang > ')
		result, error = ScholarLang.run('<stdin>', text)

		if error: print(error.as_string())
		else: print(result)
