print("Hello, my name is calculator")
num1 = input("Please enter a number   ")
num1 = float(num1)

print("Cool, Cool, how about another number?")

num2 = input("Please enter another number    ")
num2 = float(num2)

op = input("Please select add, subtract, multiply, divide, for second number to act on first  ")
op = str(op)

if( op == "add"):
    print(num1 + num2)
else:
    if(op == "subtract"):
        print(num1 - num2)
    else:
        if(op == "multiply"):
            print(num1*num2)
        else:
            if(op == "divide"):
                print(num1/num2)
            else:
                print("I'm sorry, I don't understand your request")
