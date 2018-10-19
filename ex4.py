#defines integer class variable cars, which makes sense since
#one rarely encounters a fraction of a car that still functions
cars  = 100

#space in a car represents the number of spaces available in a car, presumably
#not including the driver. This makes sense as a floating point since
#it could probably be evaluated on average because of two-seaters, sedans,
#mini-vans etc
space_in_a_car = 4.0
#space_in_a_car = 4

#drivers is defined as an integer since there must be a discrete number of
#drivers
drivers = 30

#passengers is defined as an integer since there must be a discrete number of
#passengers
passengers = 90

#cars_not_driven is an integer since it is the difference of two integers
#cars and drivers which makes sense since only a discrete number of cars_driven
#may not be driven
cars_not_driven = cars - drivers

#cars_driven is an integer since it is defined to be equal to an integer
#which makes sense since only a discrete number of cars may be driven
cars_driven = drivers

#carpool capacity is a floating point since it is the product of an integer
# and a floating point, which makes sense since space_in_a_car is defined
# on the average
carpool_capacity = cars_driven*space_in_a_car

#average_passengers_per_car = is a floating point since passengers is a known discrete
#integer being divided by another known discrete integer using conventional divison
#which will not necessarily result in another discrete integer
average_passengers_per_car = passengers/cars_driven

print("There are", cars, "cars available.")

print("There are only", drivers, "drivers available.")

print("There will be", cars_not_driven, "empty cars today.")

print("We can transport", carpool_capacity, "people today.")

print("We have",passengers,"to carpool today.")

print("We need to put about", average_passengers_per_car,"in each car")

#Study Drill 1
#The author used car_pool_capacity as a variable, but had
#named the variable carpool_capacity in line 7, i.e. there was
#no underscore between "car" and "pool". Thus the computer
#had no knowledge of car_pool_capacity and thus could not
#process the equation in line 8.

#Study drill 2
#The floating point seems unnecessary, the output doesn't change,
#but it may become necessary for other calculations that need to be
#integers rather than floating points/doubles

#study drill 3 (see comments)

#study drill 4
# equals sign, singular, just assigns a value to a variable for integers, floating
# doubles, strings etcself.

#study drill 5 (understood)

#study drill 6 (see code part 2)
