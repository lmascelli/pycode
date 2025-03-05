import builtins

# Personally i prefer to manage the memory by myself and i'm not always Sure
# about how Python pass or share arguments if by copy or by reference. So i've
# allocated a global variable called 'PyCodeData'. It is a dictionary which
# stores all the information shared between the various widgets and the State of
# the program. The 'PyCodeData' variable is not meant to be accessed directly
# but by the use of the Memory class in the memory module that contains the
# static methods used to access and modify the dictionary values in a checked
# manner.
PyCodeData = {
    "PHASES": {},
    "MainWindow": None,
    "Tabs": {},
}

setattr(builtins, "PyCodeData", PyCodeData)
