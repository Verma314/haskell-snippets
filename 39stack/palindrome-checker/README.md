# palindrome-checker

## Basics, 

Stack is a powerful build tool. What it does:
1. Provides an isolated installation of GHC
2. Handles installation of packages and dependencies.
3. Automates building of the project.
4. Helps with organizing and running tests.


To create a new project, ```stack new project-name```

it creates a new project in a new directory for you,
- ```the project-name.cabal``` is an important file, it contains all the project configurations. i.e all metadata related to the project.

- in the cabal file, under ```library```, under ```hs-source-dirs```, is the value for the directory where the library files live, it is ```src``` by default.
- ```exposed-modules``` in the cabal file tells us which libraries  we are using. By default stack creates a module called 'Lib' under src. We gotta add our more values to it (in the cabal file, and in the directory) by using commas
```
exposed-modules:    Lib,
                    Palindrome,
                    Utils
```

- in the cabal file, under ```executable```, this lists out where the "executable" code is stored, i.e the 'Main'. Stack separates the logic (in Lib) from the code that is used to invoke that logic (Main.hs in ```App```)
- Stack has the opinion that the main will import the Lib, and execute its functions.




-