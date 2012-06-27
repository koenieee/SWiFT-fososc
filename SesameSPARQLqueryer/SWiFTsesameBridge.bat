::Note:  Chide checked that the working directory of this bat when called is $SUBLEMEHOME\subleme.  From that place the right cd is made.
cd ..\sesame\SesameSWiFTbridge

:: If you want to check whether the current working directory is INDEED correct:
::echo %CD% 

:: And do the job!
java -classpath .;..\openrdf-sesame-2.1.3-onejar.jar;..\slf4j-api-1.5.0.jar;SWiFTsesameBridge;..\slf4j-jdk14-1.5.0.jar SWiFTsesameBridge