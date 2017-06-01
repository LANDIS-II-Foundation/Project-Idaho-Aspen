rem Scenario is %1
rem Replicate number is %2

set workingdir=I:\NWCSC-Idaho\LANDIS_modeling\RCEW
set homedir=I:\NWCSC-Idaho\LANDIS_modeling\RCEW

if not exist %workingdir%\%1\replicate%2 mkdir %workingdir%\%1\replicate%2
i:
cd %workingdir%\%1\replicate%2
copy I:\NWCSC-Idaho\LANDIS_modeling\RCEW\%1.txt
call landis %1.txt
i:
cd %homedir%

