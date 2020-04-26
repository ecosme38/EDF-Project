# Readme_python_Mordor

Create python library to use MORDOR-SD

2 libraries must be created :
-one tu use Litre function (able to read parameters files)
-another to use MORDOR-SD

For each one, a .so file will be made with the f2py tool, and can be import in python.

For Litre :
f2py -c litres.f95 -m litres

For Mordor-SD :
f2py -c MORDOR_INIT_static.f95 MordorSD_dll.f95 MORDOR_SD_OM_static.f95 -m mordor_sd_assim

After that, litres.so and mordor_sd_assim.so files are created.
To use them, they must be in the same directory as the python scripts.
