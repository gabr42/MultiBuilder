# MultiBuilder

*MultiBuilder* is a simple tool that allows you to run a set of programs on a different configurations (called *environments* in the program) at a same time. Results from programs that fail execution are presented for inspection.

Currently, *MultiBuilder* can be compiled for Windows. 

If you are in a hurry and just need compiled binary, you can grab it [here](https://drive.google.com/open?id=1b9MSuDiQ3nq3JF60vY0Z6CpEkCkCHZyn).

# Configuration

Execution environment consists of two parts - a configuration file defining all execution *environments* (consisting mostly of *variable definitions*) and a configuration file defining the *project* (mostly describing programs that should be executed). It is possible to use one generic environment configuration with different project configurations.

## Environment

Environment definitions are stored in a INI-style configuration file with extension `.mbenv`. Default environment configuration file is stored in `C:\Users\Public\Documents\multibuilder.mbenv`. You can use a different configuration file by providing its name as a command-line parameter or by clicking the *Load Environment* button. *Edit Environment* button opens up a simple text editor where you can modify the configuration file.

Environment configuraton file is split into *sections*. Besides the special section named *Global* you can define any number of sections, each describing its own environment. The following example shows environment file used for testing [OmniThreadLibrary](http://www.omnithreadlibrary.com).

[Global]
Scratch=c:\0\MultiBuilder\$(EnvironmentName)
ForceDir=$(Scratch)\exe;$(Scratch)\dcu;$(Scratch)\dcu\win32;$(Scratch)\dcu\win64

[Delphi 2007]
Path=d:\Delphi\5.0\bin

[Delphi 2009]
Path=d:\Delphi\6.0\bin

[Delphi 2010]                     *
Path=d:\Delphi\7.0\bin

[Delphi XE]
Path=d:\Delphi\8.0\bin

[Delphi XE2]
Path=d:\Delphi\9.0\bin

[Delphi XE3]
Path=d:\Delphi\10.0\bin

[Delphi XE4]
Path=d:\Delphi\11.0\bin

[Delphi XE5]
Path=d:\Delphi\12.0\bin

[Delphi XE6]
Path=d:\Delphi\14.0\bin

[Delphi XE7]
Path=d:\Delphi\15.0\bin

[Delphi XE8]
Path=d:\Delphi\16.0\bin

[Delphi 10 Seattle]
Path=d:\Delphi\17.0\bin

[Delphi 10.1 Berlin]
Path=d:\Delphi\18.0\bin

[Delphi 10.2 Tokyo]
Path=d:\Delphi\19.0\bin

OmniThreadLibrary supports Delphi compilers from version *2007* to *10.2 Tokyo* and this configuration file reflects that. Each environment section defines *variable* `Path`. We'll see in the section *Project* below how this variable is used in practice. 

Special section *Global* defines variables that are defined for all environments. In this example it defines variable *Scratch* that is used in the project file and special *directive* *ForceDir* specifying folder that *MultiBuilder* should create before the project is executed. (For more information see *Running projects* below.) 

Special macro *$(EnvironmentName)* always expands to the name of *environment* settings used to run the project. In the example above, this macro could expand to *Delphi 2007*, *Delphi XE5*, *Delphi 10.1 Berlin*, and so on.

## Project

Project definition is stored in a INI-style configuration file with extension `.mbproj`. There is no default project configuration file. You can provide it via the command-line parameter or by clicking the *Load Project* button. *Edit Project* button opens up a simple text editor where you can modify the project file.

Project configuraton file is split into *sections*. Special section *Global* defines commands that are always executed. Sections with names corresponding to *environment* names contain commands that are only executed in that environment. If a section for specific environment doesn't exist in the project file, commands from the special *Default* section will be used. (For more information see *Running projects* below.)  
 
The following example shows project file used for unit-testing [OmniThreadLibrary](http://www.omnithreadlibrary.com).

[Global]
Folder=h:\razvoj\omnithreadlibrary\unittests
Cmd=$(Path)\dcc32.exe CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e"$(Scratch)\exe" -n0"$(Scratch)\dcu\win32" -dCONSOLE_TESTRUNNER
Cmd=$(Path)\dcc32.exe TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e"$(Scratch)\exe" -n0"$(Scratch)\dcu\win32" -dCONSOLE_TESTRUNNER
Cmd="$(Scratch)\exe\TestRunner.exe"

[Default]
Cmd=$(Path)\dcc64.exe CompileAllUnits -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e"$(Scratch)\exe" -n0"$(Scratch)\dcu\win64" -dCONSOLE_TESTRUNNER
Cmd=$(Path)\dcc64.exe TestRunner -b -u..;..\src;..\..\fastmm -i.. -nsSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Samples;Data;Xml -e"$(Scratch)\exe" -n0"$(Scratch)\dcu\win64" -dCONSOLE_TESTRUNNER
Cmd="$(Scratch)\exe\TestRunner.exe"

[Delphi 2007]
Null=

[Delphi 2009]
Null=

[Delphi 2010]
Null=

[Delphi XE]
Null=

Section *Global* specifies a *folder* which is used as a *working directory* for each command. Commands are all introduced with key *Cmd*. All other keys are ignored.

# Running projects

If you click the *Run All* button or press the `F9` key, the current project is started in all environments.

If an environment is selected and you click the *Run Selected* button or press the `Alt+F9` key combo, the project is started in selected environment.

Each environment executes the project in its own thread. Therefore you can select one project, click *Run Selected*, repeat that with another projects, and both will be executed in parallel.  

In this example, environment *Delphi 2007* would execute all commands from the *Global* section. As there exists a *Delphi 2007* section, commands from the *Default* section are ignored. *Delphi 2007* section contains just one setting with key *Null*, which is ignores. (At least one setting must be stored in a section, otherwise the information about that section is lost when configuration file is loaded.)  

Environment 

Multibuilder.exe SmokeTest.mbenv SmokeTest.mbproj 