# Pawno
Simple and lightweight Pawn script editor, for replacing the old official version of Pawno

#### Differences between this and original Pawno
- It's open source
- It has a 'Redo' button
- More Pawn language like highlighting instead of C++
- It associate `*.p` and `*.pawn` files, not just only `*.pwn` files
- And maybe more!

*Some features like function list or PAWN file association is requires admin privileges, and it can be done by right clicking on the EXE and select **Properties**, select **Compatibility** tab and check **Run this program as an administrator**, and click **OK**.*

![NewVsOld](https://github.com/dashr9230/Pawno/blob/master/new_and_old.png "New vs. old original Pawno")

#### How to compile
1.) Download Lazarus 2.2.0 (or newer, if available) version from [SourceForge](https://sourceforge.net/projects/lazarus/files/). 
You can choose either Windows 32 bits or 64 bits version. In the same folder, you also need to download the cross add-on, except the cross-arm-wince installer. If you not install the cross compiler add-on, building the source may fail, depending on what build mode you choose on what Lazarus version.
2.) Install Lazarus and the Add-on, and make sure both is on full installation
3.) Download Pawno source by clicking **Clone or download** button and select **Download ZIP**
4.) Extract the downloaded ZIP file wherever you want
5.) In the `\src` folder, open **pawno.lpi**, and wait until Lazarus initializes 
6.) On top, click on **Run** and select **Compile many Modes...** and uncheck all, except **Release (32-bit)** and/or **Release (64-bit)**, then press **OK**
7.) After the build was successfull, the `Pawno.exe` can be found where was a source extracted, inside `\src` folder
8.) ???
9.) Profit!
