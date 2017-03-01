#!/bin/sh

nuget install ILRepack -Version 2.0.12 -OutputDirectory packages

mono ./packages/ILRepack.2.0.12/tools/ILRepack.exe \
    /target:library \
    /targetplatform:v4 \
    /out:./Artifacts/Debug/Package/lib/Shen.dll \
    ./Artifacts/Debug/Shen.Runtime.dll \
    ./Kl/bin/Debug/Kl.dll

mono ./packages/ILRepack.2.0.12/tools/ILRepack.exe \
    /target:exe \
    /targetplatform:v4 \
    /allowDup:ShenSharp.Shared \
    /allowDup:ShenSharp.Metadata \
    /out:./Artifacts/Debug/Package/tools/Shen.exe \
    ./Shen.Repl/bin/Debug/Shen.Repl.exe \
    ./Kl/bin/Debug/Kl.dll \
    ./Artifacts/Debug/Shen.Runtime.dll
