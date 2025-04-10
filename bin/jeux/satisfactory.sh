#!/usr/bin/env bash
# Setup : "export WINEPREFIX=<dir>; \
#          export WINEARCH=win64; winecfg; winetricks wininet winhttp dotnet46 vcrun2005 vcrun2008 vcrun2015 dxvk mf vcrun2017; return"
# Add to drive_c/users/<user>/Local Settings/Application Data/FactoryGame/Saved/Config/WindowsNoEditor:
# [/script/windowstargetplatform.windowstargetsettings]
# DefaultGraphicsRHI=DefaultGraphicsRHI_Vulkan
#
# [SystemSettings]
# r.Vulkan.EnableDefrag=0

export BASEPATH=/home/sirc/Jeux/Satisfactory
export TMP=/home/sirc/tmp
export CACHEPATH=$BASEPATH/.cache
export WINEPREFIX=$BASEPATH/.wine
export DXVK_STATE_CACHE_PATH=$CACHEPATH/dxvk
export __GL_SHADER_DISK_CACHE=1
export __GL_SHADER_DISK_CACHE_PATH=$CACHEPATH/opengl
export mesa_glthread=true

wine64 $BASEPATH/SatisfactoryExperimental/FactoryGame.exe -FULLSCREEN -NOSPLASH -vulkan \
    >$TMP/satisfactory.log \
    2>$TMP/satisfactory.err.log

