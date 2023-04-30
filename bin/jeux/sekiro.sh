#!/usr/bin/env bash

export WINEPREFIX=~/Jeux/epic-games-store/
export AMD_DEBUG=nodmacopyimage
export DXVK_HUD=compiler
export MESA_GL_VERSION_OVERRIDE=4.4COMPAT
export PROTON_USE_WINED3D=1
export __GL_SHADER_DISK_CACHE=1
export __GL_SHADER_DISK_CACHE_PATH=$WINEPREFIX
export STEAM_COMPAT_DATA_PATH=~/.steam/steam/steamapps/compatdata/

steam-run /home/sirc/.steam/root/compatibilitytools.d/Proton-5.8-GE-2-MF/proton run \
  /home/sirc/Jeux/epic-games-store/drive_c/Program\ Files\ \(x86\)/Epic\ Games/Launcher/Portal/Binaries/Win32/EpicGamesLauncher.exe -opengl -SkipBuildPatchPrereq
  #/home/sirc/Jeux/epic-games-store/drive_c/Program\ Files/Epic\ Games/SatisfactoryExperimental/FactoryGame.exe -FULLSCREEN -NOSPLASH
