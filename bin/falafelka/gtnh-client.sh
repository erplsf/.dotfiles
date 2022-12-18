#!/bin/sh

case $1 in
     "create")
        /home/komrad/.config/gdlauncher_next/java/8u242-b08/bin/java -Djava.library.path="/home/komrad/.config/gdlauncher_next/instances/Create  Above and Beyond/natives" -Dminecraft.launcher.brand=GDLauncher -Dminecraft.launcher.version=1.0 -cp /home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/forge/1.16.5-36.2.20/forge-1.16.5-36.2.20.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/ow2/asm/asm/9.1/asm-9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/ow2/asm/asm-commons/9.1/asm-commons-9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/ow2/asm/asm-tree/9.1/asm-tree-9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/ow2/asm/asm-util/9.1/asm-util-9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/ow2/asm/asm-analysis/9.1/asm-analysis-9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/cpw/mods/modlauncher/8.0.9/modlauncher-8.0.9.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/cpw/mods/grossjava9hacks/1.3.3/grossjava9hacks-1.3.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/accesstransformers/3.0.1/accesstransformers-3.0.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/antlr/antlr4-runtime/4.9.1/antlr4-runtime-4.9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/eventbus/4.0.0/eventbus-4.0.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/forgespi/3.2.0/forgespi-3.2.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/coremods/4.0.6/coremods-4.0.6.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/unsafe/0.2.0/unsafe-0.2.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/electronwill/night-config/core/3.6.3/core-3.6.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/electronwill/night-config/toml/3.6.3/toml-3.6.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/jline/jline/3.12.1/jline-3.12.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/maven/maven-artifact/3.6.3/maven-artifact-3.6.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/jodah/typetools/0.8.3/typetools-0.8.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecrell/terminalconsoleappender/1.2.0/terminalconsoleappender-1.2.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/sf/jopt-simple/jopt-simple/5.0.4/jopt-simple-5.0.4.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/spongepowered/mixin/0.8.4/mixin-0.8.4.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/nashorn-core-compat/15.1.1.1/nashorn-core-compat-15.1.1.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/patchy/1.1/patchy-1.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/oshi-project/oshi-core/1.1/oshi-core-1.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/dev/jna/jna/4.4.0/jna-4.4.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/dev/jna/platform/3.4.0/platform-3.4.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/ibm/icu/icu4j/66.1/icu4j-66.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/javabridge/1.0.22/javabridge-1.0.22.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/sf/jopt-simple/jopt-simple/5.0.3/jopt-simple-5.0.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/io/netty/netty-all/4.1.25.Final/netty-all-4.1.25.Final.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/google/guava/guava/21.0/guava-21.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/commons/commons-lang3/3.5/commons-lang3-3.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-io/commons-io/2.5/commons-io-2.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-codec/commons-codec/1.10/commons-codec-1.10.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/jinput/jinput/2.0.5/jinput-2.0.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/jutils/jutils/1.0.0/jutils-1.0.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/brigadier/1.0.17/brigadier-1.0.17.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/datafixerupper/4.0.26/datafixerupper-4.0.26.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/google/code/gson/gson/2.8.0/gson-2.8.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/authlib/2.1.28/authlib-2.1.28.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/commons/commons-compress/1.8.1/commons-compress-1.8.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/httpcomponents/httpclient/4.3.3/httpclient-4.3.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-logging/commons-logging/1.1.3/commons-logging-1.1.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/httpcomponents/httpcore/4.3.2/httpcore-4.3.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/it/unimi/dsi/fastutil/8.2.1/fastutil-8.2.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/logging/log4j/log4j-api/2.15.0/log4j-api-2.15.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/logging/log4j/log4j-core/2.15.0/log4j-core-2.15.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl/3.2.2/lwjgl-3.2.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl-jemalloc/3.2.2/lwjgl-jemalloc-3.2.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl-openal/3.2.2/lwjgl-openal-3.2.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl-opengl/3.2.2/lwjgl-opengl-3.2.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl-glfw/3.2.2/lwjgl-glfw-3.2.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl-stb/3.2.2/lwjgl-stb-3.2.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl-tinyfd/3.2.2/lwjgl-tinyfd-3.2.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/text2speech/1.11.3/text2speech-1.11.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraft/1.16.5.jar -Xmx8192m -Xms8192m -Dminecraft.applet.TargetDirectory="/home/komrad/.config/gdlauncher_next/instances/Create  Above and Beyond" -Dlog4j.configurationFile=/home/komrad/.config/gdlauncher_next/datastore/assets/objects/ef/client-1.12.xml -Dfml.ignorePatchDiscrepancies=true -Dfml.ignoreInvalidMinecraftCertificates=true -Xms256m -XX:+IgnoreUnrecognizedVMOptions --add-exports=java.base/sun.security.util=ALL-UNNAMED --add-exports=jdk.naming.dns/com.sun.jndi.dns=java.naming --add-opens=java.base/java.util.jar=ALL-UNNAMED cpw.mods.modlauncher.Launcher --username erp_lsf --version 1.16.5 --gameDir "/home/komrad/.config/gdlauncher_next/instances/Create  Above and Beyond" --assetsDir /home/komrad/.config/gdlauncher_next/datastore/assets --assetIndex 1.16 --uuid ee5cd7dbbd70454ab2d1f2941b2fb965 --accessToken eyJhbGciOiJIUzI1NiJ9.eyJhZ2ciOiJBZHVsdCIsInN1YiI6ImZkMzRjNzJhZjE2Y2RmYjdkODA4Y2Q4MWQ4YmFjYTA5IiwieWdndCI6ImNmMzMwOWJlMzkxYzQ5MmM5MGI1MzI1YzNhMDI0OTU0Iiwic3ByIjoiZWU1Y2Q3ZGJiZDcwNDU0YWIyZDFmMjk0MWIyZmI5NjUiLCJpc3MiOiJZZ2dkcmFzaWwtQXV0aCIsImV4cCI6MTY0MTUwMDUwMSwiaWF0IjoxNjQxMzI3NzAxfQ.aoUuleHSjz4GAX585h0deCT1k4cwNiCHZMWpT8gxvR8 --userType mojang --versionType release --launchTarget fmlclient --fml.forgeVersion 36.2.20 --fml.mcVersion 1.16.5 --fml.forgeGroup net.minecraftforge --fml.mcpVersion 20210115.111550 --width 854 --height 480
        ;;
     "omnifactory")
        cd /home/komrad/.config/gdlauncher_next/instances/Omnifactory && /usr/lib/jvm/java-8-jre/jre/bin/java -cp /home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/forge/1.12.2-14.23.5.2855/forge-1.12.2-14.23.5.2855.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/ow2/asm/asm-debug-all/5.2/asm-debug-all-5.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraft/launchwrapper/1.12/launchwrapper-1.12.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/jline/jline/3.5.1/jline-3.5.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/typesafe/akka/akka-actor_2.11/2.3.3/akka-actor_2.11-2.3.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/typesafe/config/1.2.1/config-1.2.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-actors-migration_2.11/1.1.0/scala-actors-migration_2.11-1.1.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-compiler/2.11.1/scala-compiler-2.11.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/plugins/scala-continuations-library_2.11/1.0.2_mc/scala-continuations-library_2.11-1.0.2_mc.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/plugins/scala-continuations-plugin_2.11.1/1.0.2_mc/scala-continuations-plugin_2.11.1-1.0.2_mc.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-library/2.11.1/scala-library-2.11.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-parser-combinators_2.11/1.0.1/scala-parser-combinators_2.11-1.0.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-reflect/2.11.1/scala-reflect-2.11.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-swing_2.11/1.0.1/scala-swing_2.11-1.0.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-xml_2.11/1.0.2/scala-xml_2.11-1.0.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/lzma/lzma/0.0.1/lzma-0.0.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/java3d/vecmath/1.5.2/vecmath-1.5.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/sf/trove4j/trove4j/3.0.3/trove4j-3.0.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/maven/maven-artifact/3.5.3/maven-artifact-3.5.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/sf/jopt-simple/jopt-simple/5.0.3/jopt-simple-5.0.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/patchy/1.1/patchy-1.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/oshi-project/oshi-core/1.1/oshi-core-1.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/dev/jna/jna/4.4.0/jna-4.4.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/dev/jna/platform/3.4.0/platform-3.4.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/ibm/icu/icu4j-core-mojang/51.2/icu4j-core-mojang-51.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/codecjorbis/20101023/codecjorbis-20101023.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/codecwav/20101023/codecwav-20101023.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/libraryjavasound/20101123/libraryjavasound-20101123.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/librarylwjglopenal/20100824/librarylwjglopenal-20100824.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/soundsystem/20120107/soundsystem-20120107.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/io/netty/netty-all/4.1.9.Final/netty-all-4.1.9.Final.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/google/guava/guava/21.0/guava-21.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/commons/commons-lang3/3.5/commons-lang3-3.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-io/commons-io/2.5/commons-io-2.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-codec/commons-codec/1.10/commons-codec-1.10.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/jinput/jinput/2.0.5/jinput-2.0.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/jutils/jutils/1.0.0/jutils-1.0.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/google/code/gson/gson/2.8.0/gson-2.8.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/authlib/1.5.25/authlib-1.5.25.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/realms/1.10.22/realms-1.10.22.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/commons/commons-compress/1.8.1/commons-compress-1.8.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/httpcomponents/httpclient/4.3.3/httpclient-4.3.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-logging/commons-logging/1.1.3/commons-logging-1.1.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/httpcomponents/httpcore/4.3.2/httpcore-4.3.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/it/unimi/dsi/fastutil/7.1.0/fastutil-7.1.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/logging/log4j/log4j-api/2.8.1/log4j-api-2.8.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/logging/log4j/log4j-core/2.8.1/log4j-core-2.8.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl/lwjgl/2.9.4-nightly-20150209/lwjgl-2.9.4-nightly-20150209.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl/lwjgl_util/2.9.4-nightly-20150209/lwjgl_util-2.9.4-nightly-20150209.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl/lwjgl-platform/2.9.4-nightly-20150209/lwjgl-platform-2.9.4-nightly-20150209.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/text2speech/1.10.3/text2speech-1.10.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraft/1.12.2.jar -Xmx4096m -Xms4096m -Dfml.ignorePatchDiscrepancies=true -Dfml.ignoreInvalidMinecraftCertificates=true -Xms256m -Djava.library.path=/home/komrad/.config/gdlauncher_next/instances/Omnifactory/natives -Dminecraft.applet.TargetDirectory=/home/komrad/.config/gdlauncher_next/instances/Omnifactory net.minecraft.launchwrapper.Launch --username erp_lsf --version 1.12.2 --gameDir /home/komrad/.config/gdlauncher_next/instances/Omnifactory --assetsDir /home/komrad/.config/gdlauncher_next/datastore/assets --assetIndex 1.12 --uuid ee5cd7dbbd70454ab2d1f2941b2fb965 --accessToken eyJhbGciOiJIUzI1NiJ9.eyJhZ2ciOiJBZHVsdCIsInN1YiI6ImZkMzRjNzJhZjE2Y2RmYjdkODA4Y2Q4MWQ4YmFjYTA5IiwieWdndCI6IjAxM2RlYzgwMjg5NzRlOGFhNGVkNGQ2MmZjN2E3YzNmIiwic3ByIjoiZWU1Y2Q3ZGJiZDcwNDU0YWIyZDFmMjk0MWIyZmI5NjUiLCJpc3MiOiJZZ2dkcmFzaWwtQXV0aCIsImV4cCI6MTYzNjgyODI4MywiaWF0IjoxNjM2NjU1NDgzfQ.nw6nw2zkgsFUpybHim32k4RqKr9zZFwuLvzzACXOjFc --userType mojang --tweakClass net.minecraftforge.fml.common.launcher.FMLTweaker --versionType Forge --width 854 --height 480
        ;;
     "gtnh")
        /home/komrad/.config/gdlauncher_next/java/8u242-b08/bin/java -cp /home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraftforge/forge/1.7.10-10.13.4.1614-1.7.10/forge-1.7.10-10.13.4.1614-1.7.10.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraft/launchwrapper/1.12/launchwrapper-1.12.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/ow2/asm/asm-all/5.0.3/asm-all-5.0.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/typesafe/akka/akka-actor_2.11/2.3.3/akka-actor_2.11-2.3.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/typesafe/config/1.2.1/config-1.2.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-actors-migration_2.11/1.1.0/scala-actors-migration_2.11-1.1.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-compiler/2.11.1/scala-compiler-2.11.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/plugins/scala-continuations-library_2.11/1.0.2/scala-continuations-library_2.11-1.0.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/plugins/scala-continuations-plugin_2.11.1/1.0.2/scala-continuations-plugin_2.11.1-1.0.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-library/2.11.1/scala-library-2.11.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-parser-combinators_2.11/1.0.1/scala-parser-combinators_2.11-1.0.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-reflect/2.11.1/scala-reflect-2.11.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-swing_2.11/1.0.1/scala-swing_2.11-1.0.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/scala-lang/scala-xml_2.11/1.0.2/scala-xml_2.11-1.0.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/lzma/lzma/0.0.1/lzma-0.0.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/sf/jopt-simple/jopt-simple/4.5/jopt-simple-4.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/google/guava/guava/17.0/guava-17.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/commons/commons-lang3/3.3.2/commons-lang3-3.3.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/netty/1.6/netty-1.6.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/realms/1.3.5/realms-1.3.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/commons/commons-compress/1.8.1/commons-compress-1.8.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/httpcomponents/httpclient/4.3.3/httpclient-4.3.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-logging/commons-logging/1.1.3/commons-logging-1.1.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/httpcomponents/httpcore/4.3.2/httpcore-4.3.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/java3d/vecmath/1.3.1/vecmath-1.3.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/sf/trove4j/trove4j/3.0.3/trove4j-3.0.3.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/ibm/icu/icu4j-core-mojang/51.2/icu4j-core-mojang-51.2.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/codecjorbis/20101023/codecjorbis-20101023.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/codecwav/20101023/codecwav-20101023.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/libraryjavasound/20101123/libraryjavasound-20101123.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/librarylwjglopenal/20100824/librarylwjglopenal-20100824.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/paulscode/soundsystem/20120107/soundsystem-20120107.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/io/netty/netty-all/4.0.10.Final/netty-all-4.0.10.Final.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/google/guava/guava/15.0/guava-15.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/commons/commons-lang3/3.1/commons-lang3-3.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-io/commons-io/2.4/commons-io-2.4.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/commons-codec/commons-codec/1.9/commons-codec-1.9.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/jinput/jinput/2.0.5/jinput-2.0.5.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/java/jutils/jutils/1.0.0/jutils-1.0.0.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/google/code/gson/gson/2.2.4/gson-2.2.4.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/com/mojang/authlib/1.5.21/authlib-1.5.21.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/logging/log4j/log4j-api/2.0-beta9-fixed/log4j-api-2.0-beta9-fixed.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/apache/logging/log4j/log4j-core/2.0-beta9-fixed/log4j-core-2.0-beta9-fixed.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl/lwjgl/2.9.1/lwjgl-2.9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/org/lwjgl/lwjgl/lwjgl_util/2.9.1/lwjgl_util-2.9.1.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/tv/twitch/twitch/5.16/twitch-5.16.jar:/home/komrad/.config/gdlauncher_next/datastore/libraries/net/minecraft/1.7.10.jar -Xmx8192m -Xms8192m -Dfml.ignorePatchDiscrepancies=true -Dfml.ignoreInvalidMinecraftCertificates=true -Xms256m -Djava.library.path="/home/komrad/.config/gdlauncher_next/instances/GT New Horizons/natives" -Dminecraft.applet.TargetDirectory="/home/komrad/.config/gdlauncher_next/instances/GT New Horizons" -Dlog4j.configurationFile=/home/komrad/.config/gdlauncher_next/datastore/assets/objects/66/client-1.7.xml net.minecraft.launchwrapper.Launch --username erp_lsf --version 1.7.10 --gameDir "/home/komrad/.config/gdlauncher_next/instances/GT New Horizons" --assetsDir /home/komrad/.config/gdlauncher_next/datastore/assets --assetIndex 1.7.10 --uuid ee5cd7dbbd70454ab2d1f2941b2fb965 --accessToken eyJhbGciOiJIUzI1NiJ9.eyJhZ2ciOiJBZHVsdCIsInN1YiI6ImZkMzRjNzJhZjE2Y2RmYjdkODA4Y2Q4MWQ4YmFjYTA5IiwieWdndCI6ImE0MzdhZWQ4ZDE4MzQxNDViZDQ0MjIyYmJkNDIyM2I2Iiwic3ByIjoiZWU1Y2Q3ZGJiZDcwNDU0YWIyZDFmMjk0MWIyZmI5NjUiLCJpc3MiOiJZZ2dkcmFzaWwtQXV0aCIsImV4cCI6MTY0MDcwODM3NCwiaWF0IjoxNjQwNTM1NTc0fQ.6Rp1mfjWYgp4mWvc1_CNH-dBcTiC0wWVMH6fQjDMIjU --userProperties {} --userType mojang --tweakClass cpw.mods.fml.common.launcher.FMLTweaker --width 854 --height 480
        ;;
esac
